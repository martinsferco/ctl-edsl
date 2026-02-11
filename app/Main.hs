{-# OPTIONS_GHC -Wno-x-partial #-}

module Main ( main ) where

import PrettyPrinter ( ppSentence, ppValue, ppType )
import TypeCheck     ( typeCheckSentence )
import MonadCTL      ( MonadCTL, CTL, runCTL, printCTL, getMode, getTy, getDefinitions,
                       getLastFile, setLastFile, failCTL, searchDef )
import Parser        ( P, program, sentence, runP )
import Common        ( VarIdent )
import Global        ( Conf (..), Mode (..) )
import Error         ( Error (ParseErr) )
import Eval          ( evalSentence, addDefinition )
import Lang          ( Program, Sentence (..) )

import Control.Monad.Except
import Control.Monad.Trans
import Control.Applicative  ( )
import Control.Exception    ( IOException, catch )
import Control.Monad        ( when, unless )

import Options.Applicative

import Data.List ( isPrefixOf, intersperse, nub )
import Data.Char ( isSpace )

import System.Console.Haskeline
import System.Exit
import System.IO


--------------------------------------------------------------------------------
-- Main defininition
--------------------------------------------------------------------------------

main :: IO ()
main = main'

main' :: IO ()
main' = execParser interpreterOpts >>= go
  where
    interpreterOpts :: ParserInfo (Mode, [FilePath])
    interpreterOpts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "CTL Interpreter"
     <> header "Computational Tree Logic Interpreter - ALP 2026" )

    go :: (Mode,[FilePath]) -> IO ()
    go (programMode, files) = case programMode of 
      Interactive -> runInputT customSettings $ runOrFailInputT (Conf programMode) $ do
        mapM_ handleFile files
        interactiveLoop

      _           -> runOrFailCTL (Conf programMode) $ mapM_ handleFile files


-- Loads and handles a program from a file.
handleFile ::  MonadCTL m => FilePath -> m()
handleFile file = do
  setLastFile file
  loadProgram file >>= handleProgram

-- Reads and parses the content of a file.
loadProgram ::  MonadCTL m => FilePath -> m Program
loadProgram file = do
    let filename = reverse (dropWhile isSpace (reverse file))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStrLn stderr ("Could not open the file " ++ filename ++ ": " ++ err)
                         return "")
    parseCTL filename program x

-- Checks that a program has not repeated definitions, and then handles each sentence of it.
handleProgram :: MonadCTL m => Program -> m()
handleProgram p = notRepeatedDefinitions p >> mapM_ handleSentence p


notRepeatedDefinitions :: MonadCTL m => Program -> m ()
notRepeatedDefinitions p = do 
  let defs = filter isDefinition p
  varNames <- mapM getDefinitionName defs
  let duplicates = nub [x | x <- varNames, length (filter (== x) varNames) > 1]
  unless (null duplicates) $ 
    failCTL $ "Multiple definitions of the variable " ++ head duplicates

  where
    isDefinition :: Sentence -> Bool
    isDefinition (Def _ _ _ _) = True
    isDefinition _             = False

    getDefinitionName :: MonadCTL m => Sentence -> m VarIdent
    getDefinitionName (Def _ v _ _) = return v
    getDefinitionName _             = failCTL "Internal error: expected definition"

-- Handles a sentence according to the actual mode of the Monad.
handleSentence :: MonadCTL m => Sentence -> m()
handleSentence s = do  
    typeCheckSentence s
    currentMode <- getMode
    case currentMode of
      TypeCheck   -> do
        addDefinition s  
        printCTL $ ppSentence s
      
      Interactive -> do 
        addDefinition s  
      
      Eval        -> evalSentence s


--------------------------------------------------------------------------------
-- Interactive Main
--------------------------------------------------------------------------------

data Command 
  = InteractiveSentence String
  | LoadFile String
  | FindType VarIdent
  | Show VarIdent
  | Browse
  | Reload
  | Quit 
  | Help
  | NoCommand

data InteractiveCommand = Cmd [String] String (String -> Command) String


interactiveCommands :: [InteractiveCommand]
interactiveCommands = 
  [ Cmd [":browse", ":b"] ""       (const Browse) "Shows all the definitions in the global scope."
  , Cmd [":load",   ":l"] "<file>" LoadFile       "Loads a program from a file."
  , Cmd [":reload", ":r"] ""       (const Reload) "Reloads a program from the last file."
  , Cmd [":type",   ":t"] "<def>"  FindType       "Finds the type of a global definition."
  , Cmd [":show",   ":s"] "<def>"  Show           "Prints a global definition."
  , Cmd [":help",   ":h"] ""       (const Help)   "Shows a list of available commands."
  , Cmd [":quit",   ":q"] ""       (const Quit)   "Finishes the interpreter." ]


interactiveLoop :: MonadCTL m => m ()
interactiveLoop = printCTL interactiveTitle >> repl

-- Read-Evaluate-Print Loop.
repl :: MonadCTL m => m ()
repl = do 
  line <- liftIO $ catch
            (runInputT customSettings $ getInputLine interactivePrompt)
            ioExceptionCatcher
  handleInputLine line
  where
    ioExceptionCatcher :: IOException -> IO (Maybe String)
    ioExceptionCatcher _ = return Nothing


handleInputLine :: MonadCTL m => Maybe String -> m ()
handleInputLine line = 
  case line of
    Nothing          -> return ()
    Just ""          -> repl
    Just lineContent -> do
      comm <- interpretCommand lineContent  
      continueLoop <- handleCommand comm
      when continueLoop repl  

-- Interpretes a command checking if it is from the interactive mode, or a general sentence
-- of our language.
interpretCommand :: MonadCTL m => String -> m Command
interpretCommand lineContent = 
  if ":" `isPrefixOf` lineContent
  then interpretInteractiveCommand lineContent
  else return (InteractiveSentence lineContent)

-- Interpreter a command from the interactive mode.
interpretInteractiveCommand :: MonadCTL m => String -> m Command
interpretInteractiveCommand lineContent = do
  let (cmd, t') = break isSpace lineContent
  let t         = dropWhile isSpace t'
  let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) interactiveCommands
  
  case matching of 
    [] -> do
      printCTL $ "Unknown command `" ++ cmd ++ "`. Write :h to see all the commands."
      return NoCommand
    
    [Cmd _ _ f _] -> return $ f t
    
    multiple -> do
      let cmdNames = [ head cs | Cmd cs _ _ _ <- multiple ]
      printCTL $ "Ambiguous command, it could be: " ++ concat (intersperse ", " cmdNames)
      return NoCommand

-- Handles a command and it indicates if the REPL must continue or not.
handleCommand :: MonadCTL m => Command -> m Bool
handleCommand Quit      = return False
handleCommand NoCommand = return True
handleCommand Help      = liftIO (putStr $ helpCommandsText interactiveCommands) >> return True

handleCommand Browse = 
  do  loadedDefinitions <- getDefinitions
      printCTL (unlines $ map (\(v,ty) -> v ++ " :: " ++ ppType ty) loadedDefinitions)
      return True

handleCommand (InteractiveSentence sentenceLine) =
  do  catchError (parseCTL "<interactive>" sentence sentenceLine >>= 
                                          (\s -> typeCheckSentence s >> evalSentence s))
                 (printCTL . show)
      return True
  
handleCommand (LoadFile nameFile) = 
  do  catchError (printCTL ("Loading " ++ nameFile ++ "...") >> handleFile nameFile)
                 (printCTL . show)
      return True

handleCommand (FindType var) = 
  do  catchError (getTy var >>= printCTL . ppType)
                 (printCTL . show)
      return True

handleCommand (Show var) = 
  do catchError (searchDef var >>= printCTL . ppValue)
                (printCTL . show)
     return True

handleCommand Reload = 
  do  currentFile <- getLastFile
      case currentFile of
        Nothing   -> printCTL "There are not previous files loaded." >> return True
        Just file -> handleCommand (LoadFile file)


interactiveTitle, interactivePrompt :: String 
interactiveTitle = "Computational Tree Logic Interpreter.\n" ++
                   "Write :h to get a list of available commands."
interactivePrompt = "CTL> "


helpCommandsText :: [InteractiveCommand] -> String
helpCommandsText cs = unlines $
  [ "List of commands:"
  , ""
  , "<sentence>              evaluates a sentence"
  ] ++ map formatCommand cs
  where
    formatCommand (Cmd aliases arg _ desc) = 
      let commands = [ cmd ++ if null arg then "" else " " ++ arg 
                     | cmd <- aliases ]
          cmdText = concat (intersperse ", " commands)
          padding = replicate ((24 - length cmdText) `max` 2) ' '
      in cmdText ++ padding ++ desc

-- Customs settins: it defines the file where all the history of the interactive mode is saved.
customSettings :: Settings IO
customSettings = defaultSettings { historyFile = Just ".ctl_history" }


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

runOrFailCTL :: Conf -> CTL a -> IO a
runOrFailCTL conf m = do
  result <- runCTL m conf
  case result of
    Left err -> do
      hPrint stderr err
      exitWith (ExitFailure 1)
    Right v -> return v

runOrFailInputT :: Conf -> CTL a -> InputT IO a
runOrFailInputT conf m = do
  result <- liftIO $ runCTL m conf
  case result of
    Left err -> do
      liftIO $ hPrint stderr err
      liftIO $ exitWith (ExitFailure 1)
    Right v -> return v

parseCTL ::  MonadCTL m => String -> P a -> String -> m a
parseCTL filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r

-- Parse the configuration mode of the interpreter. Eval is the default.
parseConf :: Parser Mode 
parseConf = flag' TypeCheck   (long "typecheck"   <> short 't' <> help "Chequea los tipos del programa.") <|>
            flag' Interactive (long "interactive" <> short 'i' <> help "Abre el intérprete del lenguaje.") <|>
            flag  Eval Eval   (long "eval"        <> short 'e' <> help "Evalúa el programa.")

parseArgs :: Parser (Mode, [FilePath])
parseArgs = pure (,) <*> parseConf <*> many (argument str (metavar "FILES..."))