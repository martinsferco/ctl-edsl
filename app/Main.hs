module Main where

import MonadCTL ( MonadCTL, CTL, runCTL, printCTL, getMode, getTy, getDefinitions, getLastFile, setLastFile )
import Parser ( P, program, sentence, runP )
import PrettyPrinter ( ppSentence )
import TypeCheck
import Global
import Error
import Eval
import Lang

import Control.Exception ( IOException, catch )
import Control.Applicative ((<|>), many)
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad (when)

import Data.Char ( isSpace )
import Data.List ( isPrefixOf, intersperse )

import Options.Applicative

import System.Console.Haskeline
import System.Exit
import System.IO

main :: IO ()
main = main'

main' :: IO ()
main' = execParser interpreterOpts >>= go
  where
    interpreterOpts :: ParserInfo (Mode, [FilePath])
    interpreterOpts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Intérprete de CTL"
     <> header "Intérprete de Computational Tree Logic - ALP 2026" )

    go :: (Mode,[FilePath]) -> IO ()
    go (mode, files) = case mode of 
      Interactive -> runInputT customSettings $ runOrFailInputT (Conf mode) $ do
        mapM_ handleFile files
        interactiveLoop

      _           -> runOrFailCTL (Conf mode) $ mapM_ handleFile files

customSettings :: Settings IO
customSettings = defaultSettings { historyFile = Just ".ctli_history" }


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


parseConf :: Parser Mode 
parseConf = flag' TypeCheck   (long "typecheck"   <> short 't' <> help "Chequea los tipos del programa.") <|>
            flag' Interactive (long "interactive" <> short 'i' <> help "Abre el intérprete del lenguaje.") <|>
            flag  Eval Eval   (long "eval"        <> short 'e' <> help "Evalúa el programa.")

parseArgs :: Parser (Mode, [FilePath])
parseArgs = pure (,) <*> parseConf <*> many (argument str (metavar "FILES..."))


handleFile ::  MonadCTL m => FilePath -> m()
handleFile file = do
  setLastFile file
  loadProgram file >>= handleProgram

handleProgram :: MonadCTL m => Program -> m()
handleProgram = mapM_ handleSentence

loadProgram ::  MonadCTL m => FilePath -> m Program
loadProgram file = do
    let filename = reverse (dropWhile isSpace (reverse file))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
                         return "")
    parseCTL filename program x


parseCTL ::  MonadCTL m => String -> P a -> String -> m a
parseCTL filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r


handleSentence :: MonadCTL m => Sentence -> m()
handleSentence sentence = do  
    typeCheckSentence sentence
    mode <- getMode
    case mode of
      TypeCheck   -> do
        addDefinition sentence  
        printCTL $ ppSentence sentence
      
      Interactive -> do 
        addDefinition sentence  
      
      Eval        -> evalSentence sentence


interactiveLoop :: MonadCTL m => m ()
interactiveLoop = printCTL interactiveTitle >> repl


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
      command <- interpretCommand lineContent  
      continueLoop <- handleCommand command
      when continueLoop repl  


interpretCommand :: MonadCTL m => String -> m Command
interpretCommand lineContent = 
  if ":" `isPrefixOf` lineContent
  then interpretInteractiveCommand lineContent
  else return (InteractiveSentence lineContent)


interpretInteractiveCommand :: MonadCTL m => String -> m Command
interpretInteractiveCommand lineContent = do
  let (cmd, t') = break isSpace lineContent
  let t         = dropWhile isSpace t'
  let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) interactiveCommands
  
  case matching of 
    [] -> do
      printCTL $ "Comando desconocido `" ++ cmd ++ "`. Escribe :h para ver los comandos."
      return NoCommand
    
    [Cmd _ _ f _] -> return $ f t
    
    multiple -> do
      let cmdNames = [ head cs | Cmd cs _ _ _ <- multiple ]
      printCTL $ "Comando ambiguo, podría ser: " ++ concat (intersperse ", " cmdNames)
      return NoCommand

handleCommand :: MonadCTL m => Command -> m Bool
handleCommand Quit      = return False
handleCommand NoCommand = return True
handleCommand Help      = liftIO (putStr $ helpCommandsText interactiveCommands) >> return True

handleCommand Browse = 
  do  definitions <- getDefinitions
      printCTL (unlines $ map (\(v,ty) -> v ++ " :: " ++ show ty) definitions)
      return True

handleCommand (InteractiveSentence sentenceLine) =
  do  catchError (parseCTL "<interactive>" sentence sentenceLine >>= 
                                          (\s -> typeCheckSentence s >> evalSentence s))
                 (printCTL . show)
      return True
  
handleCommand (LoadFile nameFile) = 
  do  catchError (printCTL ("Cargando " ++ nameFile ++ "...") >> handleFile nameFile)
                 (printCTL . show)
      return True

handleCommand (FindType var) = 
  do  catchError (getTy var >>= printCTL . show)
                 (printCTL . show)
      return True

handleCommand Reload = 
  do  lastFile <- getLastFile
      case lastFile of
        Nothing   -> printCTL "No existen archivos cargados previamente." >> return True
        Just file -> handleCommand (LoadFile file)


data Command = InteractiveSentence String
             | LoadFile String
             | FindType String
             | Browse
             | Reload
             | Quit 
             | Help
             | NoCommand


data InteractiveCommand = Cmd [String] String (String -> Command) String

interactiveCommands :: [InteractiveCommand]
interactiveCommands = 
  [ Cmd [":browse", ":b"] ""       (const Browse) "Ver las definiciones en el scope global."
  , Cmd [":load",   ":l"] "<file>" LoadFile       "Cargar un programa desde un archivo."
  , Cmd [":reload", ":r"] ""       (const Reload) "Volver a cargar el último archivo."
  , Cmd [":type",   ":t"] "<def>"  FindType       "Encontrar el tipo de una definicion global."
  , Cmd [":help",   ":h"] ""       (const Help)   "Mostar listado de comandos disponibles."
  , Cmd [":quit",   ":q"] ""       (const Quit)   "Salir del intérprete."
  ]


helpCommandsText :: [InteractiveCommand] -> String
helpCommandsText cs = unlines $
  [ "Lista de comandos: Cualquier comando puede ser abreviado."
  , ""
  , "<sentence>              evaluar una sentencia"
  ] ++ map formatCommand cs
  where
    formatCommand (Cmd aliases arg _ desc) = 
      let commands = [ cmd ++ if null arg then "" else " " ++ arg 
                     | cmd <- aliases ]
          cmdText = concat (intersperse ", " commands)
          padding = replicate ((24 - length cmdText) `max` 2) ' '
      in cmdText ++ padding ++ desc


interactiveTitle, interactivePrompt :: String 
interactiveTitle = "Intérprete de CTLI (Computational Tree Logic Interpreter).\n" ++
                   "Escriba :h para recibir ayuda."
interactivePrompt = "CTLI> "