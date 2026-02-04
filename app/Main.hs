module Main where

import MonadCTL ( MonadCTL, CTL, runCTL, printCTL, failCTL, getMode )
import Parser ( P, program, runP )
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

import Data.Char ( isSpace )

import Options.Applicative

import System.Exit
import System.IO


main :: IO ()
-- main = runInputT defaultSettings main'
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
    go (mode, files) = runOrFail (Conf mode) $ mapM_ handleFile files
    -- despues de hacer el mapM_ handleFile files, tengo que hacer un if interactive, ejecutar el loop, si no, return ()
    -- el loop lo voy a copiar del otro tp, y lo voy a hacer sencillo. Tengo el parser de sentences, y si no, tengo el parser
    -- de opciones. Load, reload, type de algo, etc.

runOrFail :: Conf -> CTL a -> IO a
runOrFail conf m = do
  result <- runCTL m conf
  case result of
    Left err -> do
      liftIO $ hPrint stderr err
      exitWith (ExitFailure 1)
    Right v -> return v

parseConf :: Parser Mode 
parseConf = flag' TypeCheck   (long "typecheck"   <> short 't' <> help "Chequea los tipos del programa.") <|>
            flag' Interactive (long "interactive" <> short 'i' <> help "Abre el intérprete del lenguaje.") <|>
            flag  Eval Eval   (long "eval"        <> short 'e' <> help "Evalúa el programa.")

parseArgs :: Parser (Mode, [FilePath])
parseArgs = pure (,) <*> parseConf <*> many (argument str (metavar "FILES..."))


handleFile ::  MonadCTL m => FilePath -> m()
handleFile file = loadProgram file >>= handleProgram 

handleProgram :: MonadCTL m => Program -> m()
handleProgram []              = return ()
handleProgram (sentence : xs) = do handleSentence sentence
                                   handleProgram xs

loadProgram ::  MonadCTL m => FilePath -> m Program
loadProgram file = do
    let filename = reverse (dropWhile isSpace (reverse file))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
                         return "")
    parseIO filename program x


parseIO ::  MonadCTL m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r


handleSentence :: MonadCTL m => Sentence -> m()
handleSentence sentence = do  typeCheckSentence sentence
                              mode <- getMode 
                              case mode of
                                Interactive -> return ()
                                TypeCheck   -> printCTL $ ppSentence sentence
                                Eval        -> evalSentence sentence
