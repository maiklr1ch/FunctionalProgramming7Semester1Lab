module Main (main) where

import Control.Exception (finally)
import Database.MySQL.Simple (close)
import System.Exit (exitFailure)
import System.IO

import Database (connectFromEnv, safely)
import Menu (mainMenu)

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  hSetBuffering stdout LineBuffering
  conn <- safely connectFromEnv
  case conn of
    Left err -> do
      hPutStrLn stderr ("Не вдалося підключитися до MySQL: " ++ err)
      hPutStrLn stderr "Перевірте змінні DB_HOST, DB_PORT, DB_USER, DB_PASSWORD, DB_NAME і що schema.sql завантажено."
      exitFailure
    Right c -> mainMenu c `finally` close c
