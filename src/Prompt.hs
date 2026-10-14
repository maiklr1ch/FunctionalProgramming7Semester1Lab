{-# LANGUAGE ScopedTypeVariables #-}

-- | Консольне введення: клас 'Promptable' та функції запитів до користувача.
module Prompt
  ( Promptable (..)
  , askLine, ask, askRef, confirm
  ) where

import Control.Monad (when)
import Data.Proxy (Proxy (..))
import Database.MySQL.Simple (Connection)
import System.Exit (exitSuccess)
import System.IO

import Classes
import Database

-- | Тип, запис якого можна ввести або виправити в діалозі.
--   @Nothing@ — створення нового запису, @Just old@ — редагування:
--   порожній ввід залишає старе значення.
class Promptable a where
  promptRecord :: Connection -> Maybe a -> IO a

-- | Рядок із клавіатури. Кінець вводу (Ctrl+D) завершує програму.
askLine :: String -> IO String
askLine p = do
  putStr p
  hFlush stdout
  eof <- isEOF
  if eof then putStrLn "" >> exitSuccess else getLine

-- | Запитує одне поле. Якщо задано значення за замовчуванням,
--   порожній ввід його залишає. Некоректний ввід запитується повторно.
ask :: forall a. Input a => String -> Maybe a -> IO a
ask label def = loop
  where
    hint = inputHint (Proxy :: Proxy a)
    prompt = "  " ++ label
             ++ (if null hint then "" else " (" ++ hint ++ ")")
             ++ maybe "" (\d -> " [" ++ showInput d ++ "]") def
             ++ ": "
    loop = do
      s <- trim <$> askLine prompt
      case (s, def) of
        ("", Just d) -> return d
        _ -> case parseInput s of
          Right v  -> return v
          Left err -> putStrLn ("  ! " ++ err) >> loop

-- | Запитує @id@ запису іншої таблиці, попередньо показавши доступні варіанти.
askRef :: forall a. Persistent a => Connection -> Proxy a -> String -> Maybe Int -> IO Int
askRef conn p label def = do
  items <- listing conn p
  when (null items) $
    ioError (userError ("у таблиці «" ++ entityTitle p ++ "» немає записів — спершу додайте їх"))
  showItems p items
  loop (map fst items)
  where
    loop ids = do
      i <- ask label def
      if i `elem` ids
        then return i
        else putStrLn "  ! Немає запису з таким ID, спробуйте ще раз." >> loop ids

showItems :: Persistent a => Proxy a -> [(Int, String)] -> IO ()
showItems p items = do
  putStrLn ("  Доступно (" ++ entityTitle p ++ "):")
  mapM_ (\(i, s) -> putStrLn ("    " ++ show i ++ ") " ++ s)) items

-- | Питання «так/ні».
confirm :: String -> IO Bool
confirm q = ask (q ++ "?") (Just False)
