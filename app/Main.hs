module Main (main) where

import Data.List.Split (splitOn)
import Data.Text (unpack)
import Database.HDBC.PostgreSQL (Connection)
import Menu
import NetflixSQL
import Queries (netflixQueries)
import Relude
import Relude.Unsafe (read)
import System.IO (hSetEcho)

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  (db, user, password) <- initial
  con <- localConnection db user password
  userInteraction con

userInteraction :: Connection -> IO ()
userInteraction c = do
  mainMenu
  op <- unpack <$> getLine
  let query = searchQuery (read @Int op) netflixQueries
  case query of
    Nothing -> showErrorMessage
    Just (s, i) -> getArguments i >>= runQuery c s >> userInteraction c

showErrorMessage :: IO ()
showErrorMessage = do
  putTextLn "Incorrect option, please try again. (Press enter to continue)"
  hSetEcho stdin False
  _ <- getLine
  hSetEcho stdin True
  pure ()

getArguments :: Int -> IO [String]
getArguments 0 = pure []
getArguments 2 = do
  putTextLn "Por favor, entre com 1 argumento para a busca (separados por virgula)."
  inp <- getLine
  let args = splitOn "," (unpack inp)
  if length args == 1 then pure (duplicate args) else getArguments 2
getArguments i = do
  putTextLn $ "Por favor, entre com " <> show @Text i <> " argumentos para a busca (separados por virgula)."
  inp <- getLine
  let args = splitOn "," (unpack inp)
  if length args == i then pure args else getArguments i

duplicate :: [a] -> [a]
duplicate x = x ++ x
