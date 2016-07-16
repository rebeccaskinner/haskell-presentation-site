{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.Static
import System.Environment (getArgs)
import System.IO
import Data.Maybe
import Control.Monad.IO.Class
import Database.HDBC
import Database.HDBC.Sqlite3
import Site.Post

getDBArg :: IO String
getDBArg = do
  found <- maybeHead <$> getArgs
  defaultPath <- getDefaultDBPath
  if isNothing found
    then do
      hPutStr stderr ("Using default database path at: " ++ defaultPath)
      return defaultPath
    else
      return $ fromJust found
  where
    maybeHead [] = Nothing
    maybeHead (x:_) = Just x
    getDefaultDBPath :: IO String
    getDefaultDBPath = do
      return "website"

sqliteConnection = getDBArg >>= connectSqlite3

main :: IO ()
main =
  scotty 3000 $ do

  middleware $ staticPolicy (noDots >-> addBase "static")

  get "/" $ file "./static/index.html"
  get "/posts" $ do
    posts <- liftIO $ (sqliteConnection >>= getPosts)
    json posts

  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
