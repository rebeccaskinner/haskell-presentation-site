{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.Static
import System.Environment (getArgs)
import System.IO
import Data.Maybe
import Data.Char
import Control.Monad.IO.Class
import Database.HDBC.Sqlite3
import Site.Post
import Network.HTTP.Types

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

includePostContent :: String -> Bool
includePostContent s = if "true" == (map toLower s) then True else False

main :: IO ()
main =
  scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "static")

  get "/" $ file "./static/index.html"

  get "/posts" $ do
    includeContents <- (param "include-contents") `rescue` (\_ -> return "false")
    posts <- liftIO $ sqliteConnection >>= getPosts
    if includePostContent includeContents then
      json posts
      else
      json $ map postID posts

  get "/posts/:post" $ do
    post <- param "post"
    p <- liftIO $ sqliteConnection >>= flip postByID post
    case p of
      Just aPost -> json p
      Nothing -> file "./static/error.html" >>
                 addHeader "Content-Type" "text/html" >>
                 status status404

  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
