{-# LANGUAGE OverloadedStrings, BangPatterns #-}

import Web.Scotty
import Network.Wai.Middleware.Static
import System.Environment (getArgs)
import System.IO
import System.IO.Unsafe
import Data.Maybe
import Data.Char
import Control.Monad.IO.Class
import Database.HDBC.Sqlite3
import Site.Post
import Network.HTTP.Types
import Database.HDBC
import qualified Data.Text.Lazy as T
import Debug.Trace

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

sqliteConn = unsafePerformIO $ getDBArg >>= connectSqlite3

includePostContent :: String -> Bool
includePostContent s = if "true" == (map toLower s) then True else False



main :: IO ()
main =
  scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "static")

  get "/" $ file "./static/index.html"
  get "/submit" $ file "./static/submit.html"

  get "/posts" $ do
    includeContents <- (param "include-contents") `rescue` (\_ -> return "false")
    posts <- liftIO $ getPosts sqliteConn
    if includePostContent includeContents then
      json posts
      else
      json $ map postID posts

  get "/posts/:post" $ do
    post <- param "post"
    p <- liftIO $ postByID sqliteConn post
    case p of
      Just aPost -> json p
      Nothing -> file "./static/error.html" >>
                 addHeader "Content-Type" "text/html" >>
                 status status404

  get "/posts/:post/score" $ do
    post <- param "post"
    p <- liftIO $ postByID sqliteConn post
    case p of
      Just aPost -> json $ postScore aPost
      Nothing -> file "./static/error.html" >>
                 addHeader "Content-Type" "text/html" >>
                 status status404

  post "/posts/:post" $ do
    postID <- param "post"
    act <- param "action" `rescue` (\m -> status status500 >> text "invalid param" >> return "")
    p <- liftIO $ case map toLower act of
      "upvote" -> upvoteByID sqliteConn postID
      "downvote" -> downvoteByID sqliteConn postID
      x ->  trace ("unexpected action: " ++ x) (return Nothing)
    case p of
      Just p' -> json p'
      Nothing -> do
        file "./static/error.html"
        addHeader "Content-Type" "text/html"
        status status500

  post "/createpost" $ do
    title <- (param "title") `rescue` handleBadFormData
    content <- (param "contents") `rescue` handleBadFormData
    _ <- liftIO $ mkPost sqliteConn title content Nothing Nothing
    redirect "/posts?include-contents=true"

    where
      handleBadFormData _ = status status500 >> return ""
