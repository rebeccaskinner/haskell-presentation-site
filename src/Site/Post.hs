{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric #-}
module Site.Post where
import GHC.Generics
import Data.Text (Text)
import Data.Aeson
import Data.Monoid
import Control.Monad
import Text.Printf
import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Convertible

type PostPK = Int

data Post = Post { postID :: PostPK
                 , postTitle :: Text
                 , postContent :: Text
                 , postUpvotes :: Int
                 , postDownvotes:: Int
                 } deriving (Generic, Eq, Show)

instance ToJSON Post where
  toJSON (Post id title content upvotes downvotes) =
    object ["id" .= id
           , "title" .= title
           , "content" .= content
           , "upvotes" .= upvotes
           , "downvotes" .= downvotes
           ]


instance FromJSON Post where
  parseJSON (Object v) = Post <$>
                         v .: "id" <*>
                         v .: "title" <*>
                         v .: "content" <*>
                         v .: "upvotes" <*>
                         v .: "downvotes"
  parseJSON _ = mzero

upvotePost :: Post -> Post
upvotePost p = p { postUpvotes = (succ . postUpvotes) p }

downvotePost :: Post -> Post
downvotePost p = p { postDownvotes = (succ . postDownvotes) p }

postScore :: Post -> Int
postScore p = postUpvotes p - postDownvotes p

postInsertStr :: String
postInsertStr = "insert into post (rowid, title, content, upvote, downvote) values (?, ?, ?, ?, ?)"

postQueryStr :: String
postQueryStr = "select rowid, * from post"

parsePostResults :: [[SqlValue]] -> [Post]
parsePostResults = map parsePost
  where
    parsePost :: [SqlValue] -> Post
    parsePost (rowid : title : content : upvote : downvote : []) =
      Post (fromSql rowid)
           (fromSql title)
           (fromSql content)
           (fromSql upvote)
           (fromSql downvote)

getPosts :: IConnection c => c -> IO [Post]
getPosts conn = parsePostResults <$> quickQuery' conn postQueryStr []
