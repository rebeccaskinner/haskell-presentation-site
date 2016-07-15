module Site.Post where
import Data.Text
import Text.JSON
import Text.Printf
import Database.HDBC
import Database.HDBC.Sqlite3

newtype PostPK = PostPK Int

data Post = Post { postPrimaryKey :: PostPK
                   postTitle :: Text
                 , postContent :: Text
                 , postUpvotes :: Int
                 , postDownvotes:: Int
                 } deriving (Eq, Show)

instance JSON Post where
  showJSON (Post title contents upvotes downvotes) =
    (showJSON . toJSObject) $ [ ("title", showJSON title)
                              , ("content", showJSON contents)
                              , ("upvotes", showJSON upvotes)
                              , ("downvotes", showJSON downvotes)
                              ]

  readJSON (JSObject obj) =
    Post <$> getField "title"
         <*> getField "content"
         <*> getField "upvotes"
         <*> getField "downvotes"
    where
      getField :: (JSON a) => String -> Result a
      getField = (`valFromObj` obj)

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
