{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric #-}
module Site.Post where
import GHC.Generics
import Data.Text (Text)
import Data.Aeson
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad
import Database.HDBC
import qualified Debug.Trace as Debug

type PostPK = Int

data Post = Post { postID :: PostPK
                 , postTitle :: Text
                 , postContent :: Text
                 , postUpvotes :: Int
                 , postDownvotes:: Int
                 } deriving (Generic, Eq, Show)

instance ToJSON Post where
  toJSON (Post postid title content upvotes downvotes) =
    object [ "id" .= postid
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

upvotePost' :: Post -> Post
upvotePost' p = p { postUpvotes = (succ . postUpvotes) p }

downvotePost' :: Post -> Post
downvotePost' p = p { postDownvotes = (succ . postDownvotes) p }

postScore :: Post -> Int
postScore p = postUpvotes p - postDownvotes p

postInsertStr :: String
postInsertStr = "insert into post (title, content, upvotes, downvotes) values (?, ?, ?, ?)"

postQueryStr :: String
postQueryStr = "select rowid, title, content, upvotes, downvotes from post"

postUpdateStr :: String
postUpdateStr = "update post set upvotes = ?, downvotes = ? where rowid = ?"

parsePostResults :: [[SqlValue]] -> [Post]
parsePostResults = (catMaybes $) . (map parsePost)
  where
    parsePost :: [SqlValue] -> Maybe Post
    parsePost (rowid : title : content : upvote : downvote : []) =
      Just $ Post (fromSql rowid)
                  (fromSql title)
                  (fromSql content)
                  (fromSql upvote)
                  (fromSql downvote)
    parsePost vals = Debug.traceShow vals Nothing

createPost :: IConnection c => Text -> Text -> Int -> Int -> c -> IO PostPK
createPost title content up down conn =
  let prepared = [ toSql title
                 , toSql content
                 , toSql up
                 , toSql down
                 ]
  in withTransaction conn $ \c -> do
    _ <- quickQuery' c postInsertStr prepared
    (fromSql . head . head) <$> quickQuery' c "select last_insert_rowid()" []

updatePostInDB :: IConnection c => c -> Post -> IO ()
updatePostInDB conn post =
  let rowid = postID post
      upvotes = postUpvotes post
      downvotes = postDownvotes post
      params = [toSql upvotes, toSql downvotes, toSql rowid]
  in void $ quickQuery' conn postUpdateStr params

upvotePost :: IConnection c => c -> Post -> IO Post
upvotePost conn p = let p' = upvotePost' p in updatePostInDB conn p' >> return p'

downvotePost :: IConnection c => c -> Post -> IO Post
downvotePost conn p = let p' = downvotePost' p in updatePostInDB conn p' >> return p'

getPosts :: IConnection c => c -> IO [Post]
getPosts conn = parsePostResults <$> quickQuery' conn postQueryStr []

postByID :: IConnection c => c -> PostPK -> IO (Maybe Post)
postByID c k = find (\p -> k == postID p) <$> (getPosts c)

mkPost :: IConnection c => c -> Text -> Text -> Maybe Int -> Maybe Int -> IO Post
mkPost conn title content upvotes downvotes =
  let
    upvotes' = fromMaybe 0 upvotes
    downvotes' = fromMaybe 0 downvotes
    postid = withTransaction conn (createPost title content upvotes' downvotes')
  in (\x -> Post x title content upvotes' downvotes') <$> postid
