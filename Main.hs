{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty.Cookie
import Web.Scotty.Trans
import qualified Data.Text.Lazy as T
import Data.List
import Data.Time
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Ord
import Data.IORef
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State as S
import qualified Text.MMark as M
import qualified Text.MMark.Extension.Common as M
import Text.Casing
import Text.Read (readMaybe)
import Lucid
import Lucid.Base
import System.Directory
import System.FilePath
import System.Random
import System.IO.Error
import Network.HTTP.Types

type Sessions = Map.Map T.Text T.Text
type ActionM = ActionT T.Text (S.StateT Sessions IO)

-- | Makes a function that carries over the state from one
-- request to the next
mkPreserve :: s -> IO ((S.StateT s IO) a -> IO a)
mkPreserve initS = do
  ref <- newIORef initS
  return $ \f -> do
    s <- readIORef ref
    (x, s') <- S.runStateT f s
    writeIORef ref s'
    return x

-- TODO: Use hashes
logins :: Map.Map T.Text T.Text
logins = Map.fromList
           [ ("luke", "pass")
           , ("dave", "abcd")
           , ("antonia", "1234")
           , ("owen", "haskell")
           ]
-- | Checks if the user is logged in
loggedIn :: ActionM Bool
loggedIn = do
  mCookie <- getCookie "session"
  case mCookie of
    Nothing -> return False
    Just cookie -> Map.member (T.fromStrict cookie) <$> lift S.get

-- | Returns the current username
currentUser :: ActionM (Maybe T.Text)
currentUser = do
  mCookie <- getCookie "session"
  case mCookie of
    Nothing -> return Nothing
    Just cookie -> Map.lookup (T.fromStrict cookie) <$> lift S.get

main :: IO ()
main = do
  preserve <- mkPreserve mempty
  createDirectory "posts" `catch` (\e -> unless (isAlreadyExistsError e) (throw e))
  scottyT 3000 preserve $ do
    get "/style.css" $ file "style.css"

    get "/create" $ do
      authed <- loggedIn
      if authed
        then template "Create" $ do
          h1_ "Create a new post"
          form_ [action_ "create", method_ "post"] $ do
            input_ [name_ "title", type_ "text", placeholder_ "Title"]
            br_ []
            textarea_ [name_ "body", placeholder_ "# Your markdown here"] ""
            br_ []
            input_ [type_ "submit", value_ "Post"]
        else do
          text "You're not logged in"
          status unauthorized401

    post "/create" $ do
      t <- param "title"
      b <- param "body"
      p <- createPost t b
      redirect (linkToPost p)

    post "/login" $ do
      username <- param "username"
      password <- param "password"
      if Map.lookup username logins /= Just password
        then do
          text "Invalid username and/or password"
          status unauthorized401
        else do
          session <- T.pack . show <$> liftIO (randomIO :: IO Int)
          lift $ S.modify (Map.insert session username)
          setSimpleCookie "session" (T.toStrict session)
          redirect "/"

    post "/logout" $ deleteCookie "session" >> redirect "/"

    get "/" $ do
      posts <- take 5 <$> liftIO loadAll
      template "Cool blog" $ sequence (mapMaybe render posts)

    get "/posts/:post" $ do
      name <- param "post"
      mPost <- liftIO (load name)
      case mPost of
        Nothing -> status notFound404
        Just p -> case render p of
          Nothing -> status internalServerError500
          Just content -> template (postTitle p) content

    get "/author/:author" $ do
      author <- param "author"
      posts <- filter ((== author) . postAuthor) <$> liftIO loadAll
      let title = "Posts by " <> author
      template title $ do
        h1_ (toHtml title)
        sequence (mapMaybe render posts)

template :: T.Text -> Html a -> ActionM ()
template title content = do
  authed <- loggedIn
  html $ renderText $ html_ $ do
    head_ $ style >> title_ (toHtml title)
    body_ $ do
      term "nav" $ do
        a_ [href_ "/"] "🏠 Cool Blog"
        if authed
          then do
            "   " >> a_ [href_ "/create"] "Create a Post"
            form_ [action_ "/logout", method_ "post", class_ "login-form"] $
              input_ [type_ "submit", value_ "Log Out"]
          else form_ [action_ "/login", method_ "post", class_ "login-form"] $ do
            input_ [name_ "username", placeholder_ "Username", type_ "text"]
            input_ [name_ "password", placeholder_ "Password", type_ "password"]
            input_ [type_ "submit", value_ "Log In"]
      content
  where style = link_ [Attribute "rel" "stylesheet", Attribute "href" "/style.css"]

data Post = Post
          { postBody     :: T.Text
          , postTitle    :: T.Text
          , postDate     :: UTCTime
          , postAuthor   :: T.Text
          } deriving (Read, Show)

loadAll :: IO [Post]
loadAll = do
  fps <- listDirectory "posts"
  posts <- catMaybes <$> mapM load fps
  return $ sortOn (Down . postDate) posts

load :: String -> IO (Maybe Post)
load name = do
  let fp = "posts" </> name
  guard =<< doesFileExist fp
  readMaybe <$> readFile fp

render :: Post -> Maybe (Html ())
render p@(Post markdown title date author) =
  case M.parse (pathToPost p) (T.toStrict markdown) of
    Left _ -> Nothing
    Right doc -> Just $
      article_ [id_ (T.toStrict title)] $ do
        header_ $ do
          a_ [href_ (T.toStrict $ linkToPost p)] $
            h1_ $ toHtml title
          a_ [href_ (T.toStrict $ "/author/" <> postAuthor p)] $
            address_ ("By " >> toHtml author)
          time_ [datetime_ (T.toStrict $ T.pack (show date))] $ toHtml dateStr
        M.render (M.useExtensions extensions doc)
  where extensions = [M.ghcSyntaxHighlighter, M.skylighting, M.footnotes]
        dateStr = T.pack $ formatTime defaultTimeLocale "%a %e %B %Y" date

linkToPost :: Post -> T.Text
linkToPost p = T.pack $ "/posts" </> kebab (T.unpack (postTitle p))

pathToPost :: Post -> FilePath
pathToPost p = "posts" </> kebab (T.unpack (postTitle p))

-- | Creates and saves a new post
createPost :: T.Text
           -- ^ The title of the post
           -> T.Text
           -- ^ The markdown body
           -> ActionM Post
           -- ^ The freshly created post
createPost title markdown = do
  auth <- do
    ma <- currentUser
    case ma of
      Just a -> return a
      Nothing -> raise "Not logged in"
  curTime <- liftIO getCurrentTime
  let p = Post markdown title curTime auth
  liftIO $ writeFile (pathToPost p) (show p)
  return p
