{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty.Cookie
import Web.Scotty.Trans
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.List
import Data.Time
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Ord
import Data.IORef
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
import Network.HTTP.Types

type Sessions = Map.Map T.Text T.Text
type ActionM = ActionT T.Text (S.StateT Sessions IO)

loggedIn :: ActionM Bool
loggedIn = do
  mCookie <- getCookie "session"
  case mCookie of
    Nothing -> return False
    Just cookie -> Map.member (T.fromStrict cookie) <$> lift S.get

currentUser :: ActionM (Maybe T.Text)
currentUser = do
  mCookie <- getCookie "session"
  case mCookie of
    Nothing -> return Nothing
    Just cookie -> Map.lookup (T.fromStrict cookie) <$> lift S.get

-- | Makes a function that carries over the state from one
-- request to the next
mkPreserve :: s -> IO ((S.StateT s IO) a -> IO a)
mkPreserve s = do
  ref <- newIORef s
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

main :: IO ()
main = do
  preserve <- mkPreserve mempty
  scottyT 3000 preserve $ do
    get "/style.css" $ file "style.css"

    get "/create" $ do
      authed <- loggedIn
      if authed
        then template "Create" $ do
          h1_ "Create a new post"
          with form_ [action_ "create", method_ "post"] $ do
            input_ [name_ "title", type_ "text", placeholder_ "Title"]
            br_ []
            with textarea_ [name_ "body", placeholder_ "# Your markdown here"] ""
            br_ []
            input_ [type_ "submit"]
        else status unauthorized401

    post "/create" $ do
      t <- param "title"
      b <- param "body"
      p <- createPost b t
      redirect (linkToPost p)

    post "/login" $ do
      username <- param "username"
      password <- param "password"
      if Map.lookup username logins /= Just password
        then status unauthorized401
        else do
          session <- T.pack . show <$> liftIO (randomIO :: IO Int)
          lift $ S.modify (Map.insert session username)
          setSimpleCookie "session" (T.toStrict session)
          redirect "/"

    post "/logout" $ deleteCookie "session" >> redirect "/"

    get "/" $ do
      posts <- liftIO $ do
        files <- listDirectory "posts"
        posts <- catMaybes <$> mapM load files
        return $ take 5 $ sortOn (Down . date) posts
      template "Cool blog" $ sequence (mapMaybe render posts)

    get "/posts/:post" $ do
      name <- param "post"
      mPost <- liftIO (load name)
      case mPost of
        Nothing -> status notFound404
        Just post ->
          case render post of
            Nothing -> status internalServerError500
            Just content -> template (title post) content

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
            a_ [href_ "/create"] "Create a Post"
            with form_ [action_ "/logout", method_ "post", class_ "login-form"] $
              input_ [type_ "submit", value_ "Log Out"]
          else with form_ [action_ "/login", method_ "post", class_ "login-form"] $ do
            input_ [name_ "username", placeholder_ "Username", type_ "text"]
            input_ [name_ "password", placeholder_ "Password", type_ "password"]
            input_ [type_ "submit"]
      content
  where style = link_ [Attribute "rel" "stylesheet", Attribute "href" "/style.css"]

data Post = Post
          { markdown :: T.Text
          , title    :: T.Text
          , date     :: UTCTime
          , author   :: T.Text
          } deriving (Read, Show)

load :: String -> IO (Maybe Post)
load name = do
  let file = "posts" </> name
  guard =<< doesFileExist file
  readMaybe <$> readFile file

render :: Post -> Maybe (Html ())
render p@(Post markdown title date author) =
  case M.parse (pathToPost p) (T.toStrict markdown) of
    Left e -> Nothing
    Right doc -> Just $
      with article_ [id_ (T.toStrict title)] $ do
        header_ $ do
          with a_ [href_ (T.toStrict $ linkToPost p)] $
            h1_ $ toHtml title
          address_ $ "By " >> toHtml author
          with time_ [datetime_ (T.toStrict $ T.pack (show date))] $ toHtml dateStr
        M.render (M.useExtensions extensions doc)
  where extensions = [M.ghcSyntaxHighlighter, M.skylighting, M.footnotes]
        dateStr = T.pack $ formatTime defaultTimeLocale "%a %e %B %Y" date

linkToPost :: Post -> T.Text
linkToPost p = T.pack $ "/posts" </> kebab (T.unpack (title p))

pathToPost :: Post -> FilePath
pathToPost p = "posts" </> kebab (T.unpack (title p))

createPost :: T.Text -> T.Text -> ActionM Post
createPost md title = do
  author <- do
    ma <- currentUser
    case ma of
      Just a -> return a
      Nothing -> raise "Not logged in"
  curTime <- liftIO getCurrentTime
  let p = Post md title curTime author
  liftIO $ writeFile (pathToPost p) (show p)
  return p
