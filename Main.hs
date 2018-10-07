{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.List
import Data.Time
import Data.Maybe
import Data.Ord
import Control.Monad.IO.Class
import Control.Monad
import qualified Text.MMark as M
import qualified Text.MMark.Extension.Common as M
import Text.Casing
import Text.Read (readMaybe)
import Lucid
import Lucid.Base
import System.Directory
import System.FilePath
import Network.HTTP.Types

main :: IO ()
main = scotty 3000 $ do
  get "/style.css" $ file "style.css"

  get "/create" $ html $ renderText $ html_ $ do
    head_ $ do
      title_ "Create"
      style
    body_ $ do
      h1_ "Create a new post"
      with form_ [action_ "create", method_ "post"] $ do
        input_ [name_ "title", type_ "text", placeholder_ "Title"]
        br_ []
        with textarea_ [name_ "body", placeholder_ "# Your markdown here"] ""
        br_ []
        input_ [type_ "submit"]

  post "/create" $ do
    t <- param "title"
    b <- param "body"
    p <- liftIO $ save b t
    redirect ("posts/" <> title p)

  post "/login" $ do
    username <- param "username"
    password <- param "password"
    if username /= ("luke" :: T.Text) || password /= ("pass" :: T.Text)
      then status unauthorized401
      else redirect "/create"

  get "/" $ do
    posts <- liftIO $ do
      files <- listDirectory "posts"
      posts <- catMaybes <$> mapM load files
      return $ take 5 $ sortOn (Down . date) posts
    html $ renderText $ html_ $ do
      head_ $ style >> title_ "Cool Blog"
      body_ $ do
        p_ $ a_ [href_ "/"] "Cool Blog"
        with form_ [action_ "/login", method_ "post"] $ do
          input_ [name_ "username", placeholder_ "Username", type_ "text"]
          input_ [name_ "password", placeholder_ "Password", type_ "password"]
          input_ [type_ "submit"]
        sequence (mapMaybe render posts)

  get "/posts/:post" $ do
    name <- param "post"
    mPost <- liftIO (load name) 
    case mPost of
      Nothing -> status notFound404
      Just post ->
        case render post of
          Nothing -> status internalServerError500
          Just content ->
            html $ renderText $ html_ $ do
              head_ $ style >> title_ "Post"
              body_ $ do
                p_ $ a_ [href_ "/"] "Home"
                content

  where
    style = link_ [Attribute "rel" "stylesheet", Attribute "href" "/style.css"]

data Post = Post
          { markdown :: T.Text
          , title    :: T.Text
          , date     :: UTCTime
          } deriving (Read, Show)

load :: String -> IO (Maybe Post)
load name = do
  let file = "posts" </> name
  guard =<< doesFileExist file
  readMaybe <$> readFile file

render :: Post -> Maybe (Html ())
render p@(Post markdown title date) =
  case M.parse (pathToPost p) (T.toStrict markdown) of
    Left e -> Nothing
    Right doc -> Just $
      with div_ [id_ (T.toStrict title)] $ do
        with a_ [href_ (T.toStrict $ linkToPost p)] $
          h1_ $ toHtml title
        i_ $ toHtml $ formatTime defaultTimeLocale "%a %e %B %Y" date
        br_ []
        M.render (M.useExtensions extensions doc)
  where extensions = [M.ghcSyntaxHighlighter, M.skylighting, M.footnotes]

linkToPost :: Post -> T.Text
linkToPost p = T.pack $ "/posts" </> kebab (T.unpack (title p))

pathToPost :: Post -> FilePath
pathToPost p = "posts" </> kebab (T.unpack (title p))

save :: T.Text -> T.Text -> IO Post
save md title = do
  curTime <- getCurrentTime
  let kTitle = T.pack $ kebab $ T.unpack title
      p = Post md kTitle curTime
  writeFile (pathToPost p) (show p)
  return p
