{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Control.Monad.IO.Class
import Control.Monad
import qualified Text.MMark as M
import qualified Text.MMark.Extension.Common as M
import Lucid
import Lucid.Base
import System.Directory
import System.FilePath
import Network.HTTP.Types

main :: IO ()
main = scotty 3000 $ do
  get "/style.css" $ file "style.css"

  get "/create" $ html $ renderText $ html_ $ do
    head_ style
    body_ $ with form_ [action_ "create", method_ "post"] $ do
      input_ [name_ "title", type_ "text"]
      with textarea_ [name_ "body", placeholder_ "body"] ""
      input_ [type_ "submit"]

  post "/create" $ do
    title <- param "title"
    body <- param "body"
    let fp = "posts" </> T.unpack title <.> "md"
    liftIO $ T.writeFile fp body
    redirect ("post/" <> title)

  post "/login" $ do
    username <- param "username"
    password <- param "password"
    if username /= ("luke" :: T.Text) || password /= ("pass" :: T.Text)
      then status unauthorized401
      else redirect "/create"

  get "/" $ do
    posts <- fmap dropExtension <$> liftIO (listDirectory "posts")
    contents <- mapM render posts
    html $ renderText $ html_ $ do
      head_ style
      body_ $ do
        with form_ [action_ "/login", method_ "post"] $ do
          input_ [name_ "username", placeholder_ "username", type_ "text"]
          input_ [name_ "password", placeholder_ "password", type_ "password"]
          input_ [type_ "submit"]
        sequence contents


  get "/post/:post" $ do
    post <- param "post"
    content <- render post
    html $ renderText $ html_ $ do
      head_ style
      body_ content

  where
    style = link_ [Attribute "rel" "stylesheet", Attribute "href" "style.css"]

render :: String -> ActionM (Html ())
render post = do
  let name = "posts" </> post <.> ".md"
  markdown <- T.toStrict <$> liftIO (T.readFile name)
  case M.parse name markdown of
    Left e -> return "shit"
    Right doc -> return $ M.render (M.useExtensions extensions doc)
  where extensions = [M.ghcSyntaxHighlighter, M.skylighting, M.footnotes]
