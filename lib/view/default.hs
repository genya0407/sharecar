{-# LANGUAGE OverloadedStrings #-}

module View.Default where

import Lucid
import Data.Text.Lazy (toStrict)
import Data.Text
import Model.Type
import Database.Persist
import Database.Persist.Sql
import Control.Monad
import Data.Monoid

layout :: Html () -> Text
layout content = toStrict . renderText $ do
  doctype_
  html_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    body_ content

root_ :: Text
root_ = layout $ do
  h1_ "トップページ"
  p_ "ここはトップページです"

users_ :: [Entity User] -> Text
users_ users = layout $ do
  forM_ users $ \(Entity userid user) -> do
    ul_ $ do
      li_ . toHtml $ userid
      li_ . toHtml $ userName user
      li_ . toHtml $ userPhoneNumber user
