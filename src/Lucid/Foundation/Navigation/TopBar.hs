{-# LANGUAGE OverloadedStrings #-}
{-# LAnGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Navigation.TopBar where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid


fixed_ :: Term arg result => arg -> result
fixed_ =
  termWith "div" [class_ "fixed"]

contain_to_grid_ :: Term arg result => arg -> result
contain_to_grid_ =
  termWith "div" [class_ "contain-to-grid"]

data_topbar_ :: Attribute
data_topbar_ = data_ "topbar" ""

top_bar_ :: T.Text
top_bar_ = " top-bar "

sticky_ :: T.Text
sticky_ = " sticky "

sticky_on_ :: T.Text -> Attribute
sticky_on_ size =
  data_ "options" ("sticky_on: " <> size)

clickable_ :: Attribute
clickable_ =
  data_ "options" "is_hover: false"

divider_ :: Monad m => HtmlT m ()
divider_ =
  li_ [class_ "divider"] mempty

title_area_ :: Term arg result => arg -> result
title_area_ =
  termWith "ul" [class_ "title-area"]




