{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lucid.Foundation.Media.Thumbnails where

import Lucid.Base
import Lucid.Html5
import qualified Data.Text as T
import Data.Monoid

-- | The first argument is the @href@ link, and the second is the @src@ to the thumbnail.
tumb_ :: Monad m => T.Text -> T.Text -> HtmlT m ()
tumb_ href src =
  a_ [ class_ "th"
     , makeAttribute "role" "button"
     , makeAttribute "aria-label" "Thumbnail"
     , href_ href
     ] $ img_ [makeAttribute "aria-hidden" "true", src_ src]
