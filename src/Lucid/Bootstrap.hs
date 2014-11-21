{-# OPTIONS -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Bootstrap layout elements. See
-- <http://getbootstrap.com/2.3.2/scaffolding.html> for more
-- information.

module Lucid.Bootstrap
   (
    -- * Containers
    container_
   ,containerFluid_
   -- * Rows
   ,row_
   ,rowFluid_
   -- * Spans
   ,span1_
   ,span2_
   ,span3_
   ,span4_
   ,span5_
   ,span6_
   ,span7_
   ,span8_
   ,span9_
   ,span10_
   ,span11_
   ,span12_)
  where

import Lucid.Base
import Lucid.Html5

import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze

-- | A grid container.
container_ :: Monad m => HtmlT m () -> HtmlT m ()
container_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "container "]

-- | A fluid grid container.
containerFluid_ :: Monad m => HtmlT m () -> HtmlT m ()
containerFluid_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "container-fluid "]

-- | A grid row.
row_ :: Monad m => HtmlT m () -> HtmlT m ()
row_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "row "]

-- | A fluid grid row.
rowFluid_ :: Monad m => HtmlT m () -> HtmlT m ()
rowFluid_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "row-fluid "]

-- | A span of 1 column.
span1_ :: Monad m => HtmlT m () -> HtmlT m ()
span1_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span1 "]

-- | A span of 2 columns.
span2_ :: Monad m => HtmlT m () -> HtmlT m ()
span2_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span2 "]

-- | A span of 3 columns.
span3_ :: Monad m => HtmlT m () -> HtmlT m ()
span3_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span3 "]

-- | A span of 4 columns.
span4_ :: Monad m => HtmlT m () -> HtmlT m ()
span4_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span4 "]

-- | A span of 5 columns.
span5_ :: Monad m => HtmlT m () -> HtmlT m ()
span5_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span5 "]

-- | A span of 6 columns.
span6_ :: Monad m => HtmlT m () -> HtmlT m ()
span6_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span6 "]

-- | A span of 7 columns.
span7_ :: Monad m => HtmlT m () -> HtmlT m ()
span7_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span7 "]

-- | A span of 8 columns.
span8_ :: Monad m => HtmlT m () -> HtmlT m ()
span8_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span8 "]

-- | A span of 9 columns.
span9_ :: Monad m => HtmlT m () -> HtmlT m ()
span9_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span9 "]

-- | A span of 10 columns.
span10_ :: Monad m => HtmlT m () -> HtmlT m ()
span10_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span10 "]

-- | A span of 11 columns.
span11_ :: Monad m => HtmlT m () -> HtmlT m ()
span11_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span11 "]

-- | A span of 12 columns.
span12_ :: Monad m => HtmlT m () -> HtmlT m ()
span12_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span12 "]
