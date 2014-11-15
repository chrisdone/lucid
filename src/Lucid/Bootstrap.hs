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
container_ :: Html () -> Html ()
container_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "container"]

-- | A fluid grid container.
containerFluid_ :: Html () -> Html ()
containerFluid_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "container-fluid"]

-- | A grid row.
row_ :: Html () -> Html ()
row_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "row"]

-- | A fluid grid row.
rowFluid_ :: Html () -> Html ()
rowFluid_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "row-fluid"]

-- | A span of 1 column.
span1_ :: Html () -> Html ()
span1_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span1"]

-- | A span of 2 columns.
span2_ :: Html () -> Html ()
span2_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span2"]

-- | A span of 3 columns.
span3_ :: Html () -> Html ()
span3_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span3"]

-- | A span of 4 columns.
span4_ :: Html () -> Html ()
span4_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span4"]

-- | A span of 5 columns.
span5_ :: Html () -> Html ()
span5_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span5"]

-- | A span of 6 columns.
span6_ :: Html () -> Html ()
span6_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span6"]

-- | A span of 7 columns.
span7_ :: Html () -> Html ()
span7_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span7"]

-- | A span of 8 columns.
span8_ :: Html () -> Html ()
span8_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span8"]

-- | A span of 9 columns.
span9_ :: Html () -> Html ()
span9_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span9"]

-- | A span of 10 columns.
span10_ :: Html () -> Html ()
span10_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span10"]

-- | A span of 11 columns.
span11_ :: Html () -> Html ()
span11_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span11"]

-- | A span of 12 columns.
span12_ :: Html () -> Html ()
span12_ =
  with (makeElement (Blaze.fromString "div"))
       [class_ "span12"]
