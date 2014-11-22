{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-type-defaults #-}

-- | Html5 terms.

module Lucid.Html5 where

import           Data.Text (Text)
import           Lucid.Base

import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import           Data.Monoid

-- | @DOCTYPE@ element
doctype_  :: Monad m => HtmlT m ()
doctype_ = makeElementNoEnd (Blaze.fromString "!DOCTYPE HTML")

-- | @DOCTYPE@ element + @html@ element
doctypehtml_  :: Monad m => HtmlT m () -> HtmlT m ()
doctypehtml_ m =
  do doctype_
     html_ m

-- | @a@ element
a_  :: Monad m => HtmlT m () -> HtmlT m ()
a_ = makeElement (Blaze.fromString "a")

-- | @abbr@ element
abbr_  :: Monad m => HtmlT m () -> HtmlT m ()
abbr_ = makeElement (Blaze.fromString "abbr")

-- | @address@ element
address_  :: Monad m => HtmlT m () -> HtmlT m ()
address_ = makeElement (Blaze.fromString "address")

-- | @area@ element
area_  :: Monad m => HtmlT m ()
area_ = makeElementNoEnd (Blaze.fromString "area")

-- | @article@ element
article_  :: Monad m => HtmlT m () -> HtmlT m ()
article_ = makeElement (Blaze.fromString "article")

-- | @aside@ element
aside_  :: Monad m => HtmlT m () -> HtmlT m ()
aside_ = makeElement (Blaze.fromString "aside")

-- | @audio@ element
audio_  :: Monad m => HtmlT m () -> HtmlT m ()
audio_ = makeElement (Blaze.fromString "audio")

-- | @b@ element
b_  :: Monad m => HtmlT m () -> HtmlT m ()
b_ = makeElement (Blaze.fromString "b")

-- | @base@ element
base_  :: Monad m => HtmlT m ()
base_ = makeElementNoEnd (Blaze.fromString "base")

-- | @bdo@ element
bdo_  :: Monad m => HtmlT m () -> HtmlT m ()
bdo_ = makeElement (Blaze.fromString "bdo")

-- | @blockquote@ element
blockquote_  :: Monad m => HtmlT m () -> HtmlT m ()
blockquote_ = makeElement (Blaze.fromString "blockquote")

-- | @body@ element
body_  :: Monad m => HtmlT m () -> HtmlT m ()
body_ = makeElement (Blaze.fromString "body")

-- | @br@ element
br_  :: Monad m => HtmlT m ()
br_ = makeElementNoEnd (Blaze.fromString "br")

-- | @button@ element
button_  :: Monad m => HtmlT m () -> HtmlT m ()
button_ = makeElement (Blaze.fromString "button")

-- | @canvas@ element
canvas_  :: Monad m => HtmlT m () -> HtmlT m ()
canvas_ = makeElement (Blaze.fromString "canvas")

-- | @caption@ element
caption_  :: Monad m => HtmlT m () -> HtmlT m ()
caption_ = makeElement (Blaze.fromString "caption")

-- | @cite@ element or @cite@ attribute.
cite_ :: Mixed a r => a -> r
cite_ = mixed ("cite")

-- | @code@ element
code_  :: Monad m => HtmlT m () -> HtmlT m ()
code_ = makeElement (Blaze.fromString "code")

-- | @col@ element
col_  :: Monad m => HtmlT m ()
col_ = makeElementNoEnd (Blaze.fromString "col")

-- | @colgroup@ element
colgroup_  :: Monad m => HtmlT m () -> HtmlT m ()
colgroup_ = makeElement (Blaze.fromString "colgroup")

-- | @command@ element
command_  :: Monad m => HtmlT m () -> HtmlT m ()
command_ = makeElement (Blaze.fromString "command")

-- | @datalist@ element
datalist_  :: Monad m => HtmlT m () -> HtmlT m ()
datalist_ = makeElement (Blaze.fromString "datalist")

-- | @dd@ element
dd_  :: Monad m => HtmlT m () -> HtmlT m ()
dd_ = makeElement (Blaze.fromString "dd")

-- | @del@ element
del_  :: Monad m => HtmlT m () -> HtmlT m ()
del_ = makeElement (Blaze.fromString "del")

-- | @details@ element
details_  :: Monad m => HtmlT m () -> HtmlT m ()
details_ = makeElement (Blaze.fromString "details")

-- | @dfn@ element
dfn_  :: Monad m => HtmlT m () -> HtmlT m ()
dfn_ = makeElement (Blaze.fromString "dfn")

-- | @div@ element
div_  :: Monad m => HtmlT m () -> HtmlT m ()
div_ = makeElement (Blaze.fromString "div")

-- | @dl@ element
dl_  :: Monad m => HtmlT m () -> HtmlT m ()
dl_ = makeElement (Blaze.fromString "dl")

-- | @dt@ element
dt_  :: Monad m => HtmlT m () -> HtmlT m ()
dt_ = makeElement (Blaze.fromString "dt")

-- | @em@ element
em_  :: Monad m => HtmlT m () -> HtmlT m ()
em_ = makeElement (Blaze.fromString "em")

-- | @embed@ element
embed_  :: Monad m => HtmlT m ()
embed_ = makeElementNoEnd (Blaze.fromString "embed")

-- | @fieldset@ element
fieldset_  :: Monad m => HtmlT m () -> HtmlT m ()
fieldset_ = makeElement (Blaze.fromString "fieldset")

-- | @figcaption@ element
figcaption_  :: Monad m => HtmlT m () -> HtmlT m ()
figcaption_ = makeElement (Blaze.fromString "figcaption")

-- | @figure@ element
figure_  :: Monad m => HtmlT m () -> HtmlT m ()
figure_ = makeElement (Blaze.fromString "figure")

-- | @footer@ element
footer_  :: Monad m => HtmlT m () -> HtmlT m ()
footer_ = makeElement (Blaze.fromString "footer")

-- | @form@ element or @form@ attribute
form_ :: Mixed a r => a -> r
form_ = mixed ("form")

-- | @h1@ element
h1_  :: Monad m => HtmlT m () -> HtmlT m ()
h1_ = makeElement (Blaze.fromString "h1")

-- | @h2@ element
h2_  :: Monad m => HtmlT m () -> HtmlT m ()
h2_ = makeElement (Blaze.fromString "h2")

-- | @h3@ element
h3_  :: Monad m => HtmlT m () -> HtmlT m ()
h3_ = makeElement (Blaze.fromString "h3")

-- | @h4@ element
h4_  :: Monad m => HtmlT m () -> HtmlT m ()
h4_ = makeElement (Blaze.fromString "h4")

-- | @h5@ element
h5_  :: Monad m => HtmlT m () -> HtmlT m ()
h5_ = makeElement (Blaze.fromString "h5")

-- | @h6@ element
h6_  :: Monad m => HtmlT m () -> HtmlT m ()
h6_ = makeElement (Blaze.fromString "h6")

-- | @head@ element
head_  :: Monad m => HtmlT m () -> HtmlT m ()
head_ = makeElement (Blaze.fromString "head")

-- | @header@ element
header_  :: Monad m => HtmlT m () -> HtmlT m ()
header_ = makeElement (Blaze.fromString "header")

-- | @hgroup@ element
hgroup_  :: Monad m => HtmlT m () -> HtmlT m ()
hgroup_ = makeElement (Blaze.fromString "hgroup")

-- | @hr@ element
hr_  :: Monad m => HtmlT m ()
hr_ = makeElementNoEnd (Blaze.fromString "hr")

-- | @html@ element
html_  :: Monad m => HtmlT m () -> HtmlT m ()
html_ = makeElement (Blaze.fromString "html")

-- | @i@ element
i_  :: Monad m => HtmlT m () -> HtmlT m ()
i_ = makeElement (Blaze.fromString "i")

-- | @iframe@ element
iframe_  :: Monad m => HtmlT m () -> HtmlT m ()
iframe_ = makeElement (Blaze.fromString "iframe")

-- | @img@ element
img_  :: Monad m => HtmlT m ()
img_ = makeElementNoEnd (Blaze.fromString "img")

-- | @input@ element
input_  :: Monad m => HtmlT m ()
input_ = makeElementNoEnd (Blaze.fromString "input")

-- | @ins@ element
ins_  :: Monad m => HtmlT m () -> HtmlT m ()
ins_ = makeElement (Blaze.fromString "ins")

-- | @kbd@ element
kbd_  :: Monad m => HtmlT m () -> HtmlT m ()
kbd_ = makeElement (Blaze.fromString "kbd")

-- | @keygen@ element
keygen_  :: Monad m => HtmlT m ()
keygen_ = makeElementNoEnd (Blaze.fromString "keygen")

-- | @label@ element or @label@ attribute
label_ :: Mixed a r => a -> r
label_ = mixed ("label")

-- | @legend@ element
legend_  :: Monad m => HtmlT m () -> HtmlT m ()
legend_ = makeElement (Blaze.fromString "legend")

-- | @li@ element
li_  :: Monad m => HtmlT m () -> HtmlT m ()
li_ = makeElement (Blaze.fromString "li")

-- | @link@ element
link_  :: Monad m => HtmlT m ()
link_ = makeElementNoEnd (Blaze.fromString "link")

-- | @map@ element
map_  :: Monad m => HtmlT m () -> HtmlT m ()
map_ = makeElement (Blaze.fromString "map")

-- | @mark@ element
mark_  :: Monad m => HtmlT m () -> HtmlT m ()
mark_ = makeElement (Blaze.fromString "mark")

-- | @menu@ element
menu_  :: Monad m => HtmlT m () -> HtmlT m ()
menu_ = makeElement (Blaze.fromString "menu")

-- | @menuitem@ element
menuitem_  :: Monad m => HtmlT m ()
menuitem_ = makeElementNoEnd (Blaze.fromString "menuitem")

-- | @meta@ element
meta_  :: Monad m => HtmlT m ()
meta_ = makeElementNoEnd (Blaze.fromString "meta")

-- | @meter@ element
meter_  :: Monad m => HtmlT m () -> HtmlT m ()
meter_ = makeElement (Blaze.fromString "meter")

-- | @nav@ element
nav_  :: Monad m => HtmlT m () -> HtmlT m ()
nav_ = makeElement (Blaze.fromString "nav")

-- | @noscript@ element
noscript_  :: Monad m => HtmlT m () -> HtmlT m ()
noscript_ = makeElement (Blaze.fromString "noscript")

-- | @object@ element
object_  :: Monad m => HtmlT m () -> HtmlT m ()
object_ = makeElement (Blaze.fromString "object")

-- | @ol@ element
ol_  :: Monad m => HtmlT m () -> HtmlT m ()
ol_ = makeElement (Blaze.fromString "ol")

-- | @optgroup@ element
optgroup_  :: Monad m => HtmlT m () -> HtmlT m ()
optgroup_ = makeElement (Blaze.fromString "optgroup")

-- | @option@ element
option_  :: Monad m => HtmlT m () -> HtmlT m ()
option_ = makeElement (Blaze.fromString "option")

-- | @output@ element
output_  :: Monad m => HtmlT m () -> HtmlT m ()
output_ = makeElement (Blaze.fromString "output")

-- | @p@ element
p_  :: Monad m => HtmlT m () -> HtmlT m ()
p_ = makeElement (Blaze.fromString "p")

-- | @param@ element
param_  :: Monad m => HtmlT m ()
param_ = makeElementNoEnd (Blaze.fromString "param")

-- | @pre@ element
pre_  :: Monad m => HtmlT m () -> HtmlT m ()
pre_ = makeElement (Blaze.fromString "pre")

-- | @progress@ element
progress_  :: Monad m => HtmlT m () -> HtmlT m ()
progress_ = makeElement (Blaze.fromString "progress")

-- | @q@ element
q_  :: Monad m => HtmlT m () -> HtmlT m ()
q_ = makeElement (Blaze.fromString "q")

-- | @rp@ element
rp_  :: Monad m => HtmlT m () -> HtmlT m ()
rp_ = makeElement (Blaze.fromString "rp")

-- | @rt@ element
rt_  :: Monad m => HtmlT m () -> HtmlT m ()
rt_ = makeElement (Blaze.fromString "rt")

-- | @ruby@ element
ruby_  :: Monad m => HtmlT m () -> HtmlT m ()
ruby_ = makeElement (Blaze.fromString "ruby")

-- | @samp@ element
samp_  :: Monad m => HtmlT m () -> HtmlT m ()
samp_ = makeElement (Blaze.fromString "samp")

-- | @script@ element
script_  :: (ToHtml t,Monad m) => t -> HtmlT m ()
script_ = makeElement (Blaze.fromString "script") . toHtmlRaw

-- | @section@ element
section_  :: Monad m => HtmlT m () -> HtmlT m ()
section_ = makeElement (Blaze.fromString "section")

-- | @select@ element
select_  :: Monad m => HtmlT m () -> HtmlT m ()
select_ = makeElement (Blaze.fromString "select")

-- | @small@ element
small_  :: Monad m => HtmlT m () -> HtmlT m ()
small_ = makeElement (Blaze.fromString "small")

-- | @source@ element
source_  :: Monad m => HtmlT m ()
source_ = makeElementNoEnd (Blaze.fromString "source")

-- | @span@ element or @span@ attribute
span_  :: Mixed a r => a -> r
span_ = mixed ("span")

-- | @strong@ element
strong_  :: Monad m => HtmlT m () -> HtmlT m ()
strong_ = makeElement (Blaze.fromString "strong")

-- | @style@ element or @style@ attribute
style_  :: Mixed a r => a -> r
style_ = mixed ("style")

-- | @sub@ element
sub_  :: Monad m => HtmlT m () -> HtmlT m ()
sub_ = makeElement (Blaze.fromString "sub")

-- | @summary@ element or @summary@ attribute
summary_  :: Mixed a r => a -> r
summary_ = mixed ("summary")

-- | @sup@ element
sup_  :: Monad m => HtmlT m () -> HtmlT m ()
sup_ = makeElement (Blaze.fromString "sup")

-- | @table@ element
table_  :: Monad m => HtmlT m () -> HtmlT m ()
table_ = makeElement (Blaze.fromString "table")

-- | @tbody@ element
tbody_  :: Monad m => HtmlT m () -> HtmlT m ()
tbody_ = makeElement (Blaze.fromString "tbody")

-- | @td@ element
td_  :: Monad m => HtmlT m () -> HtmlT m ()
td_ = makeElement (Blaze.fromString "td")

-- | @textarea@ element
textarea_  :: Monad m => HtmlT m () -> HtmlT m ()
textarea_ = makeElement (Blaze.fromString "textarea")

-- | @tfoot@ element
tfoot_  :: Monad m => HtmlT m () -> HtmlT m ()
tfoot_ = makeElement (Blaze.fromString "tfoot")

-- | @th@ element
th_  :: Monad m => HtmlT m () -> HtmlT m ()
th_ = makeElement (Blaze.fromString "th")

-- | @thead@ element
thead_  :: Monad m => HtmlT m () -> HtmlT m ()
thead_ = makeElement (Blaze.fromString "thead")

-- | @time@ element
time_  :: Monad m => HtmlT m () -> HtmlT m ()
time_ = makeElement (Blaze.fromString "time")

-- | @title@ element or @title@ attribute
title_  :: Mixed a r => a -> r
title_ = mixed ("title")

-- | @tr@ element
tr_  :: Monad m => HtmlT m () -> HtmlT m ()
tr_ = makeElement (Blaze.fromString "tr")

-- | @track@ element
track_  :: Monad m => HtmlT m ()
track_ = makeElementNoEnd (Blaze.fromString "track")

-- | @ul@ element
ul_  :: Monad m => HtmlT m () -> HtmlT m ()
ul_ = makeElement (Blaze.fromString "ul")

-- | @var@ element
var_  :: Monad m => HtmlT m () -> HtmlT m ()
var_ = makeElement (Blaze.fromString "var")

-- | @video@ element
video_  :: Monad m => HtmlT m () -> HtmlT m ()
video_ = makeElement (Blaze.fromString "video")

-- | @wbr@ element
wbr_  :: Monad m => HtmlT m ()
wbr_ = makeElementNoEnd (Blaze.fromString "wbr")

-- | The @accept@ attribute.
accept_ :: ToText a => a -> (Text,Text)
accept_ = (,) ("accept") . toText

-- | The @acceptCharset@ attribute.
acceptCharset_ :: ToText a => a -> (Text,Text)
acceptCharset_ = (,) ("acceptCharset") . toText

-- | The @accesskey@ attribute.
accesskey_ :: ToText a => a -> (Text,Text)
accesskey_ = (,) ("accesskey") . toText

-- | The @action@ attribute.
action_ :: ToText a => a -> (Text,Text)
action_ = (,) ("action") . toText

-- | The @alt@ attribute.
alt_ :: ToText a => a -> (Text,Text)
alt_ = (,) ("alt") . toText

-- | The @async@ attribute.
async_ :: ToText a => a -> (Text,Text)
async_ = (,) ("async") . toText

-- | The @autocomplete@ attribute.
autocomplete_ :: ToText a => a -> (Text,Text)
autocomplete_ = (,) ("autocomplete") . toText

-- | The @autofocus@ attribute.
autofocus_ :: (Text,Text)
autofocus_ = (,) ("autofocus") mempty

-- | The @autoplay@ attribute.
autoplay_ :: ToText a => a -> (Text,Text)
autoplay_ = (,) ("autoplay") . toText

-- | The @challenge@ attribute.
challenge_ :: ToText a => a -> (Text,Text)
challenge_ = (,) ("challenge") . toText

-- | The @charset@ attribute.
charset_ :: ToText a => a -> (Text,Text)
charset_ = (,) ("charset") . toText

-- | The @checked@ attribute.
checked_ :: (Text,Text)
checked_ = (,) ("checked") mempty

-- | The @class@ attribute.
class_ :: ToText a => a -> (Text,Text)
class_ = (,) ("class") . toText

-- | The @cols@ attribute.
cols_ :: ToText a => a -> (Text,Text)
cols_ = (,) ("cols") . toText

-- | The @colspan@ attribute.
colspan_ :: ToText a => a -> (Text,Text)
colspan_ = (,) ("colspan") . toText

-- | The @content@ attribute.
content_ :: ToText a => a -> (Text,Text)
content_ = (,) ("content") . toText

-- | The @contenteditable@ attribute.
contenteditable_ :: ToText a => a -> (Text,Text)
contenteditable_ = (,) ("contenteditable") . toText

-- | The @contextmenu@ attribute.
contextmenu_ :: ToText a => a -> (Text,Text)
contextmenu_ = (,) ("contextmenu") . toText

-- | The @controls@ attribute.
controls_ :: ToText a => a -> (Text,Text)
controls_ = (,) ("controls") . toText

-- | The @coords@ attribute.
coords_ :: ToText a => a -> (Text,Text)
coords_ = (,) ("coords") . toText

-- | The @data@ attribute.
data_ :: ToText a => a -> (Text,Text)
data_ = (,) ("data") . toText

-- | The @datetime@ attribute.
datetime_ :: ToText a => a -> (Text,Text)
datetime_ = (,) ("datetime") . toText

-- | The @defer@ attribute.
defer_ :: ToText a => a -> (Text,Text)
defer_ = (,) ("defer") . toText

-- | The @dir@ attribute.
dir_ :: ToText a => a -> (Text,Text)
dir_ = (,) ("dir") . toText

-- | The @disabled@ attribute.
disabled_ :: ToText a => a -> (Text,Text)
disabled_ = (,) ("disabled") . toText

-- | The @draggable@ attribute.
draggable_ :: ToText a => a -> (Text,Text)
draggable_ = (,) ("draggable") . toText

-- | The @enctype@ attribute.
enctype_ :: ToText a => a -> (Text,Text)
enctype_ = (,) ("enctype") . toText

-- | The @for@ attribute.
for_ :: ToText a => a -> (Text,Text)
for_ = (,) ("for") . toText

-- | The @formaction@ attribute.
formaction_ :: ToText a => a -> (Text,Text)
formaction_ = (,) ("formaction") . toText

-- | The @formenctype@ attribute.
formenctype_ :: ToText a => a -> (Text,Text)
formenctype_ = (,) ("formenctype") . toText

-- | The @formmethod@ attribute.
formmethod_ :: ToText a => a -> (Text,Text)
formmethod_ = (,) ("formmethod") . toText

-- | The @formnovalidate@ attribute.
formnovalidate_ :: ToText a => a -> (Text,Text)
formnovalidate_ = (,) ("formnovalidate") . toText

-- | The @formtarget@ attribute.
formtarget_ :: ToText a => a -> (Text,Text)
formtarget_ = (,) ("formtarget") . toText

-- | The @headers@ attribute.
headers_ :: ToText a => a -> (Text,Text)
headers_ = (,) ("headers") . toText

-- | The @height@ attribute.
height_ :: ToText a => a -> (Text,Text)
height_ = (,) ("height") . toText

-- | The @hidden@ attribute.
hidden_ :: ToText a => a -> (Text,Text)
hidden_ = (,) ("hidden") . toText

-- | The @high@ attribute.
high_ :: ToText a => a -> (Text,Text)
high_ = (,) ("high") . toText

-- | The @href@ attribute.
href_ :: ToText a => a -> (Text,Text)
href_ = (,) ("href") . toText

-- | The @hreflang@ attribute.
hreflang_ :: ToText a => a -> (Text,Text)
hreflang_ = (,) ("hreflang") . toText

-- | The @httpEquiv@ attribute.
httpEquiv_ :: ToText a => a -> (Text,Text)
httpEquiv_ = (,) ("httpEquiv") . toText

-- | The @icon@ attribute.
icon_ :: ToText a => a -> (Text,Text)
icon_ = (,) ("icon") . toText

-- | The @id@ attribute.
id_ :: ToText a => a -> (Text,Text)
id_ = (,) ("id") . toText

-- | The @ismap@ attribute.
ismap_ :: ToText a => a -> (Text,Text)
ismap_ = (,) ("ismap") . toText

-- | The @item@ attribute.
item_ :: ToText a => a -> (Text,Text)
item_ = (,) ("item") . toText

-- | The @itemprop@ attribute.
itemprop_ :: ToText a => a -> (Text,Text)
itemprop_ = (,) ("itemprop") . toText

-- | The @keytype@ attribute.
keytype_ :: ToText a => a -> (Text,Text)
keytype_ = (,) ("keytype") . toText

-- | The @lang@ attribute.
lang_ :: ToText a => a -> (Text,Text)
lang_ = (,) ("lang") . toText

-- | The @list@ attribute.
list_ :: ToText a => a -> (Text,Text)
list_ = (,) ("list") . toText

-- | The @loop@ attribute.
loop_ :: ToText a => a -> (Text,Text)
loop_ = (,) ("loop") . toText

-- | The @low@ attribute.
low_ :: ToText a => a -> (Text,Text)
low_ = (,) ("low") . toText

-- | The @manifest@ attribute.
manifest_ :: ToText a => a -> (Text,Text)
manifest_ = (,) ("manifest") . toText

-- | The @max@ attribute.
max_ :: ToText a => a -> (Text,Text)
max_ = (,) ("max") . toText

-- | The @maxlength@ attribute.
maxlength_ :: ToText a => a -> (Text,Text)
maxlength_ = (,) ("maxlength") . toText

-- | The @media@ attribute.
media_ :: ToText a => a -> (Text,Text)
media_ = (,) ("media") . toText

-- | The @method@ attribute.
method_ :: ToText a => a -> (Text,Text)
method_ = (,) ("method") . toText

-- | The @min@ attribute.
min_ :: ToText a => a -> (Text,Text)
min_ = (,) ("min") . toText

-- | The @multiple@ attribute.
multiple_ :: ToText a => a -> (Text,Text)
multiple_ = (,) ("multiple") . toText

-- | The @name@ attribute.
name_ :: ToText a => a -> (Text,Text)
name_ = (,) ("name") . toText

-- | The @novalidate@ attribute.
novalidate_ :: ToText a => a -> (Text,Text)
novalidate_ = (,) ("novalidate") . toText

-- | The @onbeforeonload@ attribute.
onbeforeonload_ :: ToText a => a -> (Text,Text)
onbeforeonload_ = (,) ("onbeforeonload") . toText

-- | The @onbeforeprint@ attribute.
onbeforeprint_ :: ToText a => a -> (Text,Text)
onbeforeprint_ = (,) ("onbeforeprint") . toText

-- | The @onblur@ attribute.
onblur_ :: ToText a => a -> (Text,Text)
onblur_ = (,) ("onblur") . toText

-- | The @oncanplay@ attribute.
oncanplay_ :: ToText a => a -> (Text,Text)
oncanplay_ = (,) ("oncanplay") . toText

-- | The @oncanplaythrough@ attribute.
oncanplaythrough_ :: ToText a => a -> (Text,Text)
oncanplaythrough_ = (,) ("oncanplaythrough") . toText

-- | The @onchange@ attribute.
onchange_ :: ToText a => a -> (Text,Text)
onchange_ = (,) ("onchange") . toText

-- | The @onclick@ attribute.
onclick_ :: ToText a => a -> (Text,Text)
onclick_ = (,) ("onclick") . toText

-- | The @oncontextmenu@ attribute.
oncontextmenu_ :: ToText a => a -> (Text,Text)
oncontextmenu_ = (,) ("oncontextmenu") . toText

-- | The @ondblclick@ attribute.
ondblclick_ :: ToText a => a -> (Text,Text)
ondblclick_ = (,) ("ondblclick") . toText

-- | The @ondrag@ attribute.
ondrag_ :: ToText a => a -> (Text,Text)
ondrag_ = (,) ("ondrag") . toText

-- | The @ondragend@ attribute.
ondragend_ :: ToText a => a -> (Text,Text)
ondragend_ = (,) ("ondragend") . toText

-- | The @ondragenter@ attribute.
ondragenter_ :: ToText a => a -> (Text,Text)
ondragenter_ = (,) ("ondragenter") . toText

-- | The @ondragleave@ attribute.
ondragleave_ :: ToText a => a -> (Text,Text)
ondragleave_ = (,) ("ondragleave") . toText

-- | The @ondragover@ attribute.
ondragover_ :: ToText a => a -> (Text,Text)
ondragover_ = (,) ("ondragover") . toText

-- | The @ondragstart@ attribute.
ondragstart_ :: ToText a => a -> (Text,Text)
ondragstart_ = (,) ("ondragstart") . toText

-- | The @ondrop@ attribute.
ondrop_ :: ToText a => a -> (Text,Text)
ondrop_ = (,) ("ondrop") . toText

-- | The @ondurationchange@ attribute.
ondurationchange_ :: ToText a => a -> (Text,Text)
ondurationchange_ = (,) ("ondurationchange") . toText

-- | The @onemptied@ attribute.
onemptied_ :: ToText a => a -> (Text,Text)
onemptied_ = (,) ("onemptied") . toText

-- | The @onended@ attribute.
onended_ :: ToText a => a -> (Text,Text)
onended_ = (,) ("onended") . toText

-- | The @onerror@ attribute.
onerror_ :: ToText a => a -> (Text,Text)
onerror_ = (,) ("onerror") . toText

-- | The @onfocus@ attribute.
onfocus_ :: ToText a => a -> (Text,Text)
onfocus_ = (,) ("onfocus") . toText

-- | The @onformchange@ attribute.
onformchange_ :: ToText a => a -> (Text,Text)
onformchange_ = (,) ("onformchange") . toText

-- | The @onforminput@ attribute.
onforminput_ :: ToText a => a -> (Text,Text)
onforminput_ = (,) ("onforminput") . toText

-- | The @onhaschange@ attribute.
onhaschange_ :: ToText a => a -> (Text,Text)
onhaschange_ = (,) ("onhaschange") . toText

-- | The @oninput@ attribute.
oninput_ :: ToText a => a -> (Text,Text)
oninput_ = (,) ("oninput") . toText

-- | The @oninvalid@ attribute.
oninvalid_ :: ToText a => a -> (Text,Text)
oninvalid_ = (,) ("oninvalid") . toText

-- | The @onkeydown@ attribute.
onkeydown_ :: ToText a => a -> (Text,Text)
onkeydown_ = (,) ("onkeydown") . toText

-- | The @onkeyup@ attribute.
onkeyup_ :: ToText a => a -> (Text,Text)
onkeyup_ = (,) ("onkeyup") . toText

-- | The @onload@ attribute.
onload_ :: ToText a => a -> (Text,Text)
onload_ = (,) ("onload") . toText

-- | The @onloadeddata@ attribute.
onloadeddata_ :: ToText a => a -> (Text,Text)
onloadeddata_ = (,) ("onloadeddata") . toText

-- | The @onloadedmetadata@ attribute.
onloadedmetadata_ :: ToText a => a -> (Text,Text)
onloadedmetadata_ = (,) ("onloadedmetadata") . toText

-- | The @onloadstart@ attribute.
onloadstart_ :: ToText a => a -> (Text,Text)
onloadstart_ = (,) ("onloadstart") . toText

-- | The @onmessage@ attribute.
onmessage_ :: ToText a => a -> (Text,Text)
onmessage_ = (,) ("onmessage") . toText

-- | The @onmousedown@ attribute.
onmousedown_ :: ToText a => a -> (Text,Text)
onmousedown_ = (,) ("onmousedown") . toText

-- | The @onmousemove@ attribute.
onmousemove_ :: ToText a => a -> (Text,Text)
onmousemove_ = (,) ("onmousemove") . toText

-- | The @onmouseout@ attribute.
onmouseout_ :: ToText a => a -> (Text,Text)
onmouseout_ = (,) ("onmouseout") . toText

-- | The @onmouseover@ attribute.
onmouseover_ :: ToText a => a -> (Text,Text)
onmouseover_ = (,) ("onmouseover") . toText

-- | The @onmouseup@ attribute.
onmouseup_ :: ToText a => a -> (Text,Text)
onmouseup_ = (,) ("onmouseup") . toText

-- | The @onmousewheel@ attribute.
onmousewheel_ :: ToText a => a -> (Text,Text)
onmousewheel_ = (,) ("onmousewheel") . toText

-- | The @ononline@ attribute.
ononline_ :: ToText a => a -> (Text,Text)
ononline_ = (,) ("ononline") . toText

-- | The @onpagehide@ attribute.
onpagehide_ :: ToText a => a -> (Text,Text)
onpagehide_ = (,) ("onpagehide") . toText

-- | The @onpageshow@ attribute.
onpageshow_ :: ToText a => a -> (Text,Text)
onpageshow_ = (,) ("onpageshow") . toText

-- | The @onpause@ attribute.
onpause_ :: ToText a => a -> (Text,Text)
onpause_ = (,) ("onpause") . toText

-- | The @onplay@ attribute.
onplay_ :: ToText a => a -> (Text,Text)
onplay_ = (,) ("onplay") . toText

-- | The @onplaying@ attribute.
onplaying_ :: ToText a => a -> (Text,Text)
onplaying_ = (,) ("onplaying") . toText

-- | The @onprogress@ attribute.
onprogress_ :: ToText a => a -> (Text,Text)
onprogress_ = (,) ("onprogress") . toText

-- | The @onpropstate@ attribute.
onpropstate_ :: ToText a => a -> (Text,Text)
onpropstate_ = (,) ("onpropstate") . toText

-- | The @onratechange@ attribute.
onratechange_ :: ToText a => a -> (Text,Text)
onratechange_ = (,) ("onratechange") . toText

-- | The @onreadystatechange@ attribute.
onreadystatechange_ :: ToText a => a -> (Text,Text)
onreadystatechange_ = (,) ("onreadystatechange") . toText

-- | The @onredo@ attribute.
onredo_ :: ToText a => a -> (Text,Text)
onredo_ = (,) ("onredo") . toText

-- | The @onresize@ attribute.
onresize_ :: ToText a => a -> (Text,Text)
onresize_ = (,) ("onresize") . toText

-- | The @onscroll@ attribute.
onscroll_ :: ToText a => a -> (Text,Text)
onscroll_ = (,) ("onscroll") . toText

-- | The @onseeked@ attribute.
onseeked_ :: ToText a => a -> (Text,Text)
onseeked_ = (,) ("onseeked") . toText

-- | The @onseeking@ attribute.
onseeking_ :: ToText a => a -> (Text,Text)
onseeking_ = (,) ("onseeking") . toText

-- | The @onselect@ attribute.
onselect_ :: ToText a => a -> (Text,Text)
onselect_ = (,) ("onselect") . toText

-- | The @onstalled@ attribute.
onstalled_ :: ToText a => a -> (Text,Text)
onstalled_ = (,) ("onstalled") . toText

-- | The @onstorage@ attribute.
onstorage_ :: ToText a => a -> (Text,Text)
onstorage_ = (,) ("onstorage") . toText

-- | The @onsubmit@ attribute.
onsubmit_ :: ToText a => a -> (Text,Text)
onsubmit_ = (,) ("onsubmit") . toText

-- | The @onsuspend@ attribute.
onsuspend_ :: ToText a => a -> (Text,Text)
onsuspend_ = (,) ("onsuspend") . toText

-- | The @ontimeupdate@ attribute.
ontimeupdate_ :: ToText a => a -> (Text,Text)
ontimeupdate_ = (,) ("ontimeupdate") . toText

-- | The @onundo@ attribute.
onundo_ :: ToText a => a -> (Text,Text)
onundo_ = (,) ("onundo") . toText

-- | The @onunload@ attribute.
onunload_ :: ToText a => a -> (Text,Text)
onunload_ = (,) ("onunload") . toText

-- | The @onvolumechange@ attribute.
onvolumechange_ :: ToText a => a -> (Text,Text)
onvolumechange_ = (,) ("onvolumechange") . toText

-- | The @onwaiting@ attribute.
onwaiting_ :: ToText a => a -> (Text,Text)
onwaiting_ = (,) ("onwaiting") . toText

-- | The @open@ attribute.
open_ :: ToText a => a -> (Text,Text)
open_ = (,) ("open") . toText

-- | The @optimum@ attribute.
optimum_ :: ToText a => a -> (Text,Text)
optimum_ = (,) ("optimum") . toText

-- | The @pattern@ attribute.
pattern_ :: ToText a => a -> (Text,Text)
pattern_ = (,) ("pattern") . toText

-- | The @ping@ attribute.
ping_ :: ToText a => a -> (Text,Text)
ping_ = (,) ("ping") . toText

-- | The @placeholder@ attribute.
placeholder_ :: ToText a => a -> (Text,Text)
placeholder_ = (,) ("placeholder") . toText

-- | The @preload@ attribute.
preload_ :: ToText a => a -> (Text,Text)
preload_ = (,) ("preload") . toText

-- | The @pubdate@ attribute.
pubdate_ :: ToText a => a -> (Text,Text)
pubdate_ = (,) ("pubdate") . toText

-- | The @radiogroup@ attribute.
radiogroup_ :: ToText a => a -> (Text,Text)
radiogroup_ = (,) ("radiogroup") . toText

-- | The @readonly@ attribute.
readonly_ :: ToText a => a -> (Text,Text)
readonly_ = (,) ("readonly") . toText

-- | The @rel@ attribute.
rel_ :: ToText a => a -> (Text,Text)
rel_ = (,) ("rel") . toText

-- | The @required@ attribute.
required_ :: ToText a => a -> (Text,Text)
required_ = (,) ("required") . toText

-- | The @reversed@ attribute.
reversed_ :: ToText a => a -> (Text,Text)
reversed_ = (,) ("reversed") . toText

-- | The @rows@ attribute.
rows_ :: ToText a => a -> (Text,Text)
rows_ = (,) ("rows") . toText

-- | The @rowspan@ attribute.
rowspan_ :: ToText a => a -> (Text,Text)
rowspan_ = (,) ("rowspan") . toText

-- | The @sandbox@ attribute.
sandbox_ :: ToText a => a -> (Text,Text)
sandbox_ = (,) ("sandbox") . toText

-- | The @scope@ attribute.
scope_ :: ToText a => a -> (Text,Text)
scope_ = (,) ("scope") . toText

-- | The @scoped@ attribute.
scoped_ :: ToText a => a -> (Text,Text)
scoped_ = (,) ("scoped") . toText

-- | The @seamless@ attribute.
seamless_ :: ToText a => a -> (Text,Text)
seamless_ = (,) ("seamless") . toText

-- | The @selected@ attribute.
selected_ :: ToText a => a -> (Text,Text)
selected_ = (,) ("selected") . toText

-- | The @shape@ attribute.
shape_ :: ToText a => a -> (Text,Text)
shape_ = (,) ("shape") . toText

-- | The @size@ attribute.
size_ :: ToText a => a -> (Text,Text)
size_ = (,) ("size") . toText

-- | The @sizes@ attribute.
sizes_ :: ToText a => a -> (Text,Text)
sizes_ = (,) ("sizes") . toText

-- | The @spellcheck@ attribute.
spellcheck_ :: ToText a => a -> (Text,Text)
spellcheck_ = (,) ("spellcheck") . toText

-- | The @src@ attribute.
src_ :: ToText a => a -> (Text,Text)
src_ = (,) ("src") . toText

-- | The @srcdoc@ attribute.
srcdoc_ :: ToText a => a -> (Text,Text)
srcdoc_ = (,) ("srcdoc") . toText

-- | The @start@ attribute.
start_ :: ToText a => a -> (Text,Text)
start_ = (,) ("start") . toText

-- | The @step@ attribute.
step_ :: ToText a => a -> (Text,Text)
step_ = (,) ("step") . toText

-- | The @subject@ attribute.
subject_ :: ToText a => a -> (Text,Text)
subject_ = (,) ("subject") . toText

-- | The @tabindex@ attribute.
tabindex_ :: ToText a => a -> (Text,Text)
tabindex_ = (,) ("tabindex") . toText

-- | The @target@ attribute.
target_ :: ToText a => a -> (Text,Text)
target_ = (,) ("target") . toText

-- | The @type@ attribute.
type_ :: ToText a => a -> (Text,Text)
type_ = (,) ("type") . toText

-- | The @usemap@ attribute.
usemap_ :: ToText a => a -> (Text,Text)
usemap_ = (,) ("usemap") . toText

-- | The @value@ attribute.
value_ :: ToText a => a -> (Text,Text)
value_ = (,) ("value") . toText

-- | The @width@ attribute.
width_ :: ToText a => a -> (Text,Text)
width_ = (,) ("width") . toText

-- | The @wrap@ attribute.
wrap_ :: ToText a => a -> (Text,Text)
wrap_ = (,) ("wrap") . toText

-- | The @xmlns@ attribute.
xmlns_ :: ToText a => a -> (Text,Text)
xmlns_ = (,) ("xmlns") . toText
