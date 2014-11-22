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
style_  :: MixedRaw a r => a -> r
style_ = mixedRaw "style"

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
accept_ :: Text -> (Text,Text)
accept_ = (,) ("accept")

-- | The @acceptCharset@ attribute.
acceptCharset_ :: Text -> (Text,Text)
acceptCharset_ = (,) ("acceptCharset")

-- | The @accesskey@ attribute.
accesskey_ :: Text -> (Text,Text)
accesskey_ = (,) ("accesskey")

-- | The @action@ attribute.
action_ :: Text -> (Text,Text)
action_ = (,) ("action")

-- | The @alt@ attribute.
alt_ :: Text -> (Text,Text)
alt_ = (,) ("alt")

-- | The @async@ attribute.
async_ :: Text -> (Text,Text)
async_ = (,) ("async")

-- | The @autocomplete@ attribute.
autocomplete_ :: Text -> (Text,Text)
autocomplete_ = (,) ("autocomplete")

-- | The @autofocus@ attribute.
autofocus_ :: (Text,Text)
autofocus_ = (,) ("autofocus") mempty

-- | The @autoplay@ attribute.
autoplay_ :: Text -> (Text,Text)
autoplay_ = (,) ("autoplay")

-- | The @challenge@ attribute.
challenge_ :: Text -> (Text,Text)
challenge_ = (,) ("challenge")

-- | The @charset@ attribute.
charset_ :: Text -> (Text,Text)
charset_ = (,) ("charset")

-- | The @checked@ attribute.
checked_ :: (Text,Text)
checked_ = (,) ("checked") mempty

-- | The @class@ attribute.
class_ :: Text -> (Text,Text)
class_ = (,) ("class")

-- | The @cols@ attribute.
cols_ :: Text -> (Text,Text)
cols_ = (,) ("cols")

-- | The @colspan@ attribute.
colspan_ :: Text -> (Text,Text)
colspan_ = (,) ("colspan")

-- | The @content@ attribute.
content_ :: Text -> (Text,Text)
content_ = (,) ("content")

-- | The @contenteditable@ attribute.
contenteditable_ :: Text -> (Text,Text)
contenteditable_ = (,) ("contenteditable")

-- | The @contextmenu@ attribute.
contextmenu_ :: Text -> (Text,Text)
contextmenu_ = (,) ("contextmenu")

-- | The @controls@ attribute.
controls_ :: Text -> (Text,Text)
controls_ = (,) ("controls")

-- | The @coords@ attribute.
coords_ :: Text -> (Text,Text)
coords_ = (,) ("coords")

-- | The @data@ attribute.
data_ :: Text -> (Text,Text)
data_ = (,) ("data")

-- | The @datetime@ attribute.
datetime_ :: Text -> (Text,Text)
datetime_ = (,) ("datetime")

-- | The @defer@ attribute.
defer_ :: Text -> (Text,Text)
defer_ = (,) ("defer")

-- | The @dir@ attribute.
dir_ :: Text -> (Text,Text)
dir_ = (,) ("dir")

-- | The @disabled@ attribute.
disabled_ :: Text -> (Text,Text)
disabled_ = (,) ("disabled")

-- | The @draggable@ attribute.
draggable_ :: Text -> (Text,Text)
draggable_ = (,) ("draggable")

-- | The @enctype@ attribute.
enctype_ :: Text -> (Text,Text)
enctype_ = (,) ("enctype")

-- | The @for@ attribute.
for_ :: Text -> (Text,Text)
for_ = (,) ("for")

-- | The @formaction@ attribute.
formaction_ :: Text -> (Text,Text)
formaction_ = (,) ("formaction")

-- | The @formenctype@ attribute.
formenctype_ :: Text -> (Text,Text)
formenctype_ = (,) ("formenctype")

-- | The @formmethod@ attribute.
formmethod_ :: Text -> (Text,Text)
formmethod_ = (,) ("formmethod")

-- | The @formnovalidate@ attribute.
formnovalidate_ :: Text -> (Text,Text)
formnovalidate_ = (,) ("formnovalidate")

-- | The @formtarget@ attribute.
formtarget_ :: Text -> (Text,Text)
formtarget_ = (,) ("formtarget")

-- | The @headers@ attribute.
headers_ :: Text -> (Text,Text)
headers_ = (,) ("headers")

-- | The @height@ attribute.
height_ :: Text -> (Text,Text)
height_ = (,) ("height")

-- | The @hidden@ attribute.
hidden_ :: Text -> (Text,Text)
hidden_ = (,) ("hidden")

-- | The @high@ attribute.
high_ :: Text -> (Text,Text)
high_ = (,) ("high")

-- | The @href@ attribute.
href_ :: Text -> (Text,Text)
href_ = (,) ("href")

-- | The @hreflang@ attribute.
hreflang_ :: Text -> (Text,Text)
hreflang_ = (,) ("hreflang")

-- | The @httpEquiv@ attribute.
httpEquiv_ :: Text -> (Text,Text)
httpEquiv_ = (,) ("httpEquiv")

-- | The @icon@ attribute.
icon_ :: Text -> (Text,Text)
icon_ = (,) ("icon")

-- | The @id@ attribute.
id_ :: Text -> (Text,Text)
id_ = (,) ("id")

-- | The @ismap@ attribute.
ismap_ :: Text -> (Text,Text)
ismap_ = (,) ("ismap")

-- | The @item@ attribute.
item_ :: Text -> (Text,Text)
item_ = (,) ("item")

-- | The @itemprop@ attribute.
itemprop_ :: Text -> (Text,Text)
itemprop_ = (,) ("itemprop")

-- | The @keytype@ attribute.
keytype_ :: Text -> (Text,Text)
keytype_ = (,) ("keytype")

-- | The @lang@ attribute.
lang_ :: Text -> (Text,Text)
lang_ = (,) ("lang")

-- | The @list@ attribute.
list_ :: Text -> (Text,Text)
list_ = (,) ("list")

-- | The @loop@ attribute.
loop_ :: Text -> (Text,Text)
loop_ = (,) ("loop")

-- | The @low@ attribute.
low_ :: Text -> (Text,Text)
low_ = (,) ("low")

-- | The @manifest@ attribute.
manifest_ :: Text -> (Text,Text)
manifest_ = (,) ("manifest")

-- | The @max@ attribute.
max_ :: Text -> (Text,Text)
max_ = (,) ("max")

-- | The @maxlength@ attribute.
maxlength_ :: Text -> (Text,Text)
maxlength_ = (,) ("maxlength")

-- | The @media@ attribute.
media_ :: Text -> (Text,Text)
media_ = (,) ("media")

-- | The @method@ attribute.
method_ :: Text -> (Text,Text)
method_ = (,) ("method")

-- | The @min@ attribute.
min_ :: Text -> (Text,Text)
min_ = (,) ("min")

-- | The @multiple@ attribute.
multiple_ :: Text -> (Text,Text)
multiple_ = (,) ("multiple")

-- | The @name@ attribute.
name_ :: Text -> (Text,Text)
name_ = (,) ("name")

-- | The @novalidate@ attribute.
novalidate_ :: Text -> (Text,Text)
novalidate_ = (,) ("novalidate")

-- | The @onbeforeonload@ attribute.
onbeforeonload_ :: Text -> (Text,Text)
onbeforeonload_ = (,) ("onbeforeonload")

-- | The @onbeforeprint@ attribute.
onbeforeprint_ :: Text -> (Text,Text)
onbeforeprint_ = (,) ("onbeforeprint")

-- | The @onblur@ attribute.
onblur_ :: Text -> (Text,Text)
onblur_ = (,) ("onblur")

-- | The @oncanplay@ attribute.
oncanplay_ :: Text -> (Text,Text)
oncanplay_ = (,) ("oncanplay")

-- | The @oncanplaythrough@ attribute.
oncanplaythrough_ :: Text -> (Text,Text)
oncanplaythrough_ = (,) ("oncanplaythrough")

-- | The @onchange@ attribute.
onchange_ :: Text -> (Text,Text)
onchange_ = (,) ("onchange")

-- | The @onclick@ attribute.
onclick_ :: Text -> (Text,Text)
onclick_ = (,) ("onclick")

-- | The @oncontextmenu@ attribute.
oncontextmenu_ :: Text -> (Text,Text)
oncontextmenu_ = (,) ("oncontextmenu")

-- | The @ondblclick@ attribute.
ondblclick_ :: Text -> (Text,Text)
ondblclick_ = (,) ("ondblclick")

-- | The @ondrag@ attribute.
ondrag_ :: Text -> (Text,Text)
ondrag_ = (,) ("ondrag")

-- | The @ondragend@ attribute.
ondragend_ :: Text -> (Text,Text)
ondragend_ = (,) ("ondragend")

-- | The @ondragenter@ attribute.
ondragenter_ :: Text -> (Text,Text)
ondragenter_ = (,) ("ondragenter")

-- | The @ondragleave@ attribute.
ondragleave_ :: Text -> (Text,Text)
ondragleave_ = (,) ("ondragleave")

-- | The @ondragover@ attribute.
ondragover_ :: Text -> (Text,Text)
ondragover_ = (,) ("ondragover")

-- | The @ondragstart@ attribute.
ondragstart_ :: Text -> (Text,Text)
ondragstart_ = (,) ("ondragstart")

-- | The @ondrop@ attribute.
ondrop_ :: Text -> (Text,Text)
ondrop_ = (,) ("ondrop")

-- | The @ondurationchange@ attribute.
ondurationchange_ :: Text -> (Text,Text)
ondurationchange_ = (,) ("ondurationchange")

-- | The @onemptied@ attribute.
onemptied_ :: Text -> (Text,Text)
onemptied_ = (,) ("onemptied")

-- | The @onended@ attribute.
onended_ :: Text -> (Text,Text)
onended_ = (,) ("onended")

-- | The @onerror@ attribute.
onerror_ :: Text -> (Text,Text)
onerror_ = (,) ("onerror")

-- | The @onfocus@ attribute.
onfocus_ :: Text -> (Text,Text)
onfocus_ = (,) ("onfocus")

-- | The @onformchange@ attribute.
onformchange_ :: Text -> (Text,Text)
onformchange_ = (,) ("onformchange")

-- | The @onforminput@ attribute.
onforminput_ :: Text -> (Text,Text)
onforminput_ = (,) ("onforminput")

-- | The @onhaschange@ attribute.
onhaschange_ :: Text -> (Text,Text)
onhaschange_ = (,) ("onhaschange")

-- | The @oninput@ attribute.
oninput_ :: Text -> (Text,Text)
oninput_ = (,) ("oninput")

-- | The @oninvalid@ attribute.
oninvalid_ :: Text -> (Text,Text)
oninvalid_ = (,) ("oninvalid")

-- | The @onkeydown@ attribute.
onkeydown_ :: Text -> (Text,Text)
onkeydown_ = (,) ("onkeydown")

-- | The @onkeyup@ attribute.
onkeyup_ :: Text -> (Text,Text)
onkeyup_ = (,) ("onkeyup")

-- | The @onload@ attribute.
onload_ :: Text -> (Text,Text)
onload_ = (,) ("onload")

-- | The @onloadeddata@ attribute.
onloadeddata_ :: Text -> (Text,Text)
onloadeddata_ = (,) ("onloadeddata")

-- | The @onloadedmetadata@ attribute.
onloadedmetadata_ :: Text -> (Text,Text)
onloadedmetadata_ = (,) ("onloadedmetadata")

-- | The @onloadstart@ attribute.
onloadstart_ :: Text -> (Text,Text)
onloadstart_ = (,) ("onloadstart")

-- | The @onmessage@ attribute.
onmessage_ :: Text -> (Text,Text)
onmessage_ = (,) ("onmessage")

-- | The @onmousedown@ attribute.
onmousedown_ :: Text -> (Text,Text)
onmousedown_ = (,) ("onmousedown")

-- | The @onmousemove@ attribute.
onmousemove_ :: Text -> (Text,Text)
onmousemove_ = (,) ("onmousemove")

-- | The @onmouseout@ attribute.
onmouseout_ :: Text -> (Text,Text)
onmouseout_ = (,) ("onmouseout")

-- | The @onmouseover@ attribute.
onmouseover_ :: Text -> (Text,Text)
onmouseover_ = (,) ("onmouseover")

-- | The @onmouseup@ attribute.
onmouseup_ :: Text -> (Text,Text)
onmouseup_ = (,) ("onmouseup")

-- | The @onmousewheel@ attribute.
onmousewheel_ :: Text -> (Text,Text)
onmousewheel_ = (,) ("onmousewheel")

-- | The @ononline@ attribute.
ononline_ :: Text -> (Text,Text)
ononline_ = (,) ("ononline")

-- | The @onpagehide@ attribute.
onpagehide_ :: Text -> (Text,Text)
onpagehide_ = (,) ("onpagehide")

-- | The @onpageshow@ attribute.
onpageshow_ :: Text -> (Text,Text)
onpageshow_ = (,) ("onpageshow")

-- | The @onpause@ attribute.
onpause_ :: Text -> (Text,Text)
onpause_ = (,) ("onpause")

-- | The @onplay@ attribute.
onplay_ :: Text -> (Text,Text)
onplay_ = (,) ("onplay")

-- | The @onplaying@ attribute.
onplaying_ :: Text -> (Text,Text)
onplaying_ = (,) ("onplaying")

-- | The @onprogress@ attribute.
onprogress_ :: Text -> (Text,Text)
onprogress_ = (,) ("onprogress")

-- | The @onpropstate@ attribute.
onpropstate_ :: Text -> (Text,Text)
onpropstate_ = (,) ("onpropstate")

-- | The @onratechange@ attribute.
onratechange_ :: Text -> (Text,Text)
onratechange_ = (,) ("onratechange")

-- | The @onreadystatechange@ attribute.
onreadystatechange_ :: Text -> (Text,Text)
onreadystatechange_ = (,) ("onreadystatechange")

-- | The @onredo@ attribute.
onredo_ :: Text -> (Text,Text)
onredo_ = (,) ("onredo")

-- | The @onresize@ attribute.
onresize_ :: Text -> (Text,Text)
onresize_ = (,) ("onresize")

-- | The @onscroll@ attribute.
onscroll_ :: Text -> (Text,Text)
onscroll_ = (,) ("onscroll")

-- | The @onseeked@ attribute.
onseeked_ :: Text -> (Text,Text)
onseeked_ = (,) ("onseeked")

-- | The @onseeking@ attribute.
onseeking_ :: Text -> (Text,Text)
onseeking_ = (,) ("onseeking")

-- | The @onselect@ attribute.
onselect_ :: Text -> (Text,Text)
onselect_ = (,) ("onselect")

-- | The @onstalled@ attribute.
onstalled_ :: Text -> (Text,Text)
onstalled_ = (,) ("onstalled")

-- | The @onstorage@ attribute.
onstorage_ :: Text -> (Text,Text)
onstorage_ = (,) ("onstorage")

-- | The @onsubmit@ attribute.
onsubmit_ :: Text -> (Text,Text)
onsubmit_ = (,) ("onsubmit")

-- | The @onsuspend@ attribute.
onsuspend_ :: Text -> (Text,Text)
onsuspend_ = (,) ("onsuspend")

-- | The @ontimeupdate@ attribute.
ontimeupdate_ :: Text -> (Text,Text)
ontimeupdate_ = (,) ("ontimeupdate")

-- | The @onundo@ attribute.
onundo_ :: Text -> (Text,Text)
onundo_ = (,) ("onundo")

-- | The @onunload@ attribute.
onunload_ :: Text -> (Text,Text)
onunload_ = (,) ("onunload")

-- | The @onvolumechange@ attribute.
onvolumechange_ :: Text -> (Text,Text)
onvolumechange_ = (,) ("onvolumechange")

-- | The @onwaiting@ attribute.
onwaiting_ :: Text -> (Text,Text)
onwaiting_ = (,) ("onwaiting")

-- | The @open@ attribute.
open_ :: Text -> (Text,Text)
open_ = (,) ("open")

-- | The @optimum@ attribute.
optimum_ :: Text -> (Text,Text)
optimum_ = (,) ("optimum")

-- | The @pattern@ attribute.
pattern_ :: Text -> (Text,Text)
pattern_ = (,) ("pattern")

-- | The @ping@ attribute.
ping_ :: Text -> (Text,Text)
ping_ = (,) ("ping")

-- | The @placeholder@ attribute.
placeholder_ :: Text -> (Text,Text)
placeholder_ = (,) ("placeholder")

-- | The @preload@ attribute.
preload_ :: Text -> (Text,Text)
preload_ = (,) ("preload")

-- | The @pubdate@ attribute.
pubdate_ :: Text -> (Text,Text)
pubdate_ = (,) ("pubdate")

-- | The @radiogroup@ attribute.
radiogroup_ :: Text -> (Text,Text)
radiogroup_ = (,) ("radiogroup")

-- | The @readonly@ attribute.
readonly_ :: Text -> (Text,Text)
readonly_ = (,) ("readonly")

-- | The @rel@ attribute.
rel_ :: Text -> (Text,Text)
rel_ = (,) ("rel")

-- | The @required@ attribute.
required_ :: Text -> (Text,Text)
required_ = (,) ("required")

-- | The @reversed@ attribute.
reversed_ :: Text -> (Text,Text)
reversed_ = (,) ("reversed")

-- | The @rows@ attribute.
rows_ :: Text -> (Text,Text)
rows_ = (,) ("rows")

-- | The @rowspan@ attribute.
rowspan_ :: Text -> (Text,Text)
rowspan_ = (,) ("rowspan")

-- | The @sandbox@ attribute.
sandbox_ :: Text -> (Text,Text)
sandbox_ = (,) ("sandbox")

-- | The @scope@ attribute.
scope_ :: Text -> (Text,Text)
scope_ = (,) ("scope")

-- | The @scoped@ attribute.
scoped_ :: Text -> (Text,Text)
scoped_ = (,) ("scoped")

-- | The @seamless@ attribute.
seamless_ :: Text -> (Text,Text)
seamless_ = (,) ("seamless")

-- | The @selected@ attribute.
selected_ :: Text -> (Text,Text)
selected_ = (,) ("selected")

-- | The @shape@ attribute.
shape_ :: Text -> (Text,Text)
shape_ = (,) ("shape")

-- | The @size@ attribute.
size_ :: Text -> (Text,Text)
size_ = (,) ("size")

-- | The @sizes@ attribute.
sizes_ :: Text -> (Text,Text)
sizes_ = (,) ("sizes")

-- | The @spellcheck@ attribute.
spellcheck_ :: Text -> (Text,Text)
spellcheck_ = (,) ("spellcheck")

-- | The @src@ attribute.
src_ :: Text -> (Text,Text)
src_ = (,) ("src")

-- | The @srcdoc@ attribute.
srcdoc_ :: Text -> (Text,Text)
srcdoc_ = (,) ("srcdoc")

-- | The @start@ attribute.
start_ :: Text -> (Text,Text)
start_ = (,) ("start")

-- | The @step@ attribute.
step_ :: Text -> (Text,Text)
step_ = (,) ("step")

-- | The @subject@ attribute.
subject_ :: Text -> (Text,Text)
subject_ = (,) ("subject")

-- | The @tabindex@ attribute.
tabindex_ :: Text -> (Text,Text)
tabindex_ = (,) ("tabindex")

-- | The @target@ attribute.
target_ :: Text -> (Text,Text)
target_ = (,) ("target")

-- | The @type@ attribute.
type_ :: Text -> (Text,Text)
type_ = (,) ("type")

-- | The @usemap@ attribute.
usemap_ :: Text -> (Text,Text)
usemap_ = (,) ("usemap")

-- | The @value@ attribute.
value_ :: Text -> (Text,Text)
value_ = (,) ("value")

-- | The @width@ attribute.
width_ :: Text -> (Text,Text)
width_ = (,) ("width")

-- | The @wrap@ attribute.
wrap_ :: Text -> (Text,Text)
wrap_ = (,) ("wrap")

-- | The @xmlns@ attribute.
xmlns_ :: Text -> (Text,Text)
xmlns_ = (,) ("xmlns")
