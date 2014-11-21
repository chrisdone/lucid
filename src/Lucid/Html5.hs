{-# OPTIONS -fno-warn-type-defaults #-}

-- | Html5 terms.

module Lucid.Html5 where

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
cite_ = mixed (Blaze.fromString "cite")

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
form_ = mixed (Blaze.fromString "form")

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
label_ = mixed (Blaze.fromString "label")

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
script_  :: Monad m => HtmlT m () -> HtmlT m ()
script_ = makeElement (Blaze.fromString "script")

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
span_ = mixed (Blaze.fromString "span")

-- | @strong@ element
strong_  :: Monad m => HtmlT m () -> HtmlT m ()
strong_ = makeElement (Blaze.fromString "strong")

-- | @style@ element or @style@ attribute
style_  :: Mixed a r => a -> r
style_ = mixed (Blaze.fromString "style")

-- | @sub@ element
sub_  :: Monad m => HtmlT m () -> HtmlT m ()
sub_ = makeElement (Blaze.fromString "sub")

-- | @summary@ element or @summary@ attribute
summary_  :: Mixed a r => a -> r
summary_ = mixed (Blaze.fromString "summary")

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
title_ = mixed (Blaze.fromString "title")

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
accept_ :: ToText a => a -> Attr
accept_ = Attr (Blaze.fromString "accept") . toText

-- | The @acceptCharset@ attribute.
acceptCharset_ :: ToText a => a -> Attr
acceptCharset_ = Attr (Blaze.fromString "acceptCharset") . toText

-- | The @accesskey@ attribute.
accesskey_ :: ToText a => a -> Attr
accesskey_ = Attr (Blaze.fromString "accesskey") . toText

-- | The @action@ attribute.
action_ :: ToText a => a -> Attr
action_ = Attr (Blaze.fromString "action") . toText

-- | The @alt@ attribute.
alt_ :: ToText a => a -> Attr
alt_ = Attr (Blaze.fromString "alt") . toText

-- | The @async@ attribute.
async_ :: ToText a => a -> Attr
async_ = Attr (Blaze.fromString "async") . toText

-- | The @autocomplete@ attribute.
autocomplete_ :: ToText a => a -> Attr
autocomplete_ = Attr (Blaze.fromString "autocomplete") . toText

-- | The @autofocus@ attribute.
autofocus_ :: Attr
autofocus_ = Attr (Blaze.fromString "autofocus") mempty

-- | The @autoplay@ attribute.
autoplay_ :: ToText a => a -> Attr
autoplay_ = Attr (Blaze.fromString "autoplay") . toText

-- | The @challenge@ attribute.
challenge_ :: ToText a => a -> Attr
challenge_ = Attr (Blaze.fromString "challenge") . toText

-- | The @charset@ attribute.
charset_ :: ToText a => a -> Attr
charset_ = Attr (Blaze.fromString "charset") . toText

-- | The @checked@ attribute.
checked_ :: Attr
checked_ = Attr (Blaze.fromString "checked") mempty

-- | The @class@ attribute.
class_ :: ToText a => a -> Attr
class_ = Attr (Blaze.fromString "class") . toText

-- | The @cols@ attribute.
cols_ :: ToText a => a -> Attr
cols_ = Attr (Blaze.fromString "cols") . toText

-- | The @colspan@ attribute.
colspan_ :: ToText a => a -> Attr
colspan_ = Attr (Blaze.fromString "colspan") . toText

-- | The @content@ attribute.
content_ :: ToText a => a -> Attr
content_ = Attr (Blaze.fromString "content") . toText

-- | The @contenteditable@ attribute.
contenteditable_ :: ToText a => a -> Attr
contenteditable_ = Attr (Blaze.fromString "contenteditable") . toText

-- | The @contextmenu@ attribute.
contextmenu_ :: ToText a => a -> Attr
contextmenu_ = Attr (Blaze.fromString "contextmenu") . toText

-- | The @controls@ attribute.
controls_ :: ToText a => a -> Attr
controls_ = Attr (Blaze.fromString "controls") . toText

-- | The @coords@ attribute.
coords_ :: ToText a => a -> Attr
coords_ = Attr (Blaze.fromString "coords") . toText

-- | The @data@ attribute.
data_ :: ToText a => a -> Attr
data_ = Attr (Blaze.fromString "data") . toText

-- | The @datetime@ attribute.
datetime_ :: ToText a => a -> Attr
datetime_ = Attr (Blaze.fromString "datetime") . toText

-- | The @defer@ attribute.
defer_ :: ToText a => a -> Attr
defer_ = Attr (Blaze.fromString "defer") . toText

-- | The @dir@ attribute.
dir_ :: ToText a => a -> Attr
dir_ = Attr (Blaze.fromString "dir") . toText

-- | The @disabled@ attribute.
disabled_ :: ToText a => a -> Attr
disabled_ = Attr (Blaze.fromString "disabled") . toText

-- | The @draggable@ attribute.
draggable_ :: ToText a => a -> Attr
draggable_ = Attr (Blaze.fromString "draggable") . toText

-- | The @enctype@ attribute.
enctype_ :: ToText a => a -> Attr
enctype_ = Attr (Blaze.fromString "enctype") . toText

-- | The @for@ attribute.
for_ :: ToText a => a -> Attr
for_ = Attr (Blaze.fromString "for") . toText

-- | The @formaction@ attribute.
formaction_ :: ToText a => a -> Attr
formaction_ = Attr (Blaze.fromString "formaction") . toText

-- | The @formenctype@ attribute.
formenctype_ :: ToText a => a -> Attr
formenctype_ = Attr (Blaze.fromString "formenctype") . toText

-- | The @formmethod@ attribute.
formmethod_ :: ToText a => a -> Attr
formmethod_ = Attr (Blaze.fromString "formmethod") . toText

-- | The @formnovalidate@ attribute.
formnovalidate_ :: ToText a => a -> Attr
formnovalidate_ = Attr (Blaze.fromString "formnovalidate") . toText

-- | The @formtarget@ attribute.
formtarget_ :: ToText a => a -> Attr
formtarget_ = Attr (Blaze.fromString "formtarget") . toText

-- | The @headers@ attribute.
headers_ :: ToText a => a -> Attr
headers_ = Attr (Blaze.fromString "headers") . toText

-- | The @height@ attribute.
height_ :: ToText a => a -> Attr
height_ = Attr (Blaze.fromString "height") . toText

-- | The @hidden@ attribute.
hidden_ :: ToText a => a -> Attr
hidden_ = Attr (Blaze.fromString "hidden") . toText

-- | The @high@ attribute.
high_ :: ToText a => a -> Attr
high_ = Attr (Blaze.fromString "high") . toText

-- | The @href@ attribute.
href_ :: ToText a => a -> Attr
href_ = Attr (Blaze.fromString "href") . toText

-- | The @hreflang@ attribute.
hreflang_ :: ToText a => a -> Attr
hreflang_ = Attr (Blaze.fromString "hreflang") . toText

-- | The @httpEquiv@ attribute.
httpEquiv_ :: ToText a => a -> Attr
httpEquiv_ = Attr (Blaze.fromString "httpEquiv") . toText

-- | The @icon@ attribute.
icon_ :: ToText a => a -> Attr
icon_ = Attr (Blaze.fromString "icon") . toText

-- | The @id@ attribute.
id_ :: ToText a => a -> Attr
id_ = Attr (Blaze.fromString "id") . toText

-- | The @ismap@ attribute.
ismap_ :: ToText a => a -> Attr
ismap_ = Attr (Blaze.fromString "ismap") . toText

-- | The @item@ attribute.
item_ :: ToText a => a -> Attr
item_ = Attr (Blaze.fromString "item") . toText

-- | The @itemprop@ attribute.
itemprop_ :: ToText a => a -> Attr
itemprop_ = Attr (Blaze.fromString "itemprop") . toText

-- | The @keytype@ attribute.
keytype_ :: ToText a => a -> Attr
keytype_ = Attr (Blaze.fromString "keytype") . toText

-- | The @lang@ attribute.
lang_ :: ToText a => a -> Attr
lang_ = Attr (Blaze.fromString "lang") . toText

-- | The @list@ attribute.
list_ :: ToText a => a -> Attr
list_ = Attr (Blaze.fromString "list") . toText

-- | The @loop@ attribute.
loop_ :: ToText a => a -> Attr
loop_ = Attr (Blaze.fromString "loop") . toText

-- | The @low@ attribute.
low_ :: ToText a => a -> Attr
low_ = Attr (Blaze.fromString "low") . toText

-- | The @manifest@ attribute.
manifest_ :: ToText a => a -> Attr
manifest_ = Attr (Blaze.fromString "manifest") . toText

-- | The @max@ attribute.
max_ :: ToText a => a -> Attr
max_ = Attr (Blaze.fromString "max") . toText

-- | The @maxlength@ attribute.
maxlength_ :: ToText a => a -> Attr
maxlength_ = Attr (Blaze.fromString "maxlength") . toText

-- | The @media@ attribute.
media_ :: ToText a => a -> Attr
media_ = Attr (Blaze.fromString "media") . toText

-- | The @method@ attribute.
method_ :: ToText a => a -> Attr
method_ = Attr (Blaze.fromString "method") . toText

-- | The @min@ attribute.
min_ :: ToText a => a -> Attr
min_ = Attr (Blaze.fromString "min") . toText

-- | The @multiple@ attribute.
multiple_ :: ToText a => a -> Attr
multiple_ = Attr (Blaze.fromString "multiple") . toText

-- | The @name@ attribute.
name_ :: ToText a => a -> Attr
name_ = Attr (Blaze.fromString "name") . toText

-- | The @novalidate@ attribute.
novalidate_ :: ToText a => a -> Attr
novalidate_ = Attr (Blaze.fromString "novalidate") . toText

-- | The @onbeforeonload@ attribute.
onbeforeonload_ :: ToText a => a -> Attr
onbeforeonload_ = Attr (Blaze.fromString "onbeforeonload") . toText

-- | The @onbeforeprint@ attribute.
onbeforeprint_ :: ToText a => a -> Attr
onbeforeprint_ = Attr (Blaze.fromString "onbeforeprint") . toText

-- | The @onblur@ attribute.
onblur_ :: ToText a => a -> Attr
onblur_ = Attr (Blaze.fromString "onblur") . toText

-- | The @oncanplay@ attribute.
oncanplay_ :: ToText a => a -> Attr
oncanplay_ = Attr (Blaze.fromString "oncanplay") . toText

-- | The @oncanplaythrough@ attribute.
oncanplaythrough_ :: ToText a => a -> Attr
oncanplaythrough_ = Attr (Blaze.fromString "oncanplaythrough") . toText

-- | The @onchange@ attribute.
onchange_ :: ToText a => a -> Attr
onchange_ = Attr (Blaze.fromString "onchange") . toText

-- | The @onclick@ attribute.
onclick_ :: ToText a => a -> Attr
onclick_ = Attr (Blaze.fromString "onclick") . toText

-- | The @oncontextmenu@ attribute.
oncontextmenu_ :: ToText a => a -> Attr
oncontextmenu_ = Attr (Blaze.fromString "oncontextmenu") . toText

-- | The @ondblclick@ attribute.
ondblclick_ :: ToText a => a -> Attr
ondblclick_ = Attr (Blaze.fromString "ondblclick") . toText

-- | The @ondrag@ attribute.
ondrag_ :: ToText a => a -> Attr
ondrag_ = Attr (Blaze.fromString "ondrag") . toText

-- | The @ondragend@ attribute.
ondragend_ :: ToText a => a -> Attr
ondragend_ = Attr (Blaze.fromString "ondragend") . toText

-- | The @ondragenter@ attribute.
ondragenter_ :: ToText a => a -> Attr
ondragenter_ = Attr (Blaze.fromString "ondragenter") . toText

-- | The @ondragleave@ attribute.
ondragleave_ :: ToText a => a -> Attr
ondragleave_ = Attr (Blaze.fromString "ondragleave") . toText

-- | The @ondragover@ attribute.
ondragover_ :: ToText a => a -> Attr
ondragover_ = Attr (Blaze.fromString "ondragover") . toText

-- | The @ondragstart@ attribute.
ondragstart_ :: ToText a => a -> Attr
ondragstart_ = Attr (Blaze.fromString "ondragstart") . toText

-- | The @ondrop@ attribute.
ondrop_ :: ToText a => a -> Attr
ondrop_ = Attr (Blaze.fromString "ondrop") . toText

-- | The @ondurationchange@ attribute.
ondurationchange_ :: ToText a => a -> Attr
ondurationchange_ = Attr (Blaze.fromString "ondurationchange") . toText

-- | The @onemptied@ attribute.
onemptied_ :: ToText a => a -> Attr
onemptied_ = Attr (Blaze.fromString "onemptied") . toText

-- | The @onended@ attribute.
onended_ :: ToText a => a -> Attr
onended_ = Attr (Blaze.fromString "onended") . toText

-- | The @onerror@ attribute.
onerror_ :: ToText a => a -> Attr
onerror_ = Attr (Blaze.fromString "onerror") . toText

-- | The @onfocus@ attribute.
onfocus_ :: ToText a => a -> Attr
onfocus_ = Attr (Blaze.fromString "onfocus") . toText

-- | The @onformchange@ attribute.
onformchange_ :: ToText a => a -> Attr
onformchange_ = Attr (Blaze.fromString "onformchange") . toText

-- | The @onforminput@ attribute.
onforminput_ :: ToText a => a -> Attr
onforminput_ = Attr (Blaze.fromString "onforminput") . toText

-- | The @onhaschange@ attribute.
onhaschange_ :: ToText a => a -> Attr
onhaschange_ = Attr (Blaze.fromString "onhaschange") . toText

-- | The @oninput@ attribute.
oninput_ :: ToText a => a -> Attr
oninput_ = Attr (Blaze.fromString "oninput") . toText

-- | The @oninvalid@ attribute.
oninvalid_ :: ToText a => a -> Attr
oninvalid_ = Attr (Blaze.fromString "oninvalid") . toText

-- | The @onkeydown@ attribute.
onkeydown_ :: ToText a => a -> Attr
onkeydown_ = Attr (Blaze.fromString "onkeydown") . toText

-- | The @onkeyup@ attribute.
onkeyup_ :: ToText a => a -> Attr
onkeyup_ = Attr (Blaze.fromString "onkeyup") . toText

-- | The @onload@ attribute.
onload_ :: ToText a => a -> Attr
onload_ = Attr (Blaze.fromString "onload") . toText

-- | The @onloadeddata@ attribute.
onloadeddata_ :: ToText a => a -> Attr
onloadeddata_ = Attr (Blaze.fromString "onloadeddata") . toText

-- | The @onloadedmetadata@ attribute.
onloadedmetadata_ :: ToText a => a -> Attr
onloadedmetadata_ = Attr (Blaze.fromString "onloadedmetadata") . toText

-- | The @onloadstart@ attribute.
onloadstart_ :: ToText a => a -> Attr
onloadstart_ = Attr (Blaze.fromString "onloadstart") . toText

-- | The @onmessage@ attribute.
onmessage_ :: ToText a => a -> Attr
onmessage_ = Attr (Blaze.fromString "onmessage") . toText

-- | The @onmousedown@ attribute.
onmousedown_ :: ToText a => a -> Attr
onmousedown_ = Attr (Blaze.fromString "onmousedown") . toText

-- | The @onmousemove@ attribute.
onmousemove_ :: ToText a => a -> Attr
onmousemove_ = Attr (Blaze.fromString "onmousemove") . toText

-- | The @onmouseout@ attribute.
onmouseout_ :: ToText a => a -> Attr
onmouseout_ = Attr (Blaze.fromString "onmouseout") . toText

-- | The @onmouseover@ attribute.
onmouseover_ :: ToText a => a -> Attr
onmouseover_ = Attr (Blaze.fromString "onmouseover") . toText

-- | The @onmouseup@ attribute.
onmouseup_ :: ToText a => a -> Attr
onmouseup_ = Attr (Blaze.fromString "onmouseup") . toText

-- | The @onmousewheel@ attribute.
onmousewheel_ :: ToText a => a -> Attr
onmousewheel_ = Attr (Blaze.fromString "onmousewheel") . toText

-- | The @ononline@ attribute.
ononline_ :: ToText a => a -> Attr
ononline_ = Attr (Blaze.fromString "ononline") . toText

-- | The @onpagehide@ attribute.
onpagehide_ :: ToText a => a -> Attr
onpagehide_ = Attr (Blaze.fromString "onpagehide") . toText

-- | The @onpageshow@ attribute.
onpageshow_ :: ToText a => a -> Attr
onpageshow_ = Attr (Blaze.fromString "onpageshow") . toText

-- | The @onpause@ attribute.
onpause_ :: ToText a => a -> Attr
onpause_ = Attr (Blaze.fromString "onpause") . toText

-- | The @onplay@ attribute.
onplay_ :: ToText a => a -> Attr
onplay_ = Attr (Blaze.fromString "onplay") . toText

-- | The @onplaying@ attribute.
onplaying_ :: ToText a => a -> Attr
onplaying_ = Attr (Blaze.fromString "onplaying") . toText

-- | The @onprogress@ attribute.
onprogress_ :: ToText a => a -> Attr
onprogress_ = Attr (Blaze.fromString "onprogress") . toText

-- | The @onpropstate@ attribute.
onpropstate_ :: ToText a => a -> Attr
onpropstate_ = Attr (Blaze.fromString "onpropstate") . toText

-- | The @onratechange@ attribute.
onratechange_ :: ToText a => a -> Attr
onratechange_ = Attr (Blaze.fromString "onratechange") . toText

-- | The @onreadystatechange@ attribute.
onreadystatechange_ :: ToText a => a -> Attr
onreadystatechange_ = Attr (Blaze.fromString "onreadystatechange") . toText

-- | The @onredo@ attribute.
onredo_ :: ToText a => a -> Attr
onredo_ = Attr (Blaze.fromString "onredo") . toText

-- | The @onresize@ attribute.
onresize_ :: ToText a => a -> Attr
onresize_ = Attr (Blaze.fromString "onresize") . toText

-- | The @onscroll@ attribute.
onscroll_ :: ToText a => a -> Attr
onscroll_ = Attr (Blaze.fromString "onscroll") . toText

-- | The @onseeked@ attribute.
onseeked_ :: ToText a => a -> Attr
onseeked_ = Attr (Blaze.fromString "onseeked") . toText

-- | The @onseeking@ attribute.
onseeking_ :: ToText a => a -> Attr
onseeking_ = Attr (Blaze.fromString "onseeking") . toText

-- | The @onselect@ attribute.
onselect_ :: ToText a => a -> Attr
onselect_ = Attr (Blaze.fromString "onselect") . toText

-- | The @onstalled@ attribute.
onstalled_ :: ToText a => a -> Attr
onstalled_ = Attr (Blaze.fromString "onstalled") . toText

-- | The @onstorage@ attribute.
onstorage_ :: ToText a => a -> Attr
onstorage_ = Attr (Blaze.fromString "onstorage") . toText

-- | The @onsubmit@ attribute.
onsubmit_ :: ToText a => a -> Attr
onsubmit_ = Attr (Blaze.fromString "onsubmit") . toText

-- | The @onsuspend@ attribute.
onsuspend_ :: ToText a => a -> Attr
onsuspend_ = Attr (Blaze.fromString "onsuspend") . toText

-- | The @ontimeupdate@ attribute.
ontimeupdate_ :: ToText a => a -> Attr
ontimeupdate_ = Attr (Blaze.fromString "ontimeupdate") . toText

-- | The @onundo@ attribute.
onundo_ :: ToText a => a -> Attr
onundo_ = Attr (Blaze.fromString "onundo") . toText

-- | The @onunload@ attribute.
onunload_ :: ToText a => a -> Attr
onunload_ = Attr (Blaze.fromString "onunload") . toText

-- | The @onvolumechange@ attribute.
onvolumechange_ :: ToText a => a -> Attr
onvolumechange_ = Attr (Blaze.fromString "onvolumechange") . toText

-- | The @onwaiting@ attribute.
onwaiting_ :: ToText a => a -> Attr
onwaiting_ = Attr (Blaze.fromString "onwaiting") . toText

-- | The @open@ attribute.
open_ :: ToText a => a -> Attr
open_ = Attr (Blaze.fromString "open") . toText

-- | The @optimum@ attribute.
optimum_ :: ToText a => a -> Attr
optimum_ = Attr (Blaze.fromString "optimum") . toText

-- | The @pattern@ attribute.
pattern_ :: ToText a => a -> Attr
pattern_ = Attr (Blaze.fromString "pattern") . toText

-- | The @ping@ attribute.
ping_ :: ToText a => a -> Attr
ping_ = Attr (Blaze.fromString "ping") . toText

-- | The @placeholder@ attribute.
placeholder_ :: ToText a => a -> Attr
placeholder_ = Attr (Blaze.fromString "placeholder") . toText

-- | The @preload@ attribute.
preload_ :: ToText a => a -> Attr
preload_ = Attr (Blaze.fromString "preload") . toText

-- | The @pubdate@ attribute.
pubdate_ :: ToText a => a -> Attr
pubdate_ = Attr (Blaze.fromString "pubdate") . toText

-- | The @radiogroup@ attribute.
radiogroup_ :: ToText a => a -> Attr
radiogroup_ = Attr (Blaze.fromString "radiogroup") . toText

-- | The @readonly@ attribute.
readonly_ :: ToText a => a -> Attr
readonly_ = Attr (Blaze.fromString "readonly") . toText

-- | The @rel@ attribute.
rel_ :: ToText a => a -> Attr
rel_ = Attr (Blaze.fromString "rel") . toText

-- | The @required@ attribute.
required_ :: ToText a => a -> Attr
required_ = Attr (Blaze.fromString "required") . toText

-- | The @reversed@ attribute.
reversed_ :: ToText a => a -> Attr
reversed_ = Attr (Blaze.fromString "reversed") . toText

-- | The @rows@ attribute.
rows_ :: ToText a => a -> Attr
rows_ = Attr (Blaze.fromString "rows") . toText

-- | The @rowspan@ attribute.
rowspan_ :: ToText a => a -> Attr
rowspan_ = Attr (Blaze.fromString "rowspan") . toText

-- | The @sandbox@ attribute.
sandbox_ :: ToText a => a -> Attr
sandbox_ = Attr (Blaze.fromString "sandbox") . toText

-- | The @scope@ attribute.
scope_ :: ToText a => a -> Attr
scope_ = Attr (Blaze.fromString "scope") . toText

-- | The @scoped@ attribute.
scoped_ :: ToText a => a -> Attr
scoped_ = Attr (Blaze.fromString "scoped") . toText

-- | The @seamless@ attribute.
seamless_ :: ToText a => a -> Attr
seamless_ = Attr (Blaze.fromString "seamless") . toText

-- | The @selected@ attribute.
selected_ :: ToText a => a -> Attr
selected_ = Attr (Blaze.fromString "selected") . toText

-- | The @shape@ attribute.
shape_ :: ToText a => a -> Attr
shape_ = Attr (Blaze.fromString "shape") . toText

-- | The @size@ attribute.
size_ :: ToText a => a -> Attr
size_ = Attr (Blaze.fromString "size") . toText

-- | The @sizes@ attribute.
sizes_ :: ToText a => a -> Attr
sizes_ = Attr (Blaze.fromString "sizes") . toText

-- | The @spellcheck@ attribute.
spellcheck_ :: ToText a => a -> Attr
spellcheck_ = Attr (Blaze.fromString "spellcheck") . toText

-- | The @src@ attribute.
src_ :: ToText a => a -> Attr
src_ = Attr (Blaze.fromString "src") . toText

-- | The @srcdoc@ attribute.
srcdoc_ :: ToText a => a -> Attr
srcdoc_ = Attr (Blaze.fromString "srcdoc") . toText

-- | The @start@ attribute.
start_ :: ToText a => a -> Attr
start_ = Attr (Blaze.fromString "start") . toText

-- | The @step@ attribute.
step_ :: ToText a => a -> Attr
step_ = Attr (Blaze.fromString "step") . toText

-- | The @subject@ attribute.
subject_ :: ToText a => a -> Attr
subject_ = Attr (Blaze.fromString "subject") . toText

-- | The @tabindex@ attribute.
tabindex_ :: ToText a => a -> Attr
tabindex_ = Attr (Blaze.fromString "tabindex") . toText

-- | The @target@ attribute.
target_ :: ToText a => a -> Attr
target_ = Attr (Blaze.fromString "target") . toText

-- | The @type@ attribute.
type_ :: ToText a => a -> Attr
type_ = Attr (Blaze.fromString "type") . toText

-- | The @usemap@ attribute.
usemap_ :: ToText a => a -> Attr
usemap_ = Attr (Blaze.fromString "usemap") . toText

-- | The @value@ attribute.
value_ :: ToText a => a -> Attr
value_ = Attr (Blaze.fromString "value") . toText

-- | The @width@ attribute.
width_ :: ToText a => a -> Attr
width_ = Attr (Blaze.fromString "width") . toText

-- | The @wrap@ attribute.
wrap_ :: ToText a => a -> Attr
wrap_ = Attr (Blaze.fromString "wrap") . toText

-- | The @xmlns@ attribute.
xmlns_ :: ToText a => a -> Attr
xmlns_ = Attr (Blaze.fromString "xmlns") . toText
