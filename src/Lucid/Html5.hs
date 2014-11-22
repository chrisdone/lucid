{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-type-defaults #-}

-- | Html5 terms.

module Lucid.Html5 where

import           Data.Text (Text, empty)
import           Lucid.Base

-- | @DOCTYPE@ element
doctype_  :: Monad m => HtmlT m ()
doctype_ = makeElementNoEnd "!DOCTYPE HTML"

-- | @DOCTYPE@ element + @html@ element
doctypehtml_  :: Monad m => HtmlT m () -> HtmlT m ()
doctypehtml_ m =
  do doctype_
     html_ m

-- | @a@ element
a_  :: Monad m => HtmlT m () -> HtmlT m ()
a_ = makeElement "a"

-- | @abbr@ element
abbr_  :: Monad m => HtmlT m () -> HtmlT m ()
abbr_ = makeElement "abbr"

-- | @address@ element
address_  :: Monad m => HtmlT m () -> HtmlT m ()
address_ = makeElement "address"

-- | @area@ element
area_  :: Monad m => HtmlT m ()
area_ = makeElementNoEnd "area"

-- | @article@ element
article_  :: Monad m => HtmlT m () -> HtmlT m ()
article_ = makeElement "article"

-- | @aside@ element
aside_  :: Monad m => HtmlT m () -> HtmlT m ()
aside_ = makeElement "aside"

-- | @audio@ element
audio_  :: Monad m => HtmlT m () -> HtmlT m ()
audio_ = makeElement "audio"

-- | @b@ element
b_  :: Monad m => HtmlT m () -> HtmlT m ()
b_ = makeElement "b"

-- | @base@ element
base_  :: Monad m => HtmlT m ()
base_ = makeElementNoEnd "base"

-- | @bdo@ element
bdo_  :: Monad m => HtmlT m () -> HtmlT m ()
bdo_ = makeElement "bdo"

-- | @blockquote@ element
blockquote_  :: Monad m => HtmlT m () -> HtmlT m ()
blockquote_ = makeElement "blockquote"

-- | @body@ element
body_  :: Monad m => HtmlT m () -> HtmlT m ()
body_ = makeElement "body"

-- | @br@ element
br_  :: Monad m => HtmlT m ()
br_ = makeElementNoEnd "br"

-- | @button@ element
button_  :: Monad m => HtmlT m () -> HtmlT m ()
button_ = makeElement "button"

-- | @canvas@ element
canvas_  :: Monad m => HtmlT m () -> HtmlT m ()
canvas_ = makeElement "canvas"

-- | @caption@ element
caption_  :: Monad m => HtmlT m () -> HtmlT m ()
caption_ = makeElement "caption"

-- | @cite@ element or @cite@ attribute.
cite_ :: Mixed a r => a -> r
cite_ = mixed ("cite")

-- | @code@ element
code_  :: Monad m => HtmlT m () -> HtmlT m ()
code_ = makeElement "code"

-- | @col@ element
col_  :: Monad m => HtmlT m ()
col_ = makeElementNoEnd "col"

-- | @colgroup@ element
colgroup_  :: Monad m => HtmlT m () -> HtmlT m ()
colgroup_ = makeElement "colgroup"

-- | @command@ element
command_  :: Monad m => HtmlT m () -> HtmlT m ()
command_ = makeElement "command"

-- | @datalist@ element
datalist_  :: Monad m => HtmlT m () -> HtmlT m ()
datalist_ = makeElement "datalist"

-- | @dd@ element
dd_  :: Monad m => HtmlT m () -> HtmlT m ()
dd_ = makeElement "dd"

-- | @del@ element
del_  :: Monad m => HtmlT m () -> HtmlT m ()
del_ = makeElement "del"

-- | @details@ element
details_  :: Monad m => HtmlT m () -> HtmlT m ()
details_ = makeElement "details"

-- | @dfn@ element
dfn_  :: Monad m => HtmlT m () -> HtmlT m ()
dfn_ = makeElement "dfn"

-- | @div@ element
div_  :: Monad m => HtmlT m () -> HtmlT m ()
div_ = makeElement "div"

-- | @dl@ element
dl_  :: Monad m => HtmlT m () -> HtmlT m ()
dl_ = makeElement "dl"

-- | @dt@ element
dt_  :: Monad m => HtmlT m () -> HtmlT m ()
dt_ = makeElement "dt"

-- | @em@ element
em_  :: Monad m => HtmlT m () -> HtmlT m ()
em_ = makeElement "em"

-- | @embed@ element
embed_  :: Monad m => HtmlT m ()
embed_ = makeElementNoEnd "embed"

-- | @fieldset@ element
fieldset_  :: Monad m => HtmlT m () -> HtmlT m ()
fieldset_ = makeElement "fieldset"

-- | @figcaption@ element
figcaption_  :: Monad m => HtmlT m () -> HtmlT m ()
figcaption_ = makeElement "figcaption"

-- | @figure@ element
figure_  :: Monad m => HtmlT m () -> HtmlT m ()
figure_ = makeElement "figure"

-- | @footer@ element
footer_  :: Monad m => HtmlT m () -> HtmlT m ()
footer_ = makeElement "footer"

-- | @form@ element or @form@ attribute
form_ :: Mixed a r => a -> r
form_ = mixed ("form")

-- | @h1@ element
h1_  :: Monad m => HtmlT m () -> HtmlT m ()
h1_ = makeElement "h1"

-- | @h2@ element
h2_  :: Monad m => HtmlT m () -> HtmlT m ()
h2_ = makeElement "h2"

-- | @h3@ element
h3_  :: Monad m => HtmlT m () -> HtmlT m ()
h3_ = makeElement "h3"

-- | @h4@ element
h4_  :: Monad m => HtmlT m () -> HtmlT m ()
h4_ = makeElement "h4"

-- | @h5@ element
h5_  :: Monad m => HtmlT m () -> HtmlT m ()
h5_ = makeElement "h5"

-- | @h6@ element
h6_  :: Monad m => HtmlT m () -> HtmlT m ()
h6_ = makeElement "h6"

-- | @head@ element
head_  :: Monad m => HtmlT m () -> HtmlT m ()
head_ = makeElement "head"

-- | @header@ element
header_  :: Monad m => HtmlT m () -> HtmlT m ()
header_ = makeElement "header"

-- | @hgroup@ element
hgroup_  :: Monad m => HtmlT m () -> HtmlT m ()
hgroup_ = makeElement "hgroup"

-- | @hr@ element
hr_  :: Monad m => HtmlT m ()
hr_ = makeElementNoEnd "hr"

-- | @html@ element
html_  :: Monad m => HtmlT m () -> HtmlT m ()
html_ = makeElement "html"

-- | @i@ element
i_  :: Monad m => HtmlT m () -> HtmlT m ()
i_ = makeElement "i"

-- | @iframe@ element
iframe_  :: Monad m => HtmlT m () -> HtmlT m ()
iframe_ = makeElement "iframe"

-- | @img@ element
img_  :: Monad m => HtmlT m ()
img_ = makeElementNoEnd "img"

-- | @input@ element
input_  :: Monad m => HtmlT m ()
input_ = makeElementNoEnd "input"

-- | @ins@ element
ins_  :: Monad m => HtmlT m () -> HtmlT m ()
ins_ = makeElement "ins"

-- | @kbd@ element
kbd_  :: Monad m => HtmlT m () -> HtmlT m ()
kbd_ = makeElement "kbd"

-- | @keygen@ element
keygen_  :: Monad m => HtmlT m ()
keygen_ = makeElementNoEnd "keygen"

-- | @label@ element or @label@ attribute
label_ :: Mixed a r => a -> r
label_ = mixed ("label")

-- | @legend@ element
legend_  :: Monad m => HtmlT m () -> HtmlT m ()
legend_ = makeElement "legend"

-- | @li@ element
li_  :: Monad m => HtmlT m () -> HtmlT m ()
li_ = makeElement "li"

-- | @link@ element
link_  :: Monad m => HtmlT m ()
link_ = makeElementNoEnd "link"

-- | @map@ element
map_  :: Monad m => HtmlT m () -> HtmlT m ()
map_ = makeElement "map"

-- | @mark@ element
mark_  :: Monad m => HtmlT m () -> HtmlT m ()
mark_ = makeElement "mark"

-- | @menu@ element
menu_  :: Monad m => HtmlT m () -> HtmlT m ()
menu_ = makeElement "menu"

-- | @menuitem@ element
menuitem_  :: Monad m => HtmlT m ()
menuitem_ = makeElementNoEnd "menuitem"

-- | @meta@ element
meta_  :: Monad m => HtmlT m ()
meta_ = makeElementNoEnd "meta"

-- | @meter@ element
meter_  :: Monad m => HtmlT m () -> HtmlT m ()
meter_ = makeElement "meter"

-- | @nav@ element
nav_  :: Monad m => HtmlT m () -> HtmlT m ()
nav_ = makeElement "nav"

-- | @noscript@ element
noscript_  :: Monad m => HtmlT m () -> HtmlT m ()
noscript_ = makeElement "noscript"

-- | @object@ element
object_  :: Monad m => HtmlT m () -> HtmlT m ()
object_ = makeElement "object"

-- | @ol@ element
ol_  :: Monad m => HtmlT m () -> HtmlT m ()
ol_ = makeElement "ol"

-- | @optgroup@ element
optgroup_  :: Monad m => HtmlT m () -> HtmlT m ()
optgroup_ = makeElement "optgroup"

-- | @option@ element
option_  :: Monad m => HtmlT m () -> HtmlT m ()
option_ = makeElement "option"

-- | @output@ element
output_  :: Monad m => HtmlT m () -> HtmlT m ()
output_ = makeElement "output"

-- | @p@ element
p_  :: Monad m => HtmlT m () -> HtmlT m ()
p_ = makeElement "p"

-- | @param@ element
param_  :: Monad m => HtmlT m ()
param_ = makeElementNoEnd "param"

-- | @pre@ element
pre_  :: Monad m => HtmlT m () -> HtmlT m ()
pre_ = makeElement "pre"

-- | @progress@ element
progress_  :: Monad m => HtmlT m () -> HtmlT m ()
progress_ = makeElement "progress"

-- | @q@ element
q_  :: Monad m => HtmlT m () -> HtmlT m ()
q_ = makeElement "q"

-- | @rp@ element
rp_  :: Monad m => HtmlT m () -> HtmlT m ()
rp_ = makeElement "rp"

-- | @rt@ element
rt_  :: Monad m => HtmlT m () -> HtmlT m ()
rt_ = makeElement "rt"

-- | @ruby@ element
ruby_  :: Monad m => HtmlT m () -> HtmlT m ()
ruby_ = makeElement "ruby"

-- | @samp@ element
samp_  :: Monad m => HtmlT m () -> HtmlT m ()
samp_ = makeElement "samp"

-- | @script@ element
script_  :: (ToHtml t,Monad m) => t -> HtmlT m ()
script_ = makeElement "script" . toHtmlRaw

-- | @section@ element
section_  :: Monad m => HtmlT m () -> HtmlT m ()
section_ = makeElement "section"

-- | @select@ element
select_  :: Monad m => HtmlT m () -> HtmlT m ()
select_ = makeElement "select"

-- | @small@ element
small_  :: Monad m => HtmlT m () -> HtmlT m ()
small_ = makeElement "small"

-- | @source@ element
source_  :: Monad m => HtmlT m ()
source_ = makeElementNoEnd "source"

-- | @span@ element or @span@ attribute
span_  :: Mixed a r => a -> r
span_ = mixed ("span")

-- | @strong@ element
strong_  :: Monad m => HtmlT m () -> HtmlT m ()
strong_ = makeElement "strong"

-- | @style@ element or @style@ attribute
style_  :: MixedRaw a r => a -> r
style_ = mixedRaw "style"

-- | @sub@ element
sub_  :: Monad m => HtmlT m () -> HtmlT m ()
sub_ = makeElement "sub"

-- | @summary@ element or @summary@ attribute
summary_  :: Mixed a r => a -> r
summary_ = mixed ("summary")

-- | @sup@ element
sup_  :: Monad m => HtmlT m () -> HtmlT m ()
sup_ = makeElement "sup"

-- | @table@ element
table_  :: Monad m => HtmlT m () -> HtmlT m ()
table_ = makeElement "table"

-- | @tbody@ element
tbody_  :: Monad m => HtmlT m () -> HtmlT m ()
tbody_ = makeElement "tbody"

-- | @td@ element
td_  :: Monad m => HtmlT m () -> HtmlT m ()
td_ = makeElement "td"

-- | @textarea@ element
textarea_  :: Monad m => HtmlT m () -> HtmlT m ()
textarea_ = makeElement "textarea"

-- | @tfoot@ element
tfoot_  :: Monad m => HtmlT m () -> HtmlT m ()
tfoot_ = makeElement "tfoot"

-- | @th@ element
th_  :: Monad m => HtmlT m () -> HtmlT m ()
th_ = makeElement "th"

-- | @thead@ element
thead_  :: Monad m => HtmlT m () -> HtmlT m ()
thead_ = makeElement "thead"

-- | @time@ element
time_  :: Monad m => HtmlT m () -> HtmlT m ()
time_ = makeElement "time"

-- | @title@ element or @title@ attribute
title_  :: Mixed a r => a -> r
title_ = mixed ("title")

-- | @tr@ element
tr_  :: Monad m => HtmlT m () -> HtmlT m ()
tr_ = makeElement "tr"

-- | @track@ element
track_  :: Monad m => HtmlT m ()
track_ = makeElementNoEnd "track"

-- | @ul@ element
ul_  :: Monad m => HtmlT m () -> HtmlT m ()
ul_ = makeElement "ul"

-- | @var@ element
var_  :: Monad m => HtmlT m () -> HtmlT m ()
var_ = makeElement "var"

-- | @video@ element
video_  :: Monad m => HtmlT m () -> HtmlT m ()
video_ = makeElement "video"

-- | @wbr@ element
wbr_  :: Monad m => HtmlT m ()
wbr_ = makeElementNoEnd "wbr"

-- | The @accept@ attribute.
accept_ :: Text -> Attribute
accept_ = makeAttribute "accept"

-- | The @acceptCharset@ attribute.
acceptCharset_ :: Text -> Attribute
acceptCharset_ = makeAttribute "acceptCharset"

-- | The @accesskey@ attribute.
accesskey_ :: Text -> Attribute
accesskey_ = makeAttribute "accesskey"

-- | The @action@ attribute.
action_ :: Text -> Attribute
action_ = makeAttribute "action"

-- | The @alt@ attribute.
alt_ :: Text -> Attribute
alt_ = makeAttribute "alt"

-- | The @async@ attribute.
async_ :: Text -> Attribute
async_ = makeAttribute "async"

-- | The @autocomplete@ attribute.
autocomplete_ :: Text -> Attribute
autocomplete_ = makeAttribute "autocomplete"

-- | The @autofocus@ attribute.
autofocus_ :: Attribute
autofocus_ = makeAttribute "autofocus" empty

-- | The @autoplay@ attribute.
autoplay_ :: Text -> Attribute
autoplay_ = makeAttribute "autoplay"

-- | The @challenge@ attribute.
challenge_ :: Text -> Attribute
challenge_ = makeAttribute "challenge"

-- | The @charset@ attribute.
charset_ :: Text -> Attribute
charset_ = makeAttribute "charset"

-- | The @checked@ attribute.
checked_ :: Attribute
checked_ = makeAttribute "checked" empty

-- | The @class@ attribute.
class_ :: Text -> Attribute
class_ = makeAttribute "class"

-- | The @cols@ attribute.
cols_ :: Text -> Attribute
cols_ = makeAttribute "cols"

-- | The @colspan@ attribute.
colspan_ :: Text -> Attribute
colspan_ = makeAttribute "colspan"

-- | The @content@ attribute.
content_ :: Text -> Attribute
content_ = makeAttribute "content"

-- | The @contenteditable@ attribute.
contenteditable_ :: Text -> Attribute
contenteditable_ = makeAttribute "contenteditable"

-- | The @contextmenu@ attribute.
contextmenu_ :: Text -> Attribute
contextmenu_ = makeAttribute "contextmenu"

-- | The @controls@ attribute.
controls_ :: Text -> Attribute
controls_ = makeAttribute "controls"

-- | The @coords@ attribute.
coords_ :: Text -> Attribute
coords_ = makeAttribute "coords"

-- | The @data@ attribute.
data_ :: Text -> Attribute
data_ = makeAttribute "data"

-- | The @datetime@ attribute.
datetime_ :: Text -> Attribute
datetime_ = makeAttribute "datetime"

-- | The @defer@ attribute.
defer_ :: Text -> Attribute
defer_ = makeAttribute "defer"

-- | The @dir@ attribute.
dir_ :: Text -> Attribute
dir_ = makeAttribute "dir"

-- | The @disabled@ attribute.
disabled_ :: Text -> Attribute
disabled_ = makeAttribute "disabled"

-- | The @draggable@ attribute.
draggable_ :: Text -> Attribute
draggable_ = makeAttribute "draggable"

-- | The @enctype@ attribute.
enctype_ :: Text -> Attribute
enctype_ = makeAttribute "enctype"

-- | The @for@ attribute.
for_ :: Text -> Attribute
for_ = makeAttribute "for"

-- | The @formaction@ attribute.
formaction_ :: Text -> Attribute
formaction_ = makeAttribute "formaction"

-- | The @formenctype@ attribute.
formenctype_ :: Text -> Attribute
formenctype_ = makeAttribute "formenctype"

-- | The @formmethod@ attribute.
formmethod_ :: Text -> Attribute
formmethod_ = makeAttribute "formmethod"

-- | The @formnovalidate@ attribute.
formnovalidate_ :: Text -> Attribute
formnovalidate_ = makeAttribute "formnovalidate"

-- | The @formtarget@ attribute.
formtarget_ :: Text -> Attribute
formtarget_ = makeAttribute "formtarget"

-- | The @headers@ attribute.
headers_ :: Text -> Attribute
headers_ = makeAttribute "headers"

-- | The @height@ attribute.
height_ :: Text -> Attribute
height_ = makeAttribute "height"

-- | The @hidden@ attribute.
hidden_ :: Text -> Attribute
hidden_ = makeAttribute "hidden"

-- | The @high@ attribute.
high_ :: Text -> Attribute
high_ = makeAttribute "high"

-- | The @href@ attribute.
href_ :: Text -> Attribute
href_ = makeAttribute "href"

-- | The @hreflang@ attribute.
hreflang_ :: Text -> Attribute
hreflang_ = makeAttribute "hreflang"

-- | The @httpEquiv@ attribute.
httpEquiv_ :: Text -> Attribute
httpEquiv_ = makeAttribute "httpEquiv"

-- | The @icon@ attribute.
icon_ :: Text -> Attribute
icon_ = makeAttribute "icon"

-- | The @id@ attribute.
id_ :: Text -> Attribute
id_ = makeAttribute "id"

-- | The @ismap@ attribute.
ismap_ :: Text -> Attribute
ismap_ = makeAttribute "ismap"

-- | The @item@ attribute.
item_ :: Text -> Attribute
item_ = makeAttribute "item"

-- | The @itemprop@ attribute.
itemprop_ :: Text -> Attribute
itemprop_ = makeAttribute "itemprop"

-- | The @keytype@ attribute.
keytype_ :: Text -> Attribute
keytype_ = makeAttribute "keytype"

-- | The @lang@ attribute.
lang_ :: Text -> Attribute
lang_ = makeAttribute "lang"

-- | The @list@ attribute.
list_ :: Text -> Attribute
list_ = makeAttribute "list"

-- | The @loop@ attribute.
loop_ :: Text -> Attribute
loop_ = makeAttribute "loop"

-- | The @low@ attribute.
low_ :: Text -> Attribute
low_ = makeAttribute "low"

-- | The @manifest@ attribute.
manifest_ :: Text -> Attribute
manifest_ = makeAttribute "manifest"

-- | The @max@ attribute.
max_ :: Text -> Attribute
max_ = makeAttribute "max"

-- | The @maxlength@ attribute.
maxlength_ :: Text -> Attribute
maxlength_ = makeAttribute "maxlength"

-- | The @media@ attribute.
media_ :: Text -> Attribute
media_ = makeAttribute "media"

-- | The @method@ attribute.
method_ :: Text -> Attribute
method_ = makeAttribute "method"

-- | The @min@ attribute.
min_ :: Text -> Attribute
min_ = makeAttribute "min"

-- | The @multiple@ attribute.
multiple_ :: Text -> Attribute
multiple_ = makeAttribute "multiple"

-- | The @name@ attribute.
name_ :: Text -> Attribute
name_ = makeAttribute "name"

-- | The @novalidate@ attribute.
novalidate_ :: Text -> Attribute
novalidate_ = makeAttribute "novalidate"

-- | The @onbeforeonload@ attribute.
onbeforeonload_ :: Text -> Attribute
onbeforeonload_ = makeAttribute "onbeforeonload"

-- | The @onbeforeprint@ attribute.
onbeforeprint_ :: Text -> Attribute
onbeforeprint_ = makeAttribute "onbeforeprint"

-- | The @onblur@ attribute.
onblur_ :: Text -> Attribute
onblur_ = makeAttribute "onblur"

-- | The @oncanplay@ attribute.
oncanplay_ :: Text -> Attribute
oncanplay_ = makeAttribute "oncanplay"

-- | The @oncanplaythrough@ attribute.
oncanplaythrough_ :: Text -> Attribute
oncanplaythrough_ = makeAttribute "oncanplaythrough"

-- | The @onchange@ attribute.
onchange_ :: Text -> Attribute
onchange_ = makeAttribute "onchange"

-- | The @onclick@ attribute.
onclick_ :: Text -> Attribute
onclick_ = makeAttribute "onclick"

-- | The @oncontextmenu@ attribute.
oncontextmenu_ :: Text -> Attribute
oncontextmenu_ = makeAttribute "oncontextmenu"

-- | The @ondblclick@ attribute.
ondblclick_ :: Text -> Attribute
ondblclick_ = makeAttribute "ondblclick"

-- | The @ondrag@ attribute.
ondrag_ :: Text -> Attribute
ondrag_ = makeAttribute "ondrag"

-- | The @ondragend@ attribute.
ondragend_ :: Text -> Attribute
ondragend_ = makeAttribute "ondragend"

-- | The @ondragenter@ attribute.
ondragenter_ :: Text -> Attribute
ondragenter_ = makeAttribute "ondragenter"

-- | The @ondragleave@ attribute.
ondragleave_ :: Text -> Attribute
ondragleave_ = makeAttribute "ondragleave"

-- | The @ondragover@ attribute.
ondragover_ :: Text -> Attribute
ondragover_ = makeAttribute "ondragover"

-- | The @ondragstart@ attribute.
ondragstart_ :: Text -> Attribute
ondragstart_ = makeAttribute "ondragstart"

-- | The @ondrop@ attribute.
ondrop_ :: Text -> Attribute
ondrop_ = makeAttribute "ondrop"

-- | The @ondurationchange@ attribute.
ondurationchange_ :: Text -> Attribute
ondurationchange_ = makeAttribute "ondurationchange"

-- | The @onemptied@ attribute.
onemptied_ :: Text -> Attribute
onemptied_ = makeAttribute "onemptied"

-- | The @onended@ attribute.
onended_ :: Text -> Attribute
onended_ = makeAttribute "onended"

-- | The @onerror@ attribute.
onerror_ :: Text -> Attribute
onerror_ = makeAttribute "onerror"

-- | The @onfocus@ attribute.
onfocus_ :: Text -> Attribute
onfocus_ = makeAttribute "onfocus"

-- | The @onformchange@ attribute.
onformchange_ :: Text -> Attribute
onformchange_ = makeAttribute "onformchange"

-- | The @onforminput@ attribute.
onforminput_ :: Text -> Attribute
onforminput_ = makeAttribute "onforminput"

-- | The @onhaschange@ attribute.
onhaschange_ :: Text -> Attribute
onhaschange_ = makeAttribute "onhaschange"

-- | The @oninput@ attribute.
oninput_ :: Text -> Attribute
oninput_ = makeAttribute "oninput"

-- | The @oninvalid@ attribute.
oninvalid_ :: Text -> Attribute
oninvalid_ = makeAttribute "oninvalid"

-- | The @onkeydown@ attribute.
onkeydown_ :: Text -> Attribute
onkeydown_ = makeAttribute "onkeydown"

-- | The @onkeyup@ attribute.
onkeyup_ :: Text -> Attribute
onkeyup_ = makeAttribute "onkeyup"

-- | The @onload@ attribute.
onload_ :: Text -> Attribute
onload_ = makeAttribute "onload"

-- | The @onloadeddata@ attribute.
onloadeddata_ :: Text -> Attribute
onloadeddata_ = makeAttribute "onloadeddata"

-- | The @onloadedmetadata@ attribute.
onloadedmetadata_ :: Text -> Attribute
onloadedmetadata_ = makeAttribute "onloadedmetadata"

-- | The @onloadstart@ attribute.
onloadstart_ :: Text -> Attribute
onloadstart_ = makeAttribute "onloadstart"

-- | The @onmessage@ attribute.
onmessage_ :: Text -> Attribute
onmessage_ = makeAttribute "onmessage"

-- | The @onmousedown@ attribute.
onmousedown_ :: Text -> Attribute
onmousedown_ = makeAttribute "onmousedown"

-- | The @onmousemove@ attribute.
onmousemove_ :: Text -> Attribute
onmousemove_ = makeAttribute "onmousemove"

-- | The @onmouseout@ attribute.
onmouseout_ :: Text -> Attribute
onmouseout_ = makeAttribute "onmouseout"

-- | The @onmouseover@ attribute.
onmouseover_ :: Text -> Attribute
onmouseover_ = makeAttribute "onmouseover"

-- | The @onmouseup@ attribute.
onmouseup_ :: Text -> Attribute
onmouseup_ = makeAttribute "onmouseup"

-- | The @onmousewheel@ attribute.
onmousewheel_ :: Text -> Attribute
onmousewheel_ = makeAttribute "onmousewheel"

-- | The @ononline@ attribute.
ononline_ :: Text -> Attribute
ononline_ = makeAttribute "ononline"

-- | The @onpagehide@ attribute.
onpagehide_ :: Text -> Attribute
onpagehide_ = makeAttribute "onpagehide"

-- | The @onpageshow@ attribute.
onpageshow_ :: Text -> Attribute
onpageshow_ = makeAttribute "onpageshow"

-- | The @onpause@ attribute.
onpause_ :: Text -> Attribute
onpause_ = makeAttribute "onpause"

-- | The @onplay@ attribute.
onplay_ :: Text -> Attribute
onplay_ = makeAttribute "onplay"

-- | The @onplaying@ attribute.
onplaying_ :: Text -> Attribute
onplaying_ = makeAttribute "onplaying"

-- | The @onprogress@ attribute.
onprogress_ :: Text -> Attribute
onprogress_ = makeAttribute "onprogress"

-- | The @onpropstate@ attribute.
onpropstate_ :: Text -> Attribute
onpropstate_ = makeAttribute "onpropstate"

-- | The @onratechange@ attribute.
onratechange_ :: Text -> Attribute
onratechange_ = makeAttribute "onratechange"

-- | The @onreadystatechange@ attribute.
onreadystatechange_ :: Text -> Attribute
onreadystatechange_ = makeAttribute "onreadystatechange"

-- | The @onredo@ attribute.
onredo_ :: Text -> Attribute
onredo_ = makeAttribute "onredo"

-- | The @onresize@ attribute.
onresize_ :: Text -> Attribute
onresize_ = makeAttribute "onresize"

-- | The @onscroll@ attribute.
onscroll_ :: Text -> Attribute
onscroll_ = makeAttribute "onscroll"

-- | The @onseeked@ attribute.
onseeked_ :: Text -> Attribute
onseeked_ = makeAttribute "onseeked"

-- | The @onseeking@ attribute.
onseeking_ :: Text -> Attribute
onseeking_ = makeAttribute "onseeking"

-- | The @onselect@ attribute.
onselect_ :: Text -> Attribute
onselect_ = makeAttribute "onselect"

-- | The @onstalled@ attribute.
onstalled_ :: Text -> Attribute
onstalled_ = makeAttribute "onstalled"

-- | The @onstorage@ attribute.
onstorage_ :: Text -> Attribute
onstorage_ = makeAttribute "onstorage"

-- | The @onsubmit@ attribute.
onsubmit_ :: Text -> Attribute
onsubmit_ = makeAttribute "onsubmit"

-- | The @onsuspend@ attribute.
onsuspend_ :: Text -> Attribute
onsuspend_ = makeAttribute "onsuspend"

-- | The @ontimeupdate@ attribute.
ontimeupdate_ :: Text -> Attribute
ontimeupdate_ = makeAttribute "ontimeupdate"

-- | The @onundo@ attribute.
onundo_ :: Text -> Attribute
onundo_ = makeAttribute "onundo"

-- | The @onunload@ attribute.
onunload_ :: Text -> Attribute
onunload_ = makeAttribute "onunload"

-- | The @onvolumechange@ attribute.
onvolumechange_ :: Text -> Attribute
onvolumechange_ = makeAttribute "onvolumechange"

-- | The @onwaiting@ attribute.
onwaiting_ :: Text -> Attribute
onwaiting_ = makeAttribute "onwaiting"

-- | The @open@ attribute.
open_ :: Text -> Attribute
open_ = makeAttribute "open"

-- | The @optimum@ attribute.
optimum_ :: Text -> Attribute
optimum_ = makeAttribute "optimum"

-- | The @pattern@ attribute.
pattern_ :: Text -> Attribute
pattern_ = makeAttribute "pattern"

-- | The @ping@ attribute.
ping_ :: Text -> Attribute
ping_ = makeAttribute "ping"

-- | The @placeholder@ attribute.
placeholder_ :: Text -> Attribute
placeholder_ = makeAttribute "placeholder"

-- | The @preload@ attribute.
preload_ :: Text -> Attribute
preload_ = makeAttribute "preload"

-- | The @pubdate@ attribute.
pubdate_ :: Text -> Attribute
pubdate_ = makeAttribute "pubdate"

-- | The @radiogroup@ attribute.
radiogroup_ :: Text -> Attribute
radiogroup_ = makeAttribute "radiogroup"

-- | The @readonly@ attribute.
readonly_ :: Text -> Attribute
readonly_ = makeAttribute "readonly"

-- | The @rel@ attribute.
rel_ :: Text -> Attribute
rel_ = makeAttribute "rel"

-- | The @required@ attribute.
required_ :: Text -> Attribute
required_ = makeAttribute "required"

-- | The @reversed@ attribute.
reversed_ :: Text -> Attribute
reversed_ = makeAttribute "reversed"

-- | The @rows@ attribute.
rows_ :: Text -> Attribute
rows_ = makeAttribute "rows"

-- | The @rowspan@ attribute.
rowspan_ :: Text -> Attribute
rowspan_ = makeAttribute "rowspan"

-- | The @sandbox@ attribute.
sandbox_ :: Text -> Attribute
sandbox_ = makeAttribute "sandbox"

-- | The @scope@ attribute.
scope_ :: Text -> Attribute
scope_ = makeAttribute "scope"

-- | The @scoped@ attribute.
scoped_ :: Text -> Attribute
scoped_ = makeAttribute "scoped"

-- | The @seamless@ attribute.
seamless_ :: Text -> Attribute
seamless_ = makeAttribute "seamless"

-- | The @selected@ attribute.
selected_ :: Text -> Attribute
selected_ = makeAttribute "selected"

-- | The @shape@ attribute.
shape_ :: Text -> Attribute
shape_ = makeAttribute "shape"

-- | The @size@ attribute.
size_ :: Text -> Attribute
size_ = makeAttribute "size"

-- | The @sizes@ attribute.
sizes_ :: Text -> Attribute
sizes_ = makeAttribute "sizes"

-- | The @spellcheck@ attribute.
spellcheck_ :: Text -> Attribute
spellcheck_ = makeAttribute "spellcheck"

-- | The @src@ attribute.
src_ :: Text -> Attribute
src_ = makeAttribute "src"

-- | The @srcdoc@ attribute.
srcdoc_ :: Text -> Attribute
srcdoc_ = makeAttribute "srcdoc"

-- | The @start@ attribute.
start_ :: Text -> Attribute
start_ = makeAttribute "start"

-- | The @step@ attribute.
step_ :: Text -> Attribute
step_ = makeAttribute "step"

-- | The @subject@ attribute.
subject_ :: Text -> Attribute
subject_ = makeAttribute "subject"

-- | The @tabindex@ attribute.
tabindex_ :: Text -> Attribute
tabindex_ = makeAttribute "tabindex"

-- | The @target@ attribute.
target_ :: Text -> Attribute
target_ = makeAttribute "target"

-- | The @type@ attribute.
type_ :: Text -> Attribute
type_ = makeAttribute "type"

-- | The @usemap@ attribute.
usemap_ :: Text -> Attribute
usemap_ = makeAttribute "usemap"

-- | The @value@ attribute.
value_ :: Text -> Attribute
value_ = makeAttribute "value"

-- | The @width@ attribute.
width_ :: Text -> Attribute
width_ = makeAttribute "width"

-- | The @wrap@ attribute.
wrap_ :: Text -> Attribute
wrap_ = makeAttribute "wrap"

-- | The @xmlns@ attribute.
xmlns_ :: Text -> Attribute
xmlns_ = makeAttribute "xmlns"
