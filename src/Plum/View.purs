module Plum.View
  ( View
  , Nerve(..)
  , Meat(..)
  , Font
  , BorderStyle(..)
  , Corner(..)
  , Shadow(..)
  , ShadowCfg
  , Side(..)
  , Bones(..)
  , grow
  , Direction(..)
  , Alignment(..)
  , Length(..)
  , Color(..)
  , rgb255
  , rgba255
  , rgb
  , rgba
  , UI
  , GenericUI(..)
  , GenericUIView
  , column
  , row
  , stack
  , text
  , link
  , newTabLink
  , download
  , downloadAs
  , image
  , textInput
  , spacing
  , explain
  , align
  , width
  , height
  , wrapped
  , bgColor
  , padding
  , spread
  , opacity
  , pointer
  , move
  , clip
  , borders
  , border
  , borderStyle
  , roundAll
  , round
  , shadow
  , innerShadow
  , onHover
  , onMouseDown
  , font
  , fontSize
  , fontWeight
  , onClick
  ) where

import Prelude

import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Monad.Writer.Trans (mapWriterT)
import Data.Array (foldMap)
import Data.Array as Array
import Data.Array.NonEmpty (concatMap)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String as String
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Foreign.Object (Object)
import Foreign.Object as Object
import Literals.Undefined (undefined)
import Prim.TypeError (class Warn)
import Prim.TypeError as TypeError
import Snabbdom (VNode)
import Snabbdom as Snabbdom
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Union (asOneOf)
import Web.DOM.Element as Element
import Web.DOM.ParentNode (firstElementChild)
import Web.Event.Event (Event)
import Web.Event.Event as Event
import Web.UIEvent.InputEvent as InputEvent
import Web.HTML.HTMLInputElement as HTMLInputElement

type GenericUIView children msg =
  { nerves :: Array (Nerve msg)
  , meat :: Array Meat
  , hover :: Array Meat
  , down :: Array Meat
  , children :: children
  }

type UIView msg = GenericUIView (Array (View msg)) msg

data GenericUI children msg a = UI (GenericUIView children msg) a

instance (Semigroup children, Semigroup a) => Semigroup (GenericUI children msg a) where
  append (UI v a) (UI v' a') = UI (v <> v') (a <> a')

instance (Monoid children, Monoid a) => Monoid (GenericUI children msg a) where
  mempty = UI mempty mempty

type UI msg a = GenericUI (Array (View msg)) msg a

instance Functor (GenericUI children msg) where
  map f (UI v a) = UI v $ f a

instance Monoid children => Apply (GenericUI children msg) where
  apply (UI views f) (UI views' a) = UI (views <> views') (f a)

instance Monoid children => Applicative (GenericUI children msg) where
  pure = UI mempty

instance Monoid children => Bind (GenericUI children msg) where
  bind (UI views a) f =
    let
      UI views' b = f a
    in
      UI (views <> views') b

instance Monoid children => Monad (GenericUI children msg)

column :: forall msg. UI msg Unit -> UI msg Unit
column (UI { nerves, meat, hover, down, children } _) =
  UI (mempty :: UIView msg) { children = [ { meat, nerves, hover, down } /\ Column children ] } unit

row :: forall msg a. UI msg a -> UI msg a
row (UI { nerves, meat, hover, down, children } a) =
  UI (mempty :: UIView msg) { children = [ { meat, nerves, hover, down } /\ Row children ] } a

stack :: forall msg a. UI msg a -> UI msg a
stack (UI { nerves, meat, hover, down, children } a) =
  UI (mempty :: UIView msg) { children = [ { meat, nerves, hover, down } /\ Stack children ] } a

text :: forall msg. String -> GenericUI Unit msg Unit -> UI msg Unit
text s (UI { nerves, meat, hover, down } a) = UI (mempty :: UIView msg) { children = [ { meat, nerves, hover, down } /\ Text s ] } a

link :: forall msg a. String -> UI msg a -> UI msg a
link url (UI { nerves, meat, hover, down, children } a) = UI
  (mempty :: UIView msg)
    { children =
        [ { meat
          , nerves
          , hover
          , down
          } /\ Link url { newTab: false } (Maybe.fromMaybe none $ Array.head children)
        ]
    }
  a

newTabLink :: forall msg a. String -> UI msg a -> UI msg a
newTabLink url (UI { nerves, meat, hover, down, children } a) = UI
  (mempty :: UIView msg)
    { children =
        [ { meat
          , nerves
          , hover
          , down
          } /\ Link url { newTab: true } (Maybe.fromMaybe none $ Array.head children)
        ]
    }
  a

download :: forall msg a. String -> UI msg a -> UI msg a
download url (UI { nerves, meat, children, down, hover } a) = UI
  (mempty :: UIView msg)
    { children =
        [ { meat
          , nerves
          , down
          , hover
          } /\ Download url { filename: Nothing } (Maybe.fromMaybe none $ Array.head children)
        ]
    }
  a

downloadAs :: forall msg a. String -> { filename :: String } -> UI msg a -> UI msg a
downloadAs url { filename } (UI { nerves, meat, hover, down, children } a) = UI
  (mempty :: UIView msg)
    { children =
        [ { meat
          , nerves
          , hover
          , down
          } /\ Download url { filename: Just filename } (Maybe.fromMaybe none $ Array.head children)
        ]
    }
  a

image :: forall msg a. String -> { description :: String } -> GenericUI Unit msg a -> UI msg a
image url description (UI { nerves, meat, hover, down } a) =
  UI (mempty :: UIView msg) { children = [ { meat, nerves, hover, down } /\ Image url description ] } a

textInput :: forall msg a. { value :: String, onChange :: String -> msg } -> GenericUI Unit msg a -> UI msg a
textInput props (UI { nerves, meat, hover, down } a) =
  UI (mempty :: UIView msg) { children = [ { meat, nerves, hover, down } /\ TextInput props ] } a

string :: String -> Classes -> WiredNerves -> String -> VNode
string elem classes (WiredNerves on) s =
  Snabbdom.h elem
    { attrs: Object.singleton "class" (Array.intercalate " " classes)
    , on
    , style: Object.empty
    , props: Object.empty
    }
    []
    (asOneOf s)

h :: String -> Classes -> WiredNerves -> Array VNode -> VNode
h elem classes (WiredNerves on) children =
  Snabbdom.h elem
    { attrs: Object.singleton "class" (Array.intercalate " " classes)
    , on
    , props: Object.empty
    , style: Object.empty
    }
    children
    (asOneOf undefined)

hWith :: forall props. Homogeneous props String => String -> Record props -> Classes -> WiredNerves -> Array VNode -> VNode
hWith elem props classes (WiredNerves on) children =
  Snabbdom.h elem
    { attrs: Object.singleton "class" (Array.intercalate " " classes)
    , on
    , props: Object.fromHomogeneous props
    , style: Object.empty
    }
    children
    (asOneOf undefined)

data Length = Px Int | Fill | Fit | Max Int Length | Min Int Length

data Alignment = Start | Center | End

data Direction = X | Y

n :: forall ch msg. Monoid ch => Nerve msg -> GenericUI ch msg Unit
n nerve = UI (mempty :: GenericUIView ch msg) { nerves = [ nerve ] } mempty

onClick :: forall ch msg. Monoid ch => msg -> GenericUI ch msg Unit
onClick msg = n $ OnClick msg

m :: forall ch msg. Monoid ch => Meat -> GenericUI ch msg Unit
m meat = UI (mempty :: GenericUIView ch msg) { meat = [ meat ] } mempty

spacing :: forall ch msg. Monoid ch => { x :: Int, y :: Int } -> GenericUI ch msg Unit
spacing { x, y } = m $ Spacing x y

explain :: forall ch msg. Monoid ch => Warn (TypeError.Text "Debug explain present") => GenericUI ch msg Unit
explain = m Explain

align :: forall ch msg. Monoid ch => Direction -> Alignment -> GenericUI ch msg Unit
align dir al = m $ Align dir al

width :: forall ch msg. Monoid ch => Length -> GenericUI ch msg Unit
width l = m $ Width l

height :: forall ch msg. Monoid ch => Length -> GenericUI ch msg Unit
height l = m $ Height l

wrapped :: forall ch msg. Monoid ch => GenericUI ch msg Unit
wrapped = m Wrapped

bgColor :: forall ch msg. Monoid ch => Color -> GenericUI ch msg Unit
bgColor c = m $ BackgroundColor c

padding :: forall ch msg. Monoid ch => Side -> Int -> GenericUI ch msg Unit
padding s l = m $ Padding s l

spread :: forall ch msg. Monoid ch => GenericUI ch msg Unit
spread = m Spread

opacity :: forall ch msg. Monoid ch => Number -> GenericUI ch msg Unit
opacity o = m $ Opacity o

pointer :: forall ch msg. Monoid ch => GenericUI ch msg Unit
pointer = m Pointer

move :: forall ch msg. Monoid ch => { x :: Int, y :: Int } -> GenericUI ch msg Unit
move a = m $ Move a

clip :: forall ch msg. Monoid ch => Direction -> GenericUI ch msg Unit
clip dir = m $ Clip dir

onHover :: forall ch msg. Monoid ch => GenericUI Unit msg Unit -> GenericUI ch msg Unit
onHover (UI { meat } a) = UI (mempty :: GenericUIView ch msg) { hover = meat } a

onMouseDown :: forall ch msg. Monoid ch => GenericUI Unit msg Unit -> GenericUI ch msg Unit
onMouseDown (UI { meat } a) = UI (mempty :: GenericUIView ch msg) { down = meat } a

borders :: forall ch msg. Monoid ch => Int -> GenericUI ch msg Unit
borders l = Array.foldMap (\s -> m $ BorderWidth s l) [ Top, Right, Bottom, Left ]

border :: forall ch msg. Monoid ch => Side -> Int -> GenericUI ch msg Unit
border s l = m $ BorderWidth s l

borderStyle :: forall ch msg. Monoid ch => BorderStyle -> GenericUI ch msg Unit
borderStyle s = m $ BorderStyle s

roundAll :: forall ch msg. Monoid ch => Int -> GenericUI ch msg Unit
roundAll l = Array.foldMap (\c -> m $ Round c l) [ TopLeft, TopRight, BottomRight, BottomLeft ]

round :: forall ch msg. Monoid ch => Corner -> Int -> GenericUI ch msg Unit
round c l = m $ Round c l

shadow :: forall ch msg. Monoid ch => ShadowCfg -> GenericUI ch msg Unit
shadow cfg = m $ Shadow $ ShadowCfg cfg

innerShadow :: forall ch msg. Monoid ch => ShadowCfg -> GenericUI ch msg Unit
innerShadow cfg = m $ InnerShadow $ ShadowCfg cfg

font :: forall ch msg. Monoid ch => String -> GenericUI ch msg Unit
font f = m $ Font f

fontSize :: forall ch msg. Monoid ch => Int -> GenericUI ch msg Unit
fontSize f = m $ FontSize f

fontWeight :: forall ch msg. Monoid ch => Int -> GenericUI ch msg Unit
fontWeight f = m $ FontWeight f

data Meat
  = Spacing Int Int
  | Explain
  | Align Direction Alignment
  | Width Length
  | Height Length
  | Wrapped
  | BackgroundColor Color
  | Padding Side Int
  | Spread
  | Opacity Number
  | Pointer
  | Move { x :: Int, y :: Int }
  | Clip Direction
  | BorderWidth Side Int
  | BorderStyle BorderStyle
  | Round Corner Int
  | Shadow Shadow
  | InnerShadow Shadow
  | FontColor Color
  | FontWeight Int
  | FontSize Int
  | Font Font

type Font = String

type ShadowCfg = { offset :: { x :: Int, y :: Int }, size :: Number, blur :: Number, color :: Color }

newtype Shadow = ShadowCfg ShadowCfg

instance Renderable Shadow where
  render (ShadowCfg s) = show s.offset.x <> "px " <> show s.offset.y <> "px " <> show s.blur <> "px " <> show s.size <> "px " <> render s.color
  renderKey (ShadowCfg s) = show s.offset.x <> "-" <> show s.offset.y <> "-" <> show s.blur <> "-" <> show s.size <> "-" <> renderKey s.color

data Corner = TopLeft | TopRight | BottomRight | BottomLeft

instance Renderable Corner where
  render = case _ of
    TopLeft -> "top-left"
    TopRight -> "top-right"
    BottomRight -> "bottom-right"
    BottomLeft -> "bottom-left"
  renderKey = case _ of
    TopLeft -> "top-left"
    TopRight -> "top-right"
    BottomRight -> "bottom-right"
    BottomLeft -> "bottom-left"

data BorderStyle = Solid | Dashed | Dotted

instance Renderable BorderStyle where
  render = case _ of
    Solid -> "solid"
    Dashed -> "dashed"
    Dotted -> "dotted"
  renderKey = case _ of
    Solid -> "solid"
    Dashed -> "dashed"
    Dotted -> "dotted"

data Side = Top | Right | Bottom | Left

instance Renderable Side where
  render = case _ of
    Top -> "top"
    Right -> "right"
    Bottom -> "bottom"
    Left -> "left"
  renderKey = case _ of
    Top -> "top"
    Right -> "right"
    Bottom -> "bottom"
    Left -> "left"

data Nerve msg = OnClick msg

newtype WiredNerves = WiredNerves (Object (Event -> Effect Unit))

instance Semigroup WiredNerves where
  append (WiredNerves x) (WiredNerves y) =
    WiredNerves $ Object.unionWith (\f g ev -> f ev *> g ev) x y

instance Monoid WiredNerves where
  mempty = WiredNerves Object.empty

wireNerve :: forall msg. (msg -> Effect Unit) -> Nerve msg -> WiredNerves
wireNerve fire nerve = WiredNerves $ case nerve of
  OnClick msg -> Object.singleton "click" $ \_ev -> fire msg

type View msg =
  { nerves :: Array (Nerve msg)
  , meat :: Array Meat
  , hover :: Array Meat
  , down :: Array Meat
  } /\ Bones msg

none :: forall msg. View msg
none = mempty /\ None

newtype Skin = Skin (Object String)

sk :: String -> String -> String -> Writer SkinGrowth Classes
sk cName key value = do
  tell $ SkinGrowth $ { meat: Object.singleton cName { key, value }, hover: Object.empty, down: Object.empty }
  pure [ cName ]

instance Semigroup Skin where
  append (Skin x) (Skin y) = Skin $ Object.union x y

instance Monoid Skin where
  mempty = Skin Object.empty

data Context = Grid | Flexbox Direction | Generic | Span

skin :: Context -> Meat -> Writer SkinGrowth Classes
skin ctx = case _ of
  Spacing x y -> sk ("spacing-" <> show x <> "-" <> show y) "gap" (show x <> "px " <> show y <> "px")
  Explain -> sk "explain" "border" "dashed magenta"
  Align dir alignment -> case ctx of
    Generic -> mempty
    Span -> mempty
    Grid ->
      let
        k =
          ( case dir of
              X -> "justify-items"
              Y -> "align-items"
          )
        v =
          ( case alignment of
              Start -> "start"
              Center -> "center"
              End -> "end"
          )
      in
        sk ("align-grid-" <> k <> "-" <> v) k v

    Flexbox flexDir ->
      let
        k =
          ( case flexDir /\ dir of
              X /\ X -> "justify-content"
              X /\ Y -> "align-items"
              Y /\ X -> "align-items"
              Y /\ Y -> "justify-content"
          )

        v =
          ( case alignment of
              Start -> "flex-start"
              Center -> "center"
              End -> "flex-end"
          )
      in
        sk ("align-flexbox-" <> k <> "-" <> v) k v
  Width l -> sk ("width-" <> renderKey l) "width" (render l)
  Height l -> sk ("height-" <> renderKey l) "height" (render l)
  Wrapped -> case ctx of
    Flexbox _ -> sk "wrapped" "flex-wrap" "wrap"
    _ -> pure []
  BackgroundColor color -> sk ("bg-color-" <> renderKey color) "background-color" (render color)
  Padding side l ->
    sk
      ("padding-" <> renderKey side <> "-" <> show l)
      ("padding-" <> render side)
      (show l <> "px")
  Spread -> case ctx of
    Flexbox _ -> sk "spread" "justify-content" "space-between"
    _ -> pure []
  Opacity o -> sk ("opacity-" <> show o) "opacity" (show o)
  Pointer -> sk "pointer" "cursor" "pointer"
  Move { x, y } -> sk ("move-" <> show x <> "-" <> show y) "transform" ("translate(" <> show x <> "px, " <> show y <> "px)")
  Clip X -> sk "clip-x" "overflow-x" "hidden"
  Clip Y -> sk "clip-y" "overflow-y" "hidden"
  BorderWidth side l -> sk ("border-width-" <> renderKey side <> "-" <> show l) ("border-" <> render side <> "-width") (show l <> "px")
  BorderStyle s -> sk ("border-style-" <> renderKey s) "border-style" (render s)
  Round corner l -> sk ("round-" <> renderKey corner <> "-" <> show l) ("border-" <> render corner <> "-radius") (show l <> "px")
  Shadow s -> sk ("shadow-" <> renderKey s) "box-shadow" (render s)
  InnerShadow s -> sk ("inner-shadow-" <> renderKey s) "box-shadow" (render s <> " inset")
  FontColor c -> sk ("font-color-" <> renderKey c) "color" (render c)
  FontSize l -> sk ("font-size-" <> show l) "font-size" (show l <> "px")
  Font f -> sk ("font-" <> removeSpaces f) "font-family" f
  FontWeight w -> sk ("font-weight-" <> show w) "font-weight" (show w)

removeSpaces :: String -> String
removeSpaces = String.replaceAll (String.Pattern " ") (String.Replacement "-")

class Renderable x where
  render :: x -> String
  renderKey :: x -> String

instance Renderable Length where
  render = case _ of
    Px px -> show px <> "px"
    Fit -> "fit-content"
    Fill -> "100%"
    Max px l -> "max(" <> show px <> "px," <> render l <> ")"
    Min px l -> "min(" <> show px <> "px," <> render l <> ")"
  renderKey = case _ of
    Px px -> show px
    Fit -> "fit-content"
    Fill -> "fill"
    Max px l -> "max-" <> show px <> "-" <> renderKey l
    Min px l -> "min-" <> show px <> "-" <> renderKey l

newtype Color = Color { r :: Int, g :: Int, b :: Int, a :: Int }

rgb255 :: Int -> Int -> Int -> Color
rgb255 r g b = Color { r, g, b, a: 100 }

rgba255 :: Int -> Int -> Int -> Int -> Color
rgba255 r g b a = Color { r, g, b, a }

rgb :: Number -> Number -> Number -> Color
rgb r g b = Color { r: Int.round (r * 255.0), g: Int.round (g * 255.0), b: Int.round (b * 255.0), a: 100 }

rgba :: Number -> Number -> Number -> Number -> Color
rgba r g b a = Color { r: Int.round (r * 255.0), g: Int.round (g * 255.0), b: Int.round (b * 255.0), a: Int.round (a * 100.0) }

instance Renderable Color where
  render (Color { r, g, b, a }) = "rgb(" <> show r <> "," <> show g <> "," <> show b <> "," <> show a <> "%)"
  renderKey (Color { r, g, b, a }) = show r <> "-" <> show g <> "-" <> show b <> "-" <> show a

data Bones :: Type -> Type
data Bones msg
  = Column (Array (View msg))
  | Row (Array (View msg))
  | Stack (Array (View msg))
  | Wrapper (View msg)
  | Text String
  | Link String { newTab :: Boolean } (View msg)
  | Download String { filename :: Maybe String } (View msg)
  | Image String { description :: String }
  | TextInput { value :: String, onChange :: String -> msg }
  | None

type Classes = Array String

newtype Mutation = Mutation { extraSkin :: Classes }

instance Semigroup Mutation where
  append (Mutation x) (Mutation y) = Mutation { extraSkin: x.extraSkin <> y.extraSkin }

instance Monoid Mutation where
  mempty = Mutation { extraSkin: mempty }

newtype SkinGrowth = SkinGrowth
  { meat :: Object { key :: String, value :: String }
  , hover :: Object { key :: String, value :: String }
  , down :: Object { key :: String, value :: String }
  }

styleSkinGrowth :: SkinGrowth -> String
styleSkinGrowth (SkinGrowth o) =
  Object.foldMap (\className { key, value } -> "." <> className <> "{" <> key <> ":" <> value <> ";}") o.meat
    <> Object.foldMap (\className { key, value } -> "." <> className <> "{" <> key <> ":" <> value <> ";}") o.hover
    <> Object.foldMap (\className { key, value } -> "." <> className <> "{" <> key <> ":" <> value <> ";}") o.down

instance Semigroup SkinGrowth where
  append (SkinGrowth x) (SkinGrowth y) = SkinGrowth $
    { meat: Object.union x.meat y.meat
    , hover: Object.union x.hover y.hover
    , down: Object.union x.down y.down
    }

instance Monoid SkinGrowth where
  mempty = SkinGrowth mempty

growSkin :: forall msg. (msg -> Effect Unit) -> Mutation -> View msg -> Writer SkinGrowth VNode
growSkin fire (Mutation mutation) ({ meat, hover, down, nerves } /\ bones) = do
  let
    wiredNerves = Array.foldMap (wireNerve fire) nerves
    alignCenter = [ Align X Center, Align Y Center ]
    widthFill = [ Width Fill, Height Fill ]
    skn ctx = do
      meatClasses <- ((_ <> mutation.extraSkin) <<< Array.concat) <$> traverse (skin ctx)
        ( meat
            <> case ctx of
              Span -> mempty
              Grid -> widthFill
              Flexbox _ -> widthFill
              Generic -> widthFill
            <> case ctx of
              Grid -> alignCenter
              Flexbox _ -> alignCenter
              Span -> mempty
              Generic -> alignCenter
        )
      let hoverClasses /\ (SkinGrowth hover') = runWriter $ traverse (skin ctx) hover
      _ <- traverseWithIndex
        ( \key val -> tell $ SkinGrowth $
            { meat: Object.empty
            , hover: Object.singleton (key <> "-hover:hover") val
            , down: Object.empty
            }
        )
        hover'.meat
      let downClasses /\ (SkinGrowth down') = runWriter $ traverse (skin ctx) down
      _ <- traverseWithIndex
        ( \key val -> tell $ SkinGrowth $
            { meat: Object.empty
            , hover: Object.empty
            , down: Object.singleton (key <> "-down:active") val
            }
        )
        down'.meat

      pure $ meatClasses <> map (_ <> "-hover") (Array.concat hoverClasses) <> map (_ <> "-down") (Array.concat downClasses)

  case bones of
    Text t -> (\c -> string "span" c wiredNerves t) <$> (skn Span)
    Wrapper x -> do
      classes <- (skn (Flexbox X) <> sk "display-flex" "display" "flex" <> sk "flex-direction-row" "flex-direction" "row")
      x' <- growSkin fire mempty x
      pure $ h "div" classes wiredNerves [ x' ]
    Stack children -> do
      classes <- (skn Grid <> sk "display-grid" "display" "grid")
      extraSkin <- sk "grid-row-1" "grid-row" "1" <> sk "grid-column-1" "grid-column" "1"
      children' <- (traverse (growSkin fire $ Mutation { extraSkin }) children)
      pure $ h "div" classes wiredNerves children'

    Row children -> do
      classes <- (skn (Flexbox X) <> sk "display-flex" "display" "flex" <> sk "flex-direction-row" "flex-direction" "row")
      children' <- traverse (growSkin fire mempty) children
      pure $ h "div" classes wiredNerves children'
    Column children -> do
      classes <- (skn (Flexbox Y) <> sk "display-flex" "display" "flex" <> sk "flex-direction-column" "flex-direction" "column")
      children' <- traverse (growSkin fire mempty) children
      pure $ h "div" classes wiredNerves children'
    Link href { newTab } child -> do
      classes <- (skn (Flexbox X) <> sk "display-flex" "display" "flex" <> sk "flex-direction-row" "flex-direction" "row")
      child' <- growSkin fire mempty child
      pure $ hWith "a"
        { href
        , rel: "noopener noreferrer"
        , target: if newTab then "_blank" else "_self"
        }
        classes
        wiredNerves
        [ child' ]

    Download href { filename } child -> do
      classes <- (skn (Flexbox X) <> sk "display-flex" "display" "flex" <> sk "flex-direction-row" "flex-direction" "row")
      child' <- growSkin fire mempty child
      pure $ hWith "a"
        { href
        , download: Maybe.fromMaybe "" filename
        }
        classes
        wiredNerves
        [ child' ]
    Image src { description } -> (\c -> hWith "img" { src, alt: description } c wiredNerves []) <$> (skn Generic)
    TextInput { value, onChange } -> do
      classes <- skn Generic
      let
        onInputNerve = WiredNerves $ Object.singleton "input" $ \ev ->
          case Event.currentTarget ev >>= HTMLInputElement.fromEventTarget of
            Nothing -> pure unit
            Just inputEl -> do
              val <- HTMLInputElement.value inputEl
              fire $ onChange val
      pure $ hWith "input" { value } classes (wiredNerves <> onInputNerve) []
    None -> pure $ h "div" mempty wiredNerves []

grow :: forall msg. (msg -> Effect Unit) -> View msg -> { node :: VNode, style :: String }
grow fire v =
  let
    node /\ skinGrowth = runWriter (growSkin fire mempty v)
  in
    { node, style: styleSkinGrowth skinGrowth }

