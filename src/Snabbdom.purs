module Snabbdom (h, VNode, init, toVNode) where

import Prelude

import Data.Function.Uncurried (Fn4, runFn4)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn2)
import Foreign.Object (Object)
import Literals.Undefined (Undefined)
import Untagged.Union (type (|+|))
import Web.DOM (Element)
import Web.Event.Event (Event)

foreign import data VNode :: Type

foreign import initWithModules :: Effect (EffectFn2 VNode VNode VNode)

init :: Effect (VNode -> VNode -> Effect VNode)
init = initWithModules <#> runEffectFn2

foreign import vnode :: forall props. Fn4 String (Record props) (Array VNode) (String |+| Undefined) VNode

h
  :: String
  -> { attrs :: Object String
     , on :: Object (Event -> Effect Unit)
     , style :: Object String
     , props :: Object String
     }
  -> Array VNode
  -> (String |+| Undefined)
  -> VNode
h tag props children text = runFn4 vnode
  tag
  ( props { on = mkEffectFn1 <$> props.on }
      :: { attrs :: Object String
         , on :: Object (EffectFn1 Event Unit)
         , style :: Object String
         , props :: Object String
         }
  )
  children
  text

foreign import toVNode :: Element -> VNode
