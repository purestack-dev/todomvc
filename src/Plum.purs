module Plum (Plum, run) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Foreign.Object as Object
import Literals.Undefined (undefined)
import Plum.View (GenericUI(..), UI, grow)
import Snabbdom as Snabbdom
import Untagged.Union (asOneOf)
import Web.DOM.NonElementParentNode (getElementById) as Web
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (toNonElementParentNode) as Web
import Web.HTML.Window (document) as Web

type Plum msg model =
  { init :: Effect model
  , update :: msg -> model -> Effect model
  , view :: model -> UI msg Unit
  }

run :: forall msg model. String -> Plum msg model -> Effect Unit
run id plum = do
  Web.window >>= Web.document >>= Web.getElementById id <<< Web.toNonElementParentNode >>= case _ of
    Just element -> do
      let node = Snabbdom.toVNode element
      patch <- Snabbdom.init
      initModel <- plum.init
      modelRef <- Ref.new { model: initModel, lastNode: node }
      let
        view model = do
          let UI { children } _ = plum.view model
          case Array.uncons children of
            Just { head: child } -> do
              let
                { node, style } = grow
                  ( \msg -> do
                      log "in"
                      { model: m, lastNode } <- Ref.read modelRef
                      m_ <- plum.update msg m
                      node <- view m_
                      newNode <- patch lastNode node
                      Ref.write { model: m_, lastNode: newNode } modelRef
                      pure unit
                  )
                  child
              pure $
                Snabbdom.h "div"
                  { attrs: Object.empty
                  , on: Object.empty
                  , style: Object.empty
                  , props: Object.empty
                  }
                  [ Snabbdom.h "style"
                      { attrs: Object.empty
                      , on: Object.empty
                      , style: Object.empty
                      , props: Object.empty
                      }
                      []
                      (asOneOf $ outerStyle <> style)
                  , node
                  ]
                  (asOneOf undefined)
            Nothing -> pure $
              Snabbdom.h "div"
                { attrs: Object.empty
                , on: Object.empty
                , style: Object.empty
                , props: Object.empty
                }
                []
                (asOneOf undefined)
      newNode <- view initModel
      newNode_ <- patch node newNode
      log "patched"
      Ref.write { model: initModel, lastNode: newNode_ } modelRef
      pure unit
    Nothing -> pure unit

outerStyle :: String
outerStyle = "html,body{height:100%;padding:0;margin:0;}"
