module Plum (Plum, run, currentOrigin) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.URL (URL)
import Data.URL as URL
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Foreign.Object as Object
import Literals.Undefined (undefined)
import Plum.View (GenericUI(..), UI, grow)
import Snabbdom as Snabbdom
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Union (asOneOf)
import Web.DOM.NonElementParentNode (getElementById) as Web
import Web.HTML (window) as Web
import Web.HTML.Event.EventTypes (invalid)
import Web.HTML.HTMLDocument (toNonElementParentNode) as Web
import Web.HTML.Location as Location
import Web.HTML.Window (document, location) as Web

type Plum msg model =
  { init :: (Aff msg -> Effect Unit) -> Effect model
  , update :: (Aff msg -> Effect Unit) -> msg -> model -> Effect model
  , view :: model -> UI msg Unit
  }

run :: forall @model @msg. String -> Plum msg model -> Effect Unit
run id plum = do
  Web.window >>= Web.document >>= Web.getElementById id <<< Web.toNonElementParentNode >>= case _ of
    Just element -> do
      let node = Snabbdom.toVNode element
      modelRef <- Ref.new { model: unsafeCoerce undefined, lastNode: node }
      patch <- Snabbdom.init
      let
        fire =
          ( \msg -> do
              log "in"
              { model: m, lastNode } <- Ref.read modelRef
              m_ <- plum.update
                ( \aff ->
                    launchAff_
                      ( do
                          msg' <- aff
                          liftEffect $ fire msg'
                          pure unit
                      )
                )
                msg
                m
              node' <- view m_
              newNode <- patch lastNode node'
              Ref.write { model: m_, lastNode: newNode } modelRef
              pure unit
          )
        view model = do
          let UI { children } _ = plum.view model
          case Array.uncons children of
            Just { head: child } -> do
              let
                { node, style } = grow
                  fire
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
      initModel <- plum.init
        ( \aff ->
            launchAff_
              ( do
                  msg' <- aff
                  liftEffect $ fire msg'
                  pure unit
              )
        )
      newNode <- view initModel
      newNode_ <- patch node newNode
      log "patched"
      Ref.write { model: initModel, lastNode: newNode_ } modelRef
      pure unit
    Nothing -> pure unit

outerStyle :: String
outerStyle = "html,body{height:100%;padding:0;margin:0;}"

currentOrigin :: Effect URL
currentOrigin = do
  s <- Web.window >>= Web.location >>= Location.origin
  case URL.fromString s of
    Nothing -> throw "window.location.origin returned an invalid url"
    Just url -> pure url
