module Main where

import Prelude
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import MovieTable as MovieTable

type ChildSlots =
  ( movies :: MovieTable.Slot Unit
  )

_movies :: SProxy "movies"
_movies = SProxy

mainComponent :: forall q i o m. MonadAff m => H.Component HH.HTML q i o m
mainComponent =
  H.mkComponent
    { initialState: const unit
    , eval: H.mkEval H.defaultEval
    , render: render
    }
  where
  render unit = 
    HH.div_
      [ HH.slot _movies unit MovieTable.component unit absurd
      ]

main :: Effect Unit
main =
  HA.runHalogenAff $ do
    body <- HA.awaitBody
    runUI mainComponent unit body
