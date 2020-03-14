module Main where

import Prelude
import AppM as App
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import MovieTable as MovieTable
import Utils.Css as Css


type ChildSlots =
  ( movies :: MovieTable.Slot Unit
  )

_movies :: SProxy "movies"
_movies = SProxy

mainComponent 
  :: forall q i o m
   . MonadEffect m 
  => MovieTable.MoviesGetter m 
  => H.Component HH.HTML q i o m
mainComponent =
  H.mkComponent
    { initialState: const unit
    , eval: H.mkEval H.defaultEval
    , render: render
    }
  where
  render unit = 
    HH.div
      [ Css.classes [ Css.Always "container" ] ]
      [ HH.slot _movies unit MovieTable.component unit absurd
      ]

main :: String -> Effect Unit
main apiUrl = do
  HA.runHalogenAff $ do
    body <- HA.awaitBody
    let nt = App.runAppM apiUrl
    runUI (H.hoist nt mainComponent) unit body
