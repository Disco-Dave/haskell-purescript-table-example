module Main where

import Prelude
import Effect (Effect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

mainComponent :: forall q i o m. H.Component HH.HTML q i o m
mainComponent =
  H.mkComponent
    { initialState: const unit
    , eval: H.mkEval H.defaultEval
    , render: render
    }
  where
  render unit = 
    HH.div_
      [ HH.h1_ [ HH.text "Some heading" ]
      , HH.text "Hello from Halogen :)" 
      , HH.p_ [ HH.text "Some paragraph element." ]
      ]

main :: Effect Unit
main =
  HA.runHalogenAff $ do
    body <- HA.awaitBody
    runUI mainComponent unit body
