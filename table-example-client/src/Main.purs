module Main(main) where

import Prelude
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import MovieTable as MovieTable
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Trans (runReaderT)


main :: String -> Effect Unit
main apiUrl = do
  HA.runHalogenAff $ do
    body <- HA.awaitBody
    runUI (H.hoist nt mainComponent) unit body
  where 
       nt :: forall a. ReaderT String Aff a -> Aff a
       nt a = runReaderT a apiUrl

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
