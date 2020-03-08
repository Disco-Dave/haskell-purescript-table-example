module Utils.Css where

import Prelude

import Control.MonadZero (guard)
import Halogen as H
import Halogen.HTML.Properties as HP

data CssClass 
  = Always String
  | When Boolean String

classes :: forall r i. Array CssClass -> HP.IProp (class :: String | r) i
classes classNames = HP.classes $ do
  class_ <- classNames
  case class_ of
    When condition className -> do
      guard condition
      pure $ H.ClassName className
    Always className ->
      pure $ H.ClassName className
