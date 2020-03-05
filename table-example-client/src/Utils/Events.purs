module Utils.Events where

import Prelude
import Effect.Class (class MonadEffect, liftEffect)
import Web.Event.Event as Event
import Web.Event.Internal.Types (Event)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.FocusEvent as FocusEvent
import Web.UIEvent.InputEvent (InputEvent)
import Web.UIEvent.InputEvent as InputEvent
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.UIEvent (UIEvent)
import Web.UIEvent.UIEvent as UIEvent
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent as WheelEvent


class ToEvent e where
  toEvent :: e -> Event

instance mouseEventToEvent :: ToEvent MouseEvent where
  toEvent = MouseEvent.toEvent

instance inputEventToEvent :: ToEvent InputEvent where
  toEvent = InputEvent.toEvent

instance keyboardEventToEvent :: ToEvent KeyboardEvent where
  toEvent = KeyboardEvent.toEvent

instance uiEventToEvent :: ToEvent UIEvent where
  toEvent = UIEvent.toEvent

instance focusEventToEvent :: ToEvent FocusEvent where
  toEvent = FocusEvent.toEvent

instance wheelEventToEvent :: ToEvent WheelEvent where
  toEvent = WheelEvent.toEvent

preventDefault :: forall e m. ToEvent e => MonadEffect m => e -> m Unit
preventDefault = toEvent >>> Event.preventDefault >>> liftEffect
