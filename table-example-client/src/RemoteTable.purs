module RemoteTable 
  ( PaginatedResponse(..)
  , SortOrder(..)
  , PaginatedRequest(..)
  , Column(..)
  , Input(..)
  , Slot
  , component
  ) where

import Prelude
import Control.Monad.Trans.Class (lift)
import DOM.HTML.Indexed.InputType (InputType(..))
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Identity (Identity)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils.Events as Events
import Utils.Css as Css
import Web.UIEvent.MouseEvent (MouseEvent)


type PaginatedResponse row
  = { rows :: Array row
    , totalRows :: Int
    , totalPages :: Int
    }

data SortOrder
  = Ascending
  | Descending

type PaginatedRequest
  = { pageSize :: Int
    , currentPage :: Int
    , sortOrder :: SortOrder
    , sortColumn :: String
    }

type Column row
  = { displayName :: String
    , remoteName :: String
    , displayValue :: row -> HH.PlainHTML
    }

type Slot = H.Slot Identity Void

type Input m row
  = { getRows :: PaginatedRequest -> m (Maybe (PaginatedResponse row))
    , columns :: Array (Column row)
    , pageSize :: Int
    , sortColumn :: String
    , sortOrder :: SortOrder
    }

type State m row
  = { getRows :: PaginatedRequest -> m (Maybe (PaginatedResponse row))
    , response :: Maybe (PaginatedResponse row)
    , columns :: Array (Column row)
    , pageSize :: Int
    , page :: Int
    , sortColumn :: String
    , sortOrder :: SortOrder
    , isRequestActive :: Boolean
    }

data Action
  = Sort String
  | ChangePage MouseEvent Int
  | ChangePageSize Int
  | Refresh

component :: forall row q o m. MonadEffect m => H.Component HH.HTML q (Input m row) o m
component = 
  H.mkComponent 
  { initialState: \input -> 
      { getRows: input.getRows
      , response: Nothing
      , columns: input.columns
      , pageSize: input.pageSize
      , page: 0
      , sortColumn: input.sortColumn
      , sortOrder: input.sortOrder
      , isRequestActive: false
      }
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                   , initialize = Just Refresh
                                   }
  }


handleAction :: forall row o m. MonadEffect m => Action -> H.HalogenM (State m row) Action () o m Unit
handleAction Refresh = do
  oldState <- H.get
  let request = { pageSize: oldState.pageSize
                , currentPage: oldState.page
                , sortOrder: oldState.sortOrder
                , sortColumn: oldState.sortColumn
                }
  H.modify_ (_ { isRequestActive = true })
  response <- lift $ oldState.getRows request
  H.modify_ (_ { response = response, isRequestActive = false })

handleAction (Sort remoteName) = do
  H.modify_ \oldState ->
    if oldState.sortColumn /= remoteName then
      oldState { sortColumn = remoteName, sortOrder = Ascending }
    else
      oldState
        { sortOrder = case oldState.sortOrder of
            Ascending -> Descending
            Descending -> Ascending
        }
  handleAction Refresh

handleAction (ChangePage evt page) = do
  Events.preventDefault evt
  when (page >= 0) $ H.modify_ (_ { page = page })
  handleAction Refresh

handleAction (ChangePageSize pageSize) = do
  when (pageSize >= 1) $ H.modify_ (_ { pageSize = pageSize, page = 0 })
  handleAction Refresh



render :: forall row m. MonadEffect m => State m row -> H.ComponentHTML Action () m
render state =
  HH.div
  [ HP.class_ $ H.ClassName "table-container"
  ]
  [ renderTable state
  , HH.div 
    [ Css.classes [ Css.Always "level" ] ]
    [ renderPageSize state
    , renderPageSelector state
    ]
  ]

renderTable :: forall row m. MonadEffect m => State m row -> H.ComponentHTML Action () m
renderTable state = 
  HH.table
    [ Css.classes 
      [ Css.Always "table"
      , Css.When state.isRequestActive "loading"
      ]
    ]
    [ HH.thead_
      [ HH.tr_ $ state.columns <#> \column ->
          HH.th
          [ HE.onClick $ \_ -> Just $ Sort column.remoteName
          , HP.class_ $ H.ClassName $ 
              case Tuple state.sortColumn state.sortOrder of
                Tuple c Ascending | c == column.remoteName -> "asc"
                Tuple c Descending | c == column.remoteName -> "desc"
                _ -> ""
          ]
          [ HH.text column.displayName ]
      ]
    , HH.tbody_ $ case state.response of
        Nothing -> []
        Just response ->
          response.rows <#> \row ->
            HH.tr_$ state.columns <#> \column ->
              HH.td_
              [ HH.fromPlainHTML $ column.displayValue row ]
    ]

renderPageSize :: forall row m. MonadEffect m => State m row -> H.ComponentHTML Action () m
renderPageSize state =
  HH.input
  [ HP.class_ $ H.ClassName "remote-table-page-size"
  , HP.type_ InputNumber
  , HP.step $ Step 1.0
  , HP.min 1.0
  , HP.value $ show state.pageSize
  , HE.onValueChange $ \newValue -> do
     newPageSize <- Int.fromString newValue
     if newPageSize > 0
       then Just $ ChangePageSize newPageSize
       else Nothing
  ]

renderPageSelector :: forall row m. MonadEffect m => State m row -> H.ComponentHTML Action () m
renderPageSelector state = 
  HH.span 
  [ HP.class_ $ H.ClassName "remote-table-page-selector" ] 
  case state.response of
    Nothing -> []
    Just response -> 
      [ if state.page > 0 
          then 
            HH.a 
            [ HE.onClick $ \evt -> Just $ ChangePage evt $ state.page - 1 
            , HP.href "#"
            , Css.classes [ Css.Always "button is-small"]
            ]
            [ HH.text "Previous" ]
          else
            HH.text ""
      , if state.page < response.totalPages - 1
          then
            HH.a 
            [ HE.onClick $ \evt -> Just $ ChangePage evt $ state.page + 1 
            , HP.href "#"
            , Css.classes [ Css.Always "button is-small"]
            ]
            [ HH.text "Next" ]
          else
            HH.text ""
      ]
