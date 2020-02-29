module RemoteTable where

import Prelude
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH

import Halogen.HTML.Events as HE
{--import Halogen.HTML.Properties as HP--}

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
    , displayValue :: row -> String
    }

type Input row
  = { getRows :: PaginatedRequest -> Aff (PaginatedResponse row)
    , columns :: Array (Column row)
    , pageSize :: Int
    , sortColumn :: String
    , sortOrder :: SortOrder
    }

type State row
  = { getRows :: PaginatedRequest -> Aff (PaginatedResponse row)
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
  | ChangePage Int
  | ChangePageSize Int
  | Refresh

component :: forall row q o m. MonadAff m => H.Component HH.HTML q (Input row) o m
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
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

handleAction :: forall row o m. MonadAff m => Action -> H.HalogenM (State row) Action () o m Unit
handleAction = case _ of
  Sort remoteName -> do
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
  ChangePage page -> do
    when (page >= 0)
      $ H.modify_ (_ { page = page })
    handleAction Refresh
  ChangePageSize pageSize -> do
    when (pageSize >= 1)
      $ H.modify_ (_ { pageSize = pageSize })
    handleAction Refresh
  Refresh -> do
    oldState <- H.get
    let
      request =
        { pageSize: oldState.pageSize
        , currentPage: oldState.page
        , sortOrder: oldState.sortOrder
        , sortColumn: oldState.sortColumn
        }
    H.modify_ (_ { isRequestActive = true })
    response <- liftAff $ oldState.getRows request
    H.modify_ (_ { response = Just response, isRequestActive = false })

render :: forall row m. State row -> H.ComponentHTML Action () m
render state =
  HH.table_
    [ HH.thead_
        [ HH.tr_ $ map makeHeader state.columns
        ]
    , HH.tbody_ case state.response of
        Nothing -> []
        Just paginatedResponse -> map makeRow paginatedResponse.rows
    ]
  where
    makeHeader c = HH.th [ HE.onClick $ const (Just $ Sort c.remoteName) ] [ HH.text c.displayName ]
    makeRow row = HH.tr_ $ map (\c -> HH.td_ [ HH.text $ c.displayValue row ]) state.columns
