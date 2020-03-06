module Paginated where

import           Relude

import           Data.Aeson                     ( (.=)
                                                , ToJSON(..)
                                                , object
                                                )
import           Servant.API                    ( FromHttpApiData(..) )

import qualified Data.Text                     as Text


data SortOrder
  = Descending
  | Ascending
  deriving Show

instance FromHttpApiData SortOrder where
  parseQueryParam sortOrder = case Text.toUpper sortOrder of
    "ASC"        -> Right Ascending
    "ASCENDING"  -> Right Ascending
    "DESC"       -> Right Descending
    "DESCENDING" -> Right Descending
    _            -> Left "Should be asc, ascending, desc, or descending"

data PaginatedRequest = PaginatedRequest
  { pageSize :: !Natural
  , currentPage :: !Natural
  , sortOrder :: !SortOrder
  , sortColumn :: !Text
  } deriving Show

data PaginatedResult a = PaginatedResult
  { rows :: ![a]
  , totalRows :: !Natural
  , totalPages :: !Natural
  } deriving Show

instance ToJSON a => ToJSON (PaginatedResult a) where
  toJSON PaginatedResult{..} = object
    [ "rows"       .= rows
    , "totalRows"  .= totalRows
    , "totalPages" .= totalPages
    ]

type SortBehavior a = SortOrder -> Text -> [a] -> [a]

paginateList :: SortBehavior a -> PaginatedRequest -> [a] -> PaginatedResult a
paginateList sortBehavior PaginatedRequest {..} list = PaginatedResult
  { rows       = rows
  , totalRows  = totalRows
  , totalPages = totalPages
  }
 where
  sortedList = sortBehavior sortOrder sortColumn list
  totalRows  = fromIntegral $ length sortedList
  rows =
    let dropAmount = if currentPage < 0
          then 0
          else fromIntegral . toInteger $ pageSize * currentPage
        takeAmount = fromIntegral . toInteger $ pageSize
    in  take takeAmount . drop dropAmount $ sortedList
  totalPages =
    let totalRows' = fromIntegral totalRows :: Double
        pageSize'  = fromIntegral pageSize :: Double
    in  totalRows' / pageSize' 
          & ceiling 
          & integerToNatural 
          & fromMaybe 0
