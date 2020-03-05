module MovieTable(Slot, component) where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut as J
import Data.Either (hush)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import RemoteTable as R
import URI.Extra.QueryPairs as QP
import URI.Query as Q
import Halogen as H
import Halogen.HTML as HH


type Movie
  = { film :: String
    , genre :: String
    , leadStudio :: String
    , audienceScore :: Int
    , profitability :: Number
    , rottenTomatoesScore :: Int
    , worldwideGross :: Number
    , year :: Int
    }

getMovies :: R.PaginatedRequest -> Aff (Maybe (R.PaginatedResponse Movie))
getMovies request = do
  response <- hush <$> AX.get ResponseFormat.json ("http://localhost:8081/movies" <> queryString)
  pure $ response >>= _.body >>> J.decodeJson >>> hush
 where
  queryString =
    [ Tuple "pageSize" (Just $ show request.pageSize)
    , Tuple "currentPage" (Just $ show request.currentPage)
    , Tuple "sortColumn" (Just request.sortColumn)
    , Tuple "sortOrder" (Just $ case request.sortOrder of
                                  R.Ascending -> "asc"
                                  R.Descending -> "desc")
    ] # QP.QueryPairs
      # QP.print QP.keyFromString QP.valueFromString
      # Q.print

type ChildSlots =
  ( movies :: R.Slot Unit
  )

_movies :: SProxy "movies"
_movies = SProxy

type Slot = H.Slot Identity Void

component :: forall q o i.  H.Component HH.HTML q i o Aff
component = 
  H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }


render :: forall q. q -> H.ComponentHTML q ChildSlots Aff
render _ = HH.slot _movies unit R.component input absurd 
  where input = { getRows: getMovies
                , pageSize: 10
                , sortColumn: "film"
                , sortOrder: R.Descending
                , columns:
                    [ { displayName: "Film"
                      , remoteName: "film"
                      , displayValue: \r -> HH.text r.film
                      }
                    , { displayName: "Genre"
                      , remoteName: "genre"
                      , displayValue: \r -> HH.text r.genre
                      }
                    , { displayName: "Lead Studio"
                      , remoteName: "leadStudio"
                      , displayValue: \r -> HH.text r.leadStudio
                      }
                    , { displayName: "Audience Score"
                      , remoteName: "audienceScore"
                      , displayValue: \r -> HH.text $ show r.audienceScore
                      }
                    , { displayName: "Profitability"
                      , remoteName: "profitability"
                      , displayValue: \r -> HH.text $ show r.profitability
                      }
                    , { displayName: "Rotten Tomatoes Score"
                      , remoteName: "rottenTomatoesScore"
                      , displayValue: \r -> HH.text $ show r.rottenTomatoesScore
                      }
                    , { displayName: "Worldwide Gross"
                      , remoteName: "worldwideGross"
                      , displayValue: \r -> HH.text $ show r.worldwideGross
                      }
                    , { displayName: "Year"
                      , remoteName: "year"
                      , displayValue: \r -> HH.text $ show r.year
                      }
                    ]
                }


