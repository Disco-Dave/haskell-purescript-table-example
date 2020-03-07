module MovieTable(Slot, Movie(..), component, class MoviesGetter, getMovies) where

import Prelude
import Data.Identity (Identity)
import Effect.Class (class MonadEffect)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import RemoteTable as R
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

class Monad m <= MoviesGetter m where
  getMovies :: R.PaginatedRequest -> m (Maybe (R.PaginatedResponse Movie))

type ChildSlots =
  ( movies :: R.Slot Unit
  )

_movies :: SProxy "movies"
_movies = SProxy

type Slot = H.Slot Identity Void

component :: forall q o i m. MonadEffect m => MoviesGetter m => H.Component HH.HTML q i o m
component = 
  H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }


render :: forall q m. MonadEffect m => MoviesGetter m => q -> H.ComponentHTML q ChildSlots m
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


