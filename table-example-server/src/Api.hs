{-# LANGUAGE TypeApplications #-}

module Api
  ( DbEnv
  , makeDbEnv
  , makeApplication
  )
where

import           Relude

import           Movie                          ( Movie )
import           Paginated

import           Servant.API
import           Servant.Server

import qualified Movie


newtype DbEnv = DbEnv { movies :: [Movie] }
newtype AppM a = AppM { fromAppM :: ReaderT DbEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader DbEnv)

makeDbEnv :: MonadIO m => m DbEnv
makeDbEnv = fmap DbEnv Movie.getMovies

runAppM :: MonadIO m => DbEnv -> AppM a -> m a
runAppM dbEnv app = liftIO $ runReaderT (fromAppM app) dbEnv


type Api = "movies" 
  :> QueryParam' '[Required] "pageSize" Natural
  :> QueryParam' '[Required] "currentPage" Natural
  :> QueryParam' '[Required] "sortOrder" SortOrder
  :> QueryParam' '[Required] "sortColumn" Text
  :> Get '[JSON] (PaginatedResult Movie)

server :: ServerT Api AppM
server pageSize currentPage sortOrder sortColumn = do
  movies <- asks movies
  let request = PaginatedRequest { pageSize    = pageSize
                                 , currentPage = currentPage
                                 , sortOrder   = sortOrder
                                 , sortColumn  = sortColumn
                                 }
  pure $ Movie.paginateMovies movies request
  
makeApplication :: DbEnv -> Application
makeApplication env = serve api server'
 where
  api     = Proxy @Api
  server' = hoistServer api (liftIO . runAppM env) server
