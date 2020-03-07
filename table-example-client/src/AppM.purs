module AppM(AppM, runAppM) where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Reader.Trans (ReaderT, class MonadAsk, ask, runReaderT)
import Data.Argonaut as J
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import MovieTable as M
import RemoteTable as R
import URI.Extra.QueryPairs as QP
import URI.Query as Q


newtype AppM a = AppM ( ReaderT String Aff a )

runAppM :: forall a. String -> AppM a -> Aff a
runAppM apiUrl (AppM a) = runReaderT a apiUrl

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadAskAppM :: MonadAsk String AppM

instance moviesGetterAppM :: M.MoviesGetter AppM where
  getMovies request = do
    apiUrl <- ask
    response <- liftAff $ hush <$> AX.get ResponseFormat.json (apiUrl <> "/movies" <> queryString)
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
