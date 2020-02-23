module Movie
  ( Movie(..)
  , getMovies
  , paginateMovies
  )
where

import           Relude

import           Paginated

import           Data.Aeson

import qualified Data.Text                     as Text


data Movie = Movie
  { film :: !Text
  , genre :: !Text
  , leadStudio :: !Text
  , audienceScore :: !Integer
  , profitability :: !Double
  , rottenTomatoesScore :: !Integer
  , worldwideGross :: !Double
  , year :: !Integer
  } deriving Show

instance ToJSON Movie where
  toJSON Movie {..} = object
    [ "film" .= film
    , "genre" .= genre
    , "leadStudio" .= leadStudio
    , "audienceScore" .= audienceScore
    , "profitability" .= profitability
    , "rottenTomatoesScore" .= rottenTomatoesScore
    , "worldwideGross" .= worldwideGross
    , "year" .= year
    ]

getMovies :: MonadIO m => m [Movie]
getMovies = do
  rawLines <- drop 1 . lines <$> readFileText "./raw_data/movies.csv"
  pure . catMaybes $ fmap parseRow rawLines

paginateMovies :: [Movie] -> PaginatedRequest -> PaginatedResult Movie
paginateMovies movies request = paginate movies
 where
  paginate = paginateList sortBehavior request
  sortBehavior sortOrder column =
    let comparer = case column of
          "film"                -> comparing Movie.film
          "genre"               -> comparing Movie.genre
          "leadStudio"          -> comparing Movie.leadStudio
          "audienceScore"       -> comparing Movie.audienceScore
          "profitability"       -> comparing Movie.profitability
          "rottenTomatoesScore" -> comparing Movie.rottenTomatoesScore
          "worldwideGross"      -> comparing Movie.worldwideGross
          _                     -> comparing Movie.year
    in  sortBy $ case sortOrder of
          Descending -> flip comparer
          Ascending  -> comparer

parseRow :: Text -> Maybe Movie
parseRow row =
  Movie
    <$> parseText 0
    <*> parseText 1
    <*> parseText 2
    <*> parse 3
    <*> parse 4
    <*> parse 5
    <*> parseMoney 6
    <*> parse 7
 where
  rawFields = Text.splitOn "," row
  parseText i = Text.strip <$> lookup rawFields i
  parse i = parseText i >>= (readMaybe . toString)
  parseMoney i = do
    field <- Text.drop 1 <$> parseText i
    readMaybe $ toString field

lookup :: [a] -> Int -> Maybe a
lookup xs i | i < 0     = Nothing
            | otherwise = go i xs
 where
  go :: Int -> [a] -> Maybe a
  go 0 (x : _ ) = Just x
  go j (_ : ys) = go (j - 1) ys
  go _ []       = Nothing
{-# INLINE lookup #-}
