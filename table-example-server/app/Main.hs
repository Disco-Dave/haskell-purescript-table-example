module Main where

import           Relude

import           Api                              ( makeApplication
                                                  , makeDbEnv
                                                  )

import           Configuration.Dotenv             ( Config(..)
                                                  , defaultConfig
                                                  , loadFile
                                                  , onMissingFile
                                                  )
import           Configuration.Dotenv.Environment ( lookupEnv )
import           Network.Wai.Handler.Warp         ( defaultSettings
                                                  , runSettings
                                                  , setBeforeMainLoop
                                                  , setPort
                                                  )
import           Network.Wai.Middleware.Cors      ( simpleCors )


main :: IO ()
main = do
  void loadEnv
  settings <- fmap makeSettings getPort
  app      <- simpleCors . makeApplication <$> makeDbEnv
  runSettings settings app
 where
  printHost p = putTextLn $ "Now listening on http://localhost:" <> show p
  makeSettings p = setBeforeMainLoop (printHost p) $ setPort p defaultSettings
  getPort = do
    port <- lookupEnv "SERVER_PORT"
    pure . fromMaybe 8081 $ port >>= readMaybe
  loadEnv = void $ onMissingFile (loadFile config) (pure [])
    where config = defaultConfig { configPath = [".env"] }
