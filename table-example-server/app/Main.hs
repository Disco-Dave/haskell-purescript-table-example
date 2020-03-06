module Main where

import           Relude

import           Api

import qualified Configuration.Dotenv             as Dotenv
import qualified Configuration.Dotenv.Environment as Env
import qualified Network.Wai.Handler.Warp         as Warp
import qualified Network.Wai.Middleware.Cors      as Cors


main :: IO ()
main = do
  void loadEnv
  port <- appPort
  let 
    printHost = putTextLn $ "Now listening on http://localhost:" <> show port
    settings = Warp.setBeforeMainLoop printHost $ Warp.setPort port Warp.defaultSettings
  app <- Cors.simpleCors . makeApplication <$> makeDbEnv
  Warp.runSettings settings app

appPort :: IO Int
appPort = do
  port <- Env.lookupEnv "SERVER_PORT"
  pure . fromMaybe 8081 $ port >>= readMaybe

loadEnv :: IO ()
loadEnv = void $ Dotenv.onMissingFile (Dotenv.loadFile config) (pure [])
  where config = Dotenv.defaultConfig { Dotenv.configPath = [".env"] }
