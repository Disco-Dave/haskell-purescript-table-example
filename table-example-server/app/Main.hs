module Main where

import           Relude

import           Api

import qualified Network.Wai.Handler.Warp      as Warp
import qualified Network.Wai.Middleware.Cors   as Cors


main :: IO ()
main = 
  fmap makeApplication makeDbEnv 
  >>= Warp.run 8081 . Cors.simpleCors
