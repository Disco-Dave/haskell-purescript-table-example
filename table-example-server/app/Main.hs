module Main where

import           Relude

import           Api

import qualified Network.Wai.Handler.Warp      as Warp


main :: IO ()
main = fmap makeApplication makeDbEnv >>= Warp.run 8081
