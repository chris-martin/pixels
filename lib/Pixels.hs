module Pixels where

import Prelude

import qualified Pixels.Server

runServer :: IO ()
runServer = Pixels.Server.main
