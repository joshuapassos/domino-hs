{-# LANGUAGE OverloadedStrings #-}

import qualified Lib
import qualified Network.HTTP.Types
import           Web.Scotty
import qualified Domino

main :: IO ()
main = scotty 8000 $
  post "/" $ do
    beam <-  body
    json $ Domino.playGame $ Lib.parse' beam
    status Network.HTTP.Types.status200