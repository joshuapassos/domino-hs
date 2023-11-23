{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.IO.Class (liftIO)
import qualified Lib
import qualified Network.HTTP.Types
import           Web.Scotty

main :: IO ()
main = scotty 3000 $
  post "/" $ do
    beam <-  body
    let a = Lib.parse' beam
    liftIO $ print a
    status Network.HTTP.Types.status200