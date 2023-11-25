{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DeriveGeneric, PolyKinds #-}

module Domino where

import           Data.Aeson
import           Data.Aeson.BetterErrors
  (
    ParseError,
  )
import           GHC.Generics
import Lib
import Data.List
import qualified Debug.Trace

data Response = Err String | Pass | Res {
  pedra :: String,
  lado :: Lib.Lado
} deriving (Show, Generic)

instance ToJSON Response where
  toJSON Pass = object []
  toJSON (Err msg) = object [
    "error" .= msg
    ]
  toJSON (Res pedra lado) = object [
    "pedra" .= pedra,
    "lado" .= lado
    ]

canPlay :: Pedra -> Tpedra -> Bool
canPlay (Pedra (a, b) _) x = a == x || b == x

playGame' :: Lib.Body -> Response
playGame' body =
  let ponta_esquerda = head (mesa body)
      ponta_direita = last (mesa body)
      jogadae = find (\p -> canPlay p (fst (valor ponta_esquerda))) (mao body)
      jogadad = find (\p -> canPlay p (snd (valor ponta_direita))) (mao body)
      r = show (jogadae, jogadad)
  in do
    case (jogadae, jogadad) of
      (Just (Pedra _ raw), _) -> Res raw Lib.Esquerda
      (_, Just (Pedra _ raw)) -> Res raw Lib.Direita
      (_) ->
        Debug.Trace.trace r
        Pass

playGame :: Either (ParseError String) Lib.Body  -> Response
playGame (Left err) = Err $ show err
playGame (Right body) = playGame' body