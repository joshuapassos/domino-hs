{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Domino where

import Data.Aeson
import Data.Aeson.BetterErrors
  ( ParseError,
  )
import qualified Data.List.NonEmpty as NE
import Debug.Trace
import GHC.Base (liftM)
import GHC.Generics
import Lib

data Response
  = Err String
  | Pass
  | Res
      { pedra :: String,
        lado :: Lib.Lado
      }
  deriving (Show, Generic)

instance ToJSON Response where
  toJSON Pass = object []
  toJSON (Err msg) =
    object
      [ "error" .= msg
      ]
  toJSON (Res pedra lado) =
    object
      [ "pedra" .= pedra,
        "lado" .= lado
      ]

canPlay :: Tpedra -> Pedra -> Bool
canPlay x (Pedra (a, b) _) = a == x || b == x

playGame' :: Lib.Body -> Response
playGame' body =
  case (playableEsq, playableDir) of
    (Just (pedra : _), Nothing) -> Res (raw pedra) Lib.Esquerda
    (Nothing, Just (pedra : _)) -> Res (raw pedra) Lib.Direita
    (Just (pedra : _), Just (pedra' : __))
      | pedra > pedra' -> Res (raw pedra) Lib.Esquerda
      | pedra < pedra' -> Res (raw pedra') Lib.Direita
    _ ->
      Pass
  where
    safe_mesa = NE.nonEmpty . mesa $ body
    safe_mao = NE.nonEmpty . mao $ body
    ponta_esquerda = NE.head <$> safe_mesa
    ponta_direita = NE.last <$> safe_mesa
    tpedra_esq = fst <$> (valor <$> ponta_esquerda)
    tpedra_dir = snd <$> (valor <$> ponta_direita)
    get tpedra_dir safe_mao = do
      a <- tpedra_dir
      NE.filter (canPlay a) <$> safe_mao
    playableEsq = get tpedra_esq safe_mao
    playableDir = get tpedra_dir safe_mao

playGame :: Either (ParseError String) Lib.Body -> Response
playGame (Left err) = Err $ show err
playGame (Right body) = playGame' body