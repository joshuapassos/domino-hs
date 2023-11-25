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

module Lib where

import           Data.Aeson
import           Data.Aeson.BetterErrors
  ( Parse,
    ParseError,
    parse,
    asIntegral,
    asString,
    key,
    keyMay,
    eachInArray, throwCustomError,
  )
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Debug.Trace

data Lado = Esquerda | Direita deriving (Show)

instance ToJSON Lado where
  toJSON Esquerda = "esquerda"
  toJSON Direita = "direita"

data Tpedra = Branco | As | Duque | Terno | Quadra | Quina | Sena deriving (Eq, Show)

data Pedra = Pedra {
  valor :: (Tpedra, Tpedra),
  raw :: String
} deriving (Show)

-- instance Show Pedra where
--   show (Pedra _ raw) =
--     case raw of
--       "0-0" -> "🁣"
--       "0-1" -> "🀲"
--       "0-2" -> "🀳"
--       "0-3" -> "🀴"
--       "0-4" -> "🀵"
--       "0-5" -> "🀶"
--       "0-6" -> "🀷"
--       "1-1" -> "🁫"
--       "1-2" -> "🀺"
--       "1-3" -> "🀻"
--       "1-4" -> "🀼"
--       "1-5" -> "🀽"
--       "1-6" -> "🀾"
--       "2-1" -> "🁀"
--       "2-2" -> "🁳"
--       "2-3" -> "🁂"
--       "2-4" -> "🁃"
--       "2-5" -> "🁄"
--       "2-6" -> "🁅"
--       "3-0" -> "🁆"
--       "3-1" -> "🁇"
--       "3-2" -> "🁈"
--       "3-3" -> "🁻"
--       "3-4" -> "🁊"
--       "3-5" -> "🁋"
--       "3-6" -> "🁌"
--       "4-0" -> "🁍"
--       "4-1" -> "🁎"
--       "4-2" -> "🁏"
--       "4-3" -> "🁐"
--       "4-4" -> "🂃"
--       "4-5" -> "🁒"
--       "4-6" -> "🁓"
--       "5-0" -> "🁔"
--       "5-1" -> "🁕"
--       "5-2" -> "🁖"
--       "5-3" -> "🁗"
--       "5-4" -> "🁘"
--       "5-5" -> "🂋"
--       "5-6" -> "🁚"
--       "6-0" -> "🁛"
--       "6-1" -> "🁜"
--       "6-2" -> "🁝"
--       "6-3" -> "🁞"
--       "6-4" -> "🁟"
--       "6-5" -> "🁠"
--       "6-6" -> "🂓"
--       _ -> ""


-- instance {-# OVERLAPPING #-} Show [Pedra] where
--   show [] = ""
--   show [x] = show x
--   show (x : xs) = show x <> " " <> show xs

data Jogada = Jogada {
  jogador :: Int,
  pedra :: Pedra,
  lado :: Maybe Lado
} deriving (Show)

data Body = Body {
  jogador :: Int,
  mao :: [Pedra],
  mesa :: [Pedra],
  jogadas :: [Jogada]
} deriving (Show)

asLado :: Parse String Lado
asLado = do
  lado <- asString
  case lado of
    "esquerda" -> pure Esquerda
    "direita" -> pure Direita
    _ -> throwCustomError "Lado inválida"


asValue :: Char -> Tpedra
asValue '0' = Branco
asValue '1' = As
asValue '2' = Duque
asValue '3' = Terno
asValue '4' = Quadra
asValue '5' = Quina
asValue '6' = Sena
asValue v = error (show v)

asPedra :: Parse String Pedra
asPedra = do
  valor <- asString
  let raw = valor
  case valor of
    [p1, '-', p2] ->
      pure $ Pedra (asValue p1, asValue p2) raw
    _ ->
      throwCustomError "Pedra inválida"

asJogada :: Parse String Jogada
asJogada = do
  jogador <- key "jogador" asIntegral
  pedra <- key "pedra" asPedra
  lado <- keyMay "lado" asLado
  return $ Jogada jogador pedra lado

asBody :: Parse String Body
asBody = do
  jogador <- key "jogador" asIntegral
  mao <- key "mao" (eachInArray asPedra)
  mesa <- key "mesa" (eachInArray asPedra)
  jogadas <- key "jogadas" (eachInArray asJogada)
  return $ Body jogador mao mesa jogadas


parse' :: BL.ByteString -> Either (ParseError String) Body
parse' = parse asBody