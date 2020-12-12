{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Atreus.Types
  ( AtreusBoardSpecT( AtreusBoardSpec ), AtreusBoardSpec
  , AtreusKeySpec( label )
  , AtreusKeySpecsT( AtreusKeySpecs ), AtreusKeySpecs
  , AtreusLayerSpec
  , BoardT( BoardT, Board ), Board
  , KeyColT( KeyCol ), KeyCol
  , KeyLabelsT( KeyLabels ), KeyLabels
  , KeyRow( KeyRow )
  , atreusLayerEmpty, atreusLayerEmptyKey, fullLabel, readBoard
  )
where

-- aeson -------------------------------

import Data.Aeson  ( FromJSON )

-- base --------------------------------

import Data.Function     ( ($) )
import Data.Functor      ( (<$>), fmap )
import Data.List         ( replicate )
import Data.Maybe        ( Maybe( Just, Nothing ), maybe )
import Data.String       ( String )
import Data.Traversable  ( traverse )
import GHC.Generics      ( Generic )
import Text.Show         ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode    ( (∘) )
import Data.Monoid.Unicode      ( (⊕) )
import Numeric.Natural.Unicode  ( ℕ )

-- data-monotraversable ----------------

import Data.MonoTraversable  ( Element, MonoFoldable, MonoFunctor
                             , MonoTraversable( otraverse ) )

-- lens --------------------------------

import Control.Lens.Iso     ( iso )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FixedList  ( AsL4( l4 ), AsL5( l5 ), AsL6( l6 ), AsL8( l8 )
                  , L4( L4 ), L5( L5 ), L6, L8( L8 ), readL8 )

--------------------------------------------------------------------------------

type 𝕄 = Maybe
type 𝕊 = String

------------------------------------------------------------

newtype AtreusLayerSpec = AtreusLayerSpec { keymap ∷ [AtreusKeySpec] }
  deriving (Generic, MonoFoldable, Show)

instance FromJSON AtreusLayerSpec

type instance Element AtreusLayerSpec = AtreusKeySpec

{- | An empty atreus layer -}
atreusLayerEmpty ∷ AtreusLayerSpec
atreusLayerEmpty = AtreusLayerSpec $ replicate 48 atreusLayerEmptyKey

------------------------------------------------------------

data AtreusKeySpec = AtreusKeySpec { keyCode ∷ ℕ
                                   , label   ∷ 𝕊
                                   , verbose ∷ 𝕄 𝕊
                                   , extraLabel ∷ 𝕄 𝕊
                                   }
  deriving (Generic, Show)

instance FromJSON AtreusKeySpec

{- | A non-functioning-key, as represented in an atreus layer -}
atreusLayerEmptyKey ∷ AtreusKeySpec
atreusLayerEmptyKey = AtreusKeySpec 65535 "" (Just "Transparent") Nothing

fullLabel ∷ AtreusKeySpec → 𝕊
fullLabel k = maybe "" (⊕ " ") (extraLabel k) ⊕ label k

------------------------------------------------------------

{- | A collection of 5 `AtreusKeySpec`s; one for each layer. -}
newtype AtreusKeySpecsT =
  AtreusKeySpecsT { unAtreusKeySpecsT ∷ L5 AtreusKeySpec }
  deriving MonoFunctor
type instance Element AtreusKeySpecsT = AtreusKeySpec
type AtreusKeySpecs = AtreusKeySpecsT
instance AsL5 AtreusKeySpecs where
  l5 = iso unAtreusKeySpecsT AtreusKeySpecsT

pattern AtreusKeySpecs ∷ AtreusKeySpec → AtreusKeySpec → AtreusKeySpec
                       → AtreusKeySpec → AtreusKeySpec → AtreusKeySpecsT
pattern AtreusKeySpecs l0 l1 l2 l3 l4 = AtreusKeySpecsT (L5 l0 l1 l2 l3 l4)
{-# COMPLETE AtreusKeySpecs #-}

------------------------------------------------------------

{- | A set of labels across 5 layers for a single key. -}
newtype KeyLabelsT = KeyLabelsT { unKeyLabelsT ∷ L5 𝕊 }
  deriving (MonoFoldable, MonoFunctor)

type instance Element KeyLabelsT = 𝕊

type KeyLabels = KeyLabelsT
pattern KeyLabels ∷ 𝕊 → 𝕊 → 𝕊 → 𝕊 → 𝕊 → KeyLabelsT
pattern KeyLabels l0 l1 l2 l3 l4 = KeyLabelsT (L5 l0 l1 l2 l3 l4)
{-# COMPLETE KeyLabels #-}

instance AsL5 KeyLabelsT where
  l5 = iso unKeyLabelsT KeyLabelsT

------------------------------------------------------------

{- | A row of 6 `KeyLabel`s -}
newtype KeyRow = KeyRow { unKeyRow ∷ L6 KeyLabels }
  deriving MonoFoldable

type instance Element KeyRow = KeyLabels

instance AsL6 KeyRow where
  l6 = iso unKeyRow KeyRow

------------------------------------------------------------

newtype BoardT = BoardT { unBoardT ∷ L8 KeyRow }

type instance Element BoardT = KeyRow

type Board = BoardT
pattern Board ∷ KeyRow → KeyRow → KeyRow → KeyRow → KeyRow → KeyRow → KeyRow
              → KeyRow → BoardT
pattern Board r0 r1 r2 r3 r4 r5 r6 r7 = BoardT (L8 r0 r1 r2 r3 r4 r5 r6 r7)
{-# COMPLETE Board #-}

instance AsL8 Board where
  l8 = iso unBoardT BoardT

{- | Convert a list of keyrows to a board, iff it's the right number of rows
     (being 8: four left, four right (interleaved: l r l r l r l r, top-down. -}
readBoard ∷ [KeyRow] → Maybe Board
readBoard = fmap BoardT ∘ readL8

------------------------------------------------------------

newtype KeyColT = KeyColT { unKeyColT ∷ L4 KeyLabels }
  deriving (MonoFoldable, MonoFunctor)

type KeyCol = KeyColT

type instance Element KeyCol = KeyLabels

instance MonoTraversable KeyCol where
  otraverse f (KeyColT ls) = KeyColT <$> traverse f ls
  
pattern KeyCol ∷ KeyLabels → KeyLabels → KeyLabels → KeyLabels → KeyCol

pattern KeyCol k0 k1 k2 k3 = KeyColT (L4 k0 k1 k2 k3)
{-# COMPLETE Board #-}

instance AsL4 KeyColT where
  l4 = iso unKeyColT KeyColT

------------------------------------------------------------

{-| Specification of a full atreus keyboard, as 5x AtreusLayerSpec. -}
newtype AtreusBoardSpecT = AtreusBoardSpecT (L5 AtreusLayerSpec)
type AtreusBoardSpec = AtreusBoardSpecT
pattern AtreusBoardSpec ∷ AtreusLayerSpec → AtreusLayerSpec → AtreusLayerSpec
                        → AtreusLayerSpec → AtreusLayerSpec → AtreusBoardSpecT
pattern AtreusBoardSpec l0 l1 l2 l3 l4 = AtreusBoardSpecT (L5 l0 l1 l2 l3 l4)

-- that's all, folks! ----------------------------------------------------------
