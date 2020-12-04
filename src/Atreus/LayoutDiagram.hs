{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
-- {-# LANGUAGE LiberalTypeSynonyms        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module Atreus.LayoutDiagram
  ( atreus_layout )
where

--------------------------------------------------------------------------------

import Prelude  ( Double, RealFloat, undefined )

-- aeson -------------------------------

import Data.Aeson  ( FromJSON, eitherDecodeFileStrict' )

-- base --------------------------------

import Control.Applicative     ( Applicative( pure, (<*>) ), ZipList( ZipList ) )
import Control.Monad           ( (>>=), join, mapM, return, sequence )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False ) )
import Data.Either             ( Either( Left, Right ), either )
import Data.Foldable           ( Foldable, all, foldl', foldl1, foldMap, foldr
                               , foldr1, length, toList )
import Data.Function           ( ($), (&), flip, id )
import Data.Functor            ( Functor( fmap ), (<$>) )
import Data.List               ( repeat, replicate, reverse, take )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.Monoid             ( Monoid, mconcat, mempty )
import Data.Ord                ( (>) )
import Data.String             ( String )
import Data.Traversable        ( Traversable( traverse ) )
import GHC.Float               ( Floating )
import GHC.Generics            ( Generic )
import System.Exit             ( ExitCode( ExitFailure ), exitWith )
import System.IO               ( FilePath, IO, hPutStrLn, stderr )
import Text.Read               ( Read )
import Text.Show               ( Show, show )

-- base-unicode-symbols ----------------

import Prelude.Unicode          ( (÷) )
import Data.Eq.Unicode          ( (≡) )
import Data.Function.Unicode    ( (∘) )
import Data.Monoid.Unicode      ( (⊕) )
import Numeric.Natural.Unicode  ( ℕ )

-- colour ------------------------------

import Data.Colour        ( Colour )
import Data.Colour.Names  ( blue, green, grey, red, yellow )

-- data-default ------------------------

import Data.Default  ( def )

-- data-monotraversable ----------------

import Data.MonoTraversable  ( Element
                             , MonoFoldable( ofoldl', ofoldl1Ex', ofoldMap
                                           , ofoldr, ofoldr1Ex, otoList )
                             , MonoFunctor( omap )
                             , MonoTraversable( otraverse, omapM )
                             )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString )

-- diagrams-core -----------------------

import Diagrams.Core            ( Diagram )
import Diagrams.Core.HasOrigin  ( HasOrigin, moveOriginBy )
import Diagrams.Core.Juxtapose  ( Juxtaposable )
import Diagrams.Core.Transform  ( transform )
import Diagrams.Core.V          ( N, V )

-- diagrams-lib ------------------------

import Diagrams.Angle             ( (@@), deg, cosA, rotation )
import Diagrams.Attributes        ( lw, none )
import Diagrams.Combinators       ( CatMethod( Distrib ), cat', catMethod, sep )
import Diagrams.TwoD.Align        ( alignBL, alignBR, alignTL, alignTR, centerXY )
import Diagrams.TwoD.Attributes   ( fc )
import Diagrams.TwoD.Path         ( strokeP )
import Diagrams.TwoD.Shapes       ( roundedRect )
import Diagrams.TwoD.Transform    ( translationY )
import Diagrams.TwoD.Types        ( V2( V2 ) )
import Diagrams.Util              ( (#), with )

-- diagrams-svg ------------------------

import Diagrams.Backend.SVG.CmdLine  ( B )

-- lens --------------------------------

import Control.Lens.Getter  ( (^.), view )
import Control.Lens.Iso     ( Iso, from, iso )
import Control.Lens.Setter  ( (.~) )
import Control.Lens.Type    ( Simple )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, runExceptT, throwError )
import Control.Monad.Reader  ( MonadReader, asks, runReaderT )

-- SVGFonts ----------------------------

import qualified  Graphics.SVGFonts  as  SF
import Graphics.SVGFonts  ( Mode( INSIDE_H ), Spacing( KERN )
                          , TextOpts( TextOpts, mode, spacing, textFont
                                    , textHeight, textWidth, underline )
                          , textSVG'
                          )
import Graphics.SVGFonts.ReadFont  ( PreparedFont )

-- text-printer ------------------------

import qualified Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FixedList  ( AsL4( l4 ), AsL5( l5 ), AsL6( l6 ), AsL8( l8 )
                  , L4( L4 ), L5( L5 ), L6( L6 ), L8( L8 ) )

--------------------------------------------------------------------------------

type 𝔻 = Double
type 𝕄 = Maybe
type 𝕊 = String

type DiagramB = Diagram B

------------------------------------------------------------


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

------------------------------------------------------------

data Fonts ν = Fonts { lin ∷ PreparedFont ν }

{- | Read in a Fonts datum -}
getFonts ∷ (Read ν, RealFloat ν) ⇒ IO (Fonts ν)
getFonts = do
  l ← SF.lin
  return $ Fonts { lin = l }
--------------------------------------------------------------------------------

{- | A width-one square with slightly rounded corners. -}
box1 ∷ DiagramB
box1 = roundedRect 1 1 0.05

----------------------------------------

{- | Like `vsep`, but going upwards rather than downwards. -}

vsup ∷ (Floating (N δ), Juxtaposable δ, Monoid δ, HasOrigin δ, V δ ~ V2) ⇒
       N δ -> [δ] -> δ
vsup s = cat' (V2 0 1) (def & sep .~ s)

----------------------------------------

{- | Create a `TextOpts` datum with given height; using the `SF.lin`
     font, with the given sizing mode, height & width; using Kerning, and no
     underline.
 -}
topts ∷ MonadReader (Fonts ν) η ⇒ Mode → ν → ν → η (TextOpts ν)
topts m h w = do
  l ← asks lin
  return TextOpts { textFont = l, mode = m, spacing = KERN
                  , underline = False, textWidth = w, textHeight = h }

----------------------------------------

{- | Create a text diagram of given height (using the `SF.lin` font). -}
text ∷ MonadReader (Fonts 𝔻) η ⇒ 𝔻 → 𝕊 → η DiagramB
text h t = do
  o ← topts INSIDE_H h 1 -- the width is irrelevant with INSIDE_H
  return $ strokeP (textSVG' o t) # lw none

----------------------------------------

{- | Create a text diagram of given height (using the `SF.lin` font);
     with given height, colour, alignment; and position. -}
text' ∷ MonadReader (Fonts 𝔻) η ⇒
      𝔻 → 𝕊 → Colour 𝔻 → 𝔻 → 𝔻 → (DiagramB → DiagramB) → η DiagramB
text' h  t c x y a = do
  t' ← text h t
  return (moveOriginBy (V2 x y) $ t' # fc c # a)

----------------------------------------

{- | Create a diagram for a key with the given labels.  Return an empty diagram
     if all the labels are `Nothing` (as opposed to, say, the empty string). -}
key ∷ MonadReader (Fonts 𝔻) μ ⇒ KeyLabels → μ DiagramB
key k@(KeyLabels c tl tr bl br) = do
  let -- kblank converts texts that should have no text - i.e., Blocked labels
      -- and empty labels - to empty.
      kblank "Blocked" = ""
      kblank x         = x
      isNull x = all (\ s → "" ≡ kblank s) (otoList x)
  t0 ← text' 0.5  (kblank c)  grey    0       0     centerXY
  t1 ← text' 0.35 (kblank tr) red   (-0.45) (-0.45) alignTR
  t2 ← text' 0.35 (kblank br) blue  (-0.45)   0.45  alignBR
  t3 ← text' 0.35 (kblank tl) green   0.45  (-0.45) alignTL
  t4 ← text' 0.35 (kblank bl) yellow  0.45    0.45  alignBL

  return $ if isNull k
           then mempty
           else mconcat [ box1, t0, t1, t2, t3, t4 ]

----------------------------------------

fmap3 ∷ (Functor ψ, Functor κ, Functor φ) ⇒
        (α → β) → ψ (κ (φ α)) → ψ (κ (φ β))
fmap3 = fmap ∘ fmap ∘ fmap

fmap4 ∷ (Functor ψ, Functor κ, Functor φ, Functor ρ) ⇒
        (α → β) → ψ (κ (φ (ρ α))) → ψ (κ (φ (ρ β)))
fmap4 = fmap ∘ fmap ∘ fmap ∘ fmap

{- | Group keys into 6s. -}
group6Keys ∷ (MonadIO μ, MonadError AtreusLayoutE μ) ⇒ [FilePath] → μ [L6 AtreusKeySpecs]
group6Keys = join ∘ fmap (groupL6 AtreusWrongKeyCount) ∘ board

{- | Read some layer files, group the keys together into 8 rows of 6 each. -}
lrRows ∷ (MonadIO μ, MonadError AtreusLayoutE μ) ⇒ [FilePath] → μ Board
lrRows fns =

  fmap3 (view $ from l5) (fmap4 label (fmap3 (view l5) $ group6Keys fns)) >>= \ case
    [l0,r0,l1,r1,l2,r2,l3,r3] → return $ (view $ from l8) $ KeyRow <$> L8 l0 r0 l1 r1 l2 r2 l3 r3
    rows                      → throwError $ AtreusWrongRowCount rows

----------------------------------------

{- | Two (6-long) lists of (4-high) columns of keys, as diagrams; split into
     left & right.
 -}
lrCols ∷ (MonadIO μ, MonadError AtreusLayoutE μ, MonadReader (Fonts 𝔻) μ) ⇒
            [FilePath] → μ (L6 (L4 DiagramB),L6 (L4 DiagramB))
lrCols fns = do
  -- each of l0,r0,…,r3 is ∷ L6 KeyLabels
  L8 l0 r0 l1 r1 l2 r2 l3 r3 ← fmap (view l6) <$> view l8 <$> lrRows fns

  let l ∷ L6 KeyCol = KeyCol <$> l0 <*> l1 <*> l2 <*> l3
      r ∷ L6 KeyCol = KeyCol <$> r0 <*> r1 <*> r2 <*> r3

  l' ← sequence $ fmap (mapM key ∘ view l4) l
  r' ← sequence $ fmap (mapM key ∘ view l4) r
  return (l',r')

------------------------------------------------------------

atreus_layout ∷ IO DiagramB
atreus_layout = do
  fonts ← getFonts @𝔻
  flip runReaderT fonts $ do
    (l,r) ← runExceptT (lrCols filenames) >>= \ case
                                                Right r → return r
                                                Left  e → liftIO $ do
                                                  hPutStrLn stderr (toString e)
                                                  exitWith (ExitFailure 255)

    let (L6 lt0 lt1 lt2 lt3 lt4 lt5) = l
        (L6 rt0 rt1 rt2 rt3 rt4 rt5) = r

    let lrot = -10@@deg
        rrot =  10@@deg
        place ks y rot = vsup 0.1 (reverse $ toList ks)
                                                    # transform (translationY y)
                                                    # transform (rotation rot)

    return $ cat' (V2 1 0)
                  (with & catMethod .~ Distrib & sep .~ (1.2 ÷ cosA lrot))
                  [ place lt0 0      lrot
                  , place lt1 0      lrot
                  , place lt2 0      lrot
                  , place lt3 (-0.5) lrot
                  , place lt4 (-1.0) lrot
                  , place lt5 (-1.0) lrot
                  , place rt0 (-1.0) rrot
                  , place rt1 (-1.0) rrot
                  , place rt2 (-0.5) rrot
                  , place rt3 0      rrot
                  , place rt4 0      rrot
                  , place rt5 0      rrot
                  ]

-- that's all, folks! ----------------------------------------------------------

newtype AtreusLayerSpec = AtreusLayerSpec { keymap ∷ [AtreusKeySpec] }
  deriving (Generic, MonoFoldable, Show)

instance FromJSON AtreusLayerSpec

type instance Element AtreusLayerSpec = AtreusKeySpec

{- | An empty atreus layer -}
atreusLayerEmpty ∷ AtreusLayerSpec
atreusLayerEmpty = AtreusLayerSpec $ replicate 48 atreusLayerEmptyKey

------------------------------------------------------------

{- | Create an atreus keyboard definition from a list of up to 5 layers.
     If fewer than 5 layers are provided, the remainder will be made up of
     empty layers.
     If more than 5 layers are provided, will throw an `AtreusTooManyLayers`
     exception.
 -}
boardFromLayers ∷ MonadError AtreusLayoutE η ⇒ [AtreusLayerSpec] → η AtreusBoardSpec
boardFromLayers ls =
  if length ls > 5
  then throwError $ AtreusTooManyLayers ls
  else let [l0,l1,l2,l3,l4] = take 5 $ ls ⊕ repeat atreusLayerEmpty
        in return $ AtreusBoardSpec l0 l1 l2 l3 l4

decode ∷ (MonadIO μ, MonadError AtreusLayoutE μ) ⇒ FilePath → μ AtreusLayerSpec
decode = let ethrow = either (throwError ∘ AtreusFailedDecodeE) return
           in join ∘ liftIO ∘ fmap ethrow ∘ eitherDecodeFileStrict' @AtreusLayerSpec

filenames ∷ [FilePath]
filenames = fmap ("/home/martyn/rc/atreus/default-layout/layer" ⊕) ["0","1","2"]

decodes ∷ (MonadIO μ, MonadError AtreusLayoutE μ) ⇒ [FilePath] → μ AtreusBoardSpec
decodes fns = mapM decode fns >>= boardFromLayers

groupL6 ∷ MonadError ε η ⇒ ([α] → ε) → [α] → η [L6 α]
groupL6 _   []               = return []
groupL6 err (a:b:c:d:e:f:xs) = (L6 a b c d e f :) <$> (groupL6 err xs)
groupL6 err xs               = throwError $ err xs

data AtreusLayoutE = AtreusFailedDecodeE 𝕊
                   | AtreusWrongRowCount [L6 KeyLabels]
                   | AtreusWrongKeyCount [AtreusKeySpecs]
                   | AtreusWrongColumnCount [KeyCol]
                   | AtreusTooManyLayers [AtreusLayerSpec]

instance Printable AtreusLayoutE where
  print (AtreusFailedDecodeE s) = P.string $ "layer decode failed: " ⊕ s
  print (AtreusWrongRowCount rs) =
    P.string $ "got wrong number of rows: " ⊕ show (length rs)
  print (AtreusWrongKeyCount ks) =
    P.string $ "got wrong number of keys: " ⊕ show (length ks)
  print (AtreusWrongColumnCount cs) =
    P.string $ "got wrong column count: " ⊕ show (length cs)
  print (AtreusTooManyLayers ls) =
    P.string $ "got too many layers: " ⊕ show (length ls)

{- | A list of keys, over 5 layers.  Only keys that are represented on all
     layers are returned. -}
board ∷ (MonadIO μ, MonadError AtreusLayoutE μ) ⇒ [FilePath] → μ [AtreusKeySpecs]
board fns =
    (\ (AtreusBoardSpec l0 l1 l2 l3 l4) → 
      toList $ AtreusKeySpecs <$> ZipList (otoList l0)
                  <*> ZipList (otoList l1)
                  <*> ZipList (otoList l2)
                  <*> ZipList (otoList l3)
                  <*> ZipList (otoList l4)) <$> decodes fns

