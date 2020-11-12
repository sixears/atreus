{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Atreus.LayoutDiagram
  ( atreus_layout )
where

--------------------------------------------------------------------------------

import Prelude  ( Double, RealFloat )

-- base --------------------------------

import Control.Applicative     ( ZipList( ZipList ), (<*>) )
import Control.Monad           ( mapM, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False ), not )
import Data.Foldable           ( all, foldl', foldl1, foldMap, foldr, foldr1
                               , toList )
import Data.Function           ( ($), (&), flip )
import Data.Functor            ( (<$>), fmap )
import Data.List               ( filter, reverse, zipWith5 )
import Data.Maybe              ( Maybe( Just, Nothing ), fromMaybe, isNothing )
import Data.Monoid             ( Monoid, mconcat )
import Data.String             ( String )
import GHC.Float               ( Floating )
import System.IO               ( IO )
import Text.Read               ( Read )
import Text.Show               ( Show )

-- base-unicode-symbols ----------------

import Prelude.Unicode          ( (÷) )
import Data.Function.Unicode    ( (∘) )
import Data.Monoid.Unicode      ( (⊕) )

-- colour ------------------------------

import Data.Colour        ( Colour )
import Data.Colour.Names  ( blue, green, grey, red, yellow )

-- data-default ------------------------

import Data.Default  ( def )

-- data-monotraversable ----------------

import Data.MonoTraversable  ( Element
                             , MonoFoldable( ofoldl', ofoldl1Ex', ofoldMap
                                           , ofoldr, ofoldr1Ex, otoList )
                             )

-- diagrams-core -----------------------

import Diagrams.Core            ( D, Diagram )
import Diagrams.Core.HasOrigin  ( HasOrigin, moveOriginBy )
import Diagrams.Core.Juxtapose  ( Juxtaposable )
import Diagrams.Core.Transform  ( transform )
import Diagrams.Core.V          ( N, V )

-- diagrams-lib ------------------------

import Diagrams.Angle             ( (@@), deg, cosA, rotation )
import Diagrams.Attributes        ( lw, none )
import Diagrams.Combinators       ( CatMethod( Distrib ), cat', catMethod, sep )
import Diagrams.Envelope          ( withEnvelope )
import Diagrams.TwoD              ( showEnvelope, showOrigin )
import Diagrams.TwoD.Align        ( alignBL, alignBR, alignTL, alignTR, centerXY )
import Diagrams.TwoD.Attributes   ( fc )
import Diagrams.TwoD.Path         ( strokeP )
import Diagrams.TwoD.Shapes       ( roundedRect, square )
import Diagrams.TwoD.Transform    ( translationY )
import Diagrams.TwoD.Types        ( V2( V2 ) )
import Diagrams.Util              ( (#), with )

-- diagrams-svg ------------------------

import Diagrams.Backend.SVG.CmdLine  ( B )

-- lens --------------------------------

import Control.Lens.Setter  ( (.~) )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, asks, runReaderT )

-- SVGFonts ----------------------------

import qualified  Graphics.SVGFonts  as  SF
import Graphics.SVGFonts  ( Mode( INSIDE_H ), Spacing( KERN )
                          , TextOpts( TextOpts, mode, spacing, textFont
                                    , textHeight, textWidth, underline )
                          , textSVG'
                          )
import Graphics.SVGFonts.ReadFont  ( PreparedFont )

--------------------------------------------------------------------------------

type 𝔹 = Bool
type 𝔻 = Double
type 𝕄 = Maybe
type 𝕊 = String

------------------------------------------------------------

data Fonts ν = Fonts { lin ∷ PreparedFont ν }

{- | Read in a Fonts datum -}
getFonts ∷ (Read ν, RealFloat ν) ⇒ IO (Fonts ν)
getFonts = do
  l ← SF.lin
  return $ Fonts { lin = l }

--------------------------------------------------------------------------------

data Key = Key (𝕄 𝕊) (𝕄 𝕊) (𝕄 𝕊) (𝕄 𝕊) (𝕄 𝕊)
  deriving Show

type instance Element Key = 𝕄 𝕊

instance MonoFoldable Key where
  otoList (Key a b c d e) = [a,b,c,d,e]
  ofoldl' f x = foldl' f x ∘ otoList
  ofoldr f x = foldr f x ∘ otoList
  ofoldMap f = foldMap f ∘ otoList
  ofoldr1Ex f = foldr1 f ∘ otoList
  ofoldl1Ex' f   = foldl1 f ∘ otoList

----------------------------------------

type instance Element (Key,Key,Key,Key) = Key

instance MonoFoldable (Key,Key,Key,Key) where
  otoList (a,b,c,d) = [a,b,c,d]
  ofoldl' f x = foldl' f x ∘ otoList
  ofoldr f x = foldr f x ∘ otoList
  ofoldMap f = foldMap f ∘ otoList
  ofoldr1Ex f = foldr1 f ∘ otoList
  ofoldl1Ex' f   = foldl1 f ∘ otoList

--------------------------------------------------------------------------------

leftLayer0 ∷ [[𝕄 𝕊]]
leftLayer0 = [ (Just `fmap` [ "Q",   "W",   "E",   "R",     "T" ] ⊕ [Nothing])
             , (Just `fmap` [ "A",   "S",   "D",   "F",     "G" ] ⊕ [Nothing])
             , (Just `fmap` [ "Z",   "X",   "C",   "V",     "B",    "~" ])
             , (Just `fmap` [ "Esc", "Tab", "Cmd", "Shift", "BkSp", "Ctrl" ])
             ]

--------------------

leftLayer1 ∷ [[𝕄 𝕊]]
leftLayer1 = [ (Just `fmap` [ "!", "@"  , "↑", "$",     "T" ] ⊕ [Nothing])
             , (Just `fmap` [ "(", "←"  , "↓", "→",     "G" ] ⊕ [Nothing])
             , (Just `fmap` [ "[", "]"  , "#", "{",     "B",    "~" ])
             , (Just `fmap` [ "" , "Ins", "" , "" , "BkSp", "Ctrl" ])
             ]

--------------------

leftLayer2 ∷ [[𝕄 𝕊]]
leftLayer2 = [ (Just `fmap` [ "Ins"  , "Home", "", "End", "T" ] ⊕ [Nothing])
             , (Just `fmap` [ "Del"  , ""    , "", ""   , "G" ] ⊕ [Nothing])
             , (Just `fmap` [ ""     , "Vol+", "", ""   , "B",    "~" ])
             , (Just `fmap` [ "Upper", "Vol-", "", ""   , "BkSp", "Ctrl" ])
             ]

--------------------

leftLayer3 ∷ [[𝕄 𝕊]]
leftLayer3 = [ (Just `fmap` [ "Ins"  , "Home", "", "End", "T" ] ⊕ [Nothing])
             , (Just `fmap` [ "Del"  , ""    , "", ""   , "G" ] ⊕ [Nothing])
             , (Just `fmap` [ ""     , "Vol+", "", ""   , "B",    "~" ])
             , (Just `fmap` [ "Upper", "Vol-", "", ""   , "BkSp", "Ctrl" ])
             ]

--------------------

leftLayer4 ∷ [[𝕄 𝕊]]
leftLayer4 = [ (Just `fmap` [ "Ins"  , "Home", "", "End", "T" ] ⊕ [Nothing])
             , (Just `fmap` [ "Del"  , ""    , "", ""   , "G" ] ⊕ [Nothing])
             , (Just `fmap` [ ""     , "Vol+", "", ""   , "B",    "~" ])
             , (Just `fmap` [ "Upper", "Vol-", "", ""   , "BkSp", "Ctrl" ])
             ]

{- | LHS Keyboard, all layers -}
leftBoard ∷ [[Key]]
leftBoard =
  zipWith5 (zipWith5 Key) leftLayer0 leftLayer1 leftLayer2 leftLayer3 leftLayer4

leftColumns ∷ [(Key,Key,Key,Key)]
leftColumns = let [r0,r1,r2,r3] = fmap ZipList leftBoard
                in toList $ (,,,) <$> r0 <*> r1 <*> r2 <*> r3

------------------------------------------------------------

{- | A width-one square with slightly rounded corners. -}
box1 ∷ Diagram B
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
text ∷ MonadReader (Fonts 𝔻) η ⇒ 𝔻 → 𝕊 → η (Diagram B)
text h t = do
  o ← topts INSIDE_H h 1 -- the width is irrelevant with INSIDE_H
  return $ strokeP (textSVG' o t) # lw none

----------------------------------------

{- | Create a text diagram of given height (using the `SF.lin` font);
     with given height, colour, alignment; and position. -}
text' ∷ MonadReader (Fonts 𝔻) η ⇒
      𝔻 → 𝕊 → Colour 𝔻 → 𝔻 → 𝔻 → (Diagram B → Diagram B) → η (Diagram B)
text' h  t c x y a = do
  t' ← text h t
  return (moveOriginBy (V2 x y) $ t' # fc c # a)

----------------------------------------

{- | Create a diagram for a key with the given labels. -}
key ∷ MonadIO μ ⇒ Key → μ (Diagram B)
key (Key c tl tr bl br) = liftIO $ do
  fonts ← getFonts
  flip runReaderT fonts $ do
    t0 ← text' 0.5  (fromMaybe "" c)  grey    0       0     centerXY
    t1 ← text' 0.35 (fromMaybe "" tr) red   (-0.45) (-0.45) alignTR
    t2 ← text' 0.35 (fromMaybe "" br) blue  (-0.45)   0.45  alignBR
    t3 ← text' 0.35 (fromMaybe "" tl) green   0.45  (-0.45) alignTL
    t4 ← text' 0.35 (fromMaybe "" bl) yellow  0.45    0.45  alignBL

    return $ mconcat [ box1 , t0, t1, t2, t3, t4 ]
----------------------------------------

keys ∷ (MonadIO μ, MonoFoldable φ, Element φ ~ Key) ⇒ φ → μ [Diagram B]
keys = mapM key ∘ filter (not ∘ all isNothing ∘ otoList) ∘ otoList

------------------------------------------------------------

atreus_layout ∷ IO (Diagram B)
atreus_layout = do
  fonts ← getFonts @𝔻
  flip runReaderT fonts $ do
    [ks0,ks1,ks2,ks3,ks4,ks5] ← mapM keys leftColumns

    let rot = -10@@deg

    return $ cat' (V2 1 0) (with & catMethod .~ Distrib & sep .~ (1.2 ÷ cosA rot))
             [ (cat' (V2 0 1) (with & sep .~ 0.1) (reverse ks0) # transform (rotation rot) # showOrigin # showEnvelope)
             , (vsup 0.1 (reverse ks1) # transform (rotation rot) # showOrigin # showEnvelope)
             , vsup 0.1 (reverse ks2) # transform (rotation rot)
             , vsup 0.1 (reverse ks3) # transform (translationY (-0.5)) # transform (rotation rot) # showOrigin
             , vsup 0.1 (reverse ks4) # transform (translationY (-1.0)) # transform (rotation rot) # showOrigin
             , vsup 0.1 (reverse ks5) # transform (translationY (-1.0)) # transform (rotation rot) # showOrigin
             ]

-- that's all, folks! ----------------------------------------------------------
