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

import Prelude  ( Double, RealFloat, fromIntegral )

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
import Data.Maybe              ( Maybe( Just, Nothing ), catMaybes, fromMaybe, isNothing )
import Data.Monoid             ( Monoid, mconcat, (<>) )
import Data.String             ( String )
import GHC.Float               ( Floating )
import System.IO               ( IO )
import Text.Read               ( Read )
import Text.Show               ( Show )

-- base-unicode-symbols ----------------

import Prelude.Unicode          ( (⋅), (÷) )
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
                             )

-- diagrams-core -----------------------

import Diagrams.Core            ( D, Diagram )
import Diagrams.Core.HasOrigin  ( HasOrigin, moveOriginBy )
import Diagrams.Core.Juxtapose  ( Juxtaposable )
import Diagrams.Core.Transform  ( transform )
import Diagrams.Core.V          ( N, V )

-- diagrams-lib ------------------------

import Diagrams.Angle             ( Angle, (@@), deg, cosA, rotation )
import Diagrams.Attributes        ( lw, none )
import Diagrams.Combinators       ( CatMethod( Distrib ), beside, cat, cat'
                                  , catMethod, frame, sep )
import Diagrams.Envelope          ( pad, setEnvelope, withEnvelope )
import Diagrams.TwoD              ( angleV, showEnvelope, showOrigin )
import Diagrams.TwoD.Align        ( alignBL, alignBR, alignTL, alignTR
                                  , centerXY, snugL, snugR, snugX )
import Diagrams.TwoD.Attributes   ( fc )
import Diagrams.TwoD.Combinators  ( boundingRect, hcat, hsep, vsep )
import Diagrams.TwoD.Path         ( strokeP )
import Diagrams.TwoD.Shapes       ( roundedRect, square )
import Diagrams.TwoD.Transform    ( translationY )
import Diagrams.TwoD.Types        ( V2( V2 ), r2 )
import Diagrams.Util              ( (#), with )

-- diagrams-svg ------------------------

import Diagrams.Backend.SVG.CmdLine  ( B )

-- lens --------------------------------

import Control.Lens.Setter  ( (.~) )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, asks, runReaderT )

-- safe --------------------------------

import Safe  ( atDef, atMay )

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

------------------------------------------------------------

{- | A width-one square with slightly rounded corners. -}
box1 ∷ Diagram B
box1 = roundedRect 1 1 0.05

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

key' ∷ MonadIO μ ⇒ Key → μ (Diagram B)
key' (Key a b c d e) = key ((fromMaybe "" a),(fromMaybe "" b),(fromMaybe "" c),(fromMaybe "" d),(fromMaybe "" e))

{- | Create a diagram for a key with the given labels. -}
key ∷ MonadIO μ ⇒ (𝕊,𝕊,𝕊,𝕊,𝕊) → μ (Diagram B)
key (c,tl,tr,bl,br) = liftIO $ do
  fonts ← getFonts
  flip runReaderT fonts $ do
    t0' ← text' 0.5  c  grey    0       0     centerXY
    t1' ← text' 0.35 tr red   (-0.45) (-0.45) alignTR
    t2' ← text' 0.35 br blue  (-0.45)   0.45  alignBR
    t3' ← text' 0.35 tl green   0.45  (-0.45) alignTL
    t4' ← text' 0.35 bl yellow  0.45    0.45  alignBL

    return $ mconcat [ box1 , t0', t1', t2', t3', t4' ] # withEnvelope (square 1 ∷ D V2 𝔻)

data HalfRow = HalfRow 𝕊 𝕊 𝕊 𝕊 𝕊 (𝕄 𝕊)
  deriving Show
data HalfBoard = HalfBoard HalfRow HalfRow HalfRow HalfRow
  deriving Show
data Board = Board HalfBoard HalfBoard
  deriving Show

type instance Element HalfBoard = HalfRow

instance MonoFoldable HalfBoard where
  otoList (HalfBoard a b c d) = [a,b,c,d]
  ofoldl' f x = foldl' f x ∘ otoList
  ofoldr f x = foldr f x ∘ otoList
  ofoldMap f = foldMap f ∘ otoList
  ofoldr1Ex f = foldr1 f ∘ otoList
  ofoldl1Ex' f   = foldl1 f ∘ otoList

type instance Element HalfRow = 𝕄 𝕊

instance MonoFoldable HalfRow where
  otoList (HalfRow a b c d e f) = [Just a, Just b, Just c, Just d, Just e,f]
  ofoldl' f x = foldl' f x ∘ otoList
  ofoldr f x = foldr f x ∘ otoList
  ofoldMap f = foldMap f ∘ otoList
  ofoldr1Ex f = foldr1 f ∘ otoList
  ofoldl1Ex' f   = foldl1 f ∘ otoList

leftBoard ∷ HalfBoard
leftBoard = HalfBoard (HalfRow "Q" "W" "E" "R" "T" Nothing)
                      (HalfRow "A" "S" "D" "F" "G" Nothing)
                      (HalfRow "Z" "X" "C" "V" "B" (Just "~"))
                      (HalfRow "Esc" "Tab" "Cmd" "Shift" "BkSp" (Just "Ctrl"))

-- data Column = [𝕊]

-- data Columns = Columns Column Column Column Column Column

leftColumns ∷ ZipList (𝕄 𝕊,𝕄 𝕊,𝕄 𝕊,𝕄 𝕊)
leftColumns = let [r0,r1,r2,r3] = fmap ZipList (otoList <$> otoList leftBoard)
               in (,,,) <$> r0 <*> r1 <*> r2 <*> r3


leftBoard0' ∷ [[𝕄 𝕊]]
leftBoard0' = [ (Just `fmap` [ "Q",   "W",   "E",   "R",     "T" ] ⊕ [Nothing])
             , (Just `fmap` [ "A",   "S",   "D",   "F",     "G" ] ⊕ [Nothing])
             , (Just `fmap` [ "Z",   "X",   "C",   "V",     "B",    "~" ])
             , (Just `fmap` [ "Esc", "Tab", "Cmd", "Shift", "BkSp", "Ctrl" ])
             ]

leftBoard1' ∷ [[𝕄 𝕊]]
leftBoard1' = [ (Just `fmap` [ "!", "@"  , "↑", "$",     "T" ] ⊕ [Nothing])
              , (Just `fmap` [ "(", "←"  , "↓", "→",     "G" ] ⊕ [Nothing])
              , (Just `fmap` [ "[", "]"  , "#", "{",     "B",    "~" ])
              , (Just `fmap` [ "" , "Ins", "" , "" , "BkSp", "Ctrl" ])
              ]

leftBoard2' ∷ [[𝕄 𝕊]]
leftBoard2' = [ (Just `fmap` [ "Ins"  , "Home", "", "End", "T" ] ⊕ [Nothing])
              , (Just `fmap` [ "Del"  , ""    , "", ""   , "G" ] ⊕ [Nothing])
              , (Just `fmap` [ ""     , "Vol+", "", ""   , "B",    "~" ])
              , (Just `fmap` [ "Upper", "Vol-", "", ""   , "BkSp", "Ctrl" ])
              ]

leftBoard3' ∷ [[𝕄 𝕊]]
leftBoard3' = [ (Just `fmap` [ "Ins"  , "Home", "", "End", "T" ] ⊕ [Nothing])
              , (Just `fmap` [ "Del"  , ""    , "", ""   , "G" ] ⊕ [Nothing])
              , (Just `fmap` [ ""     , "Vol+", "", ""   , "B",    "~" ])
              , (Just `fmap` [ "Upper", "Vol-", "", ""   , "BkSp", "Ctrl" ])
              ]

leftBoard4' ∷ [[𝕄 𝕊]]
leftBoard4' = [ (Just `fmap` [ "Ins"  , "Home", "", "End", "T" ] ⊕ [Nothing])
              , (Just `fmap` [ "Del"  , ""    , "", ""   , "G" ] ⊕ [Nothing])
              , (Just `fmap` [ ""     , "Vol+", "", ""   , "B",    "~" ])
              , (Just `fmap` [ "Upper", "Vol-", "", ""   , "BkSp", "Ctrl" ])
              ]

leftColumns' ∷ [(Key,Key,Key,Key)]
leftColumns' = let [r0,r1,r2,r3] = fmap ZipList leftBoard''
                in toList $ (,,,) <$> r0 <*> r1 <*> r2 <*> r3

data Key = Key (𝕄 𝕊) (𝕄 𝕊) (𝕄 𝕊) (𝕄 𝕊) (𝕄 𝕊)
  deriving Show

{- | LHS Keyboard, all layers -}
leftBoard'' ∷ [[Key]]
leftBoard'' =
  zipWith5 (zipWith5 Key) leftBoard0' leftBoard1' leftBoard2' leftBoard3' leftBoard4'
  
ttoList ∷ (𝕄 α,𝕄 α,𝕄 α,𝕄 α) → [α]
ttoList (m0,m1,m2,m3) = catMaybes [m0,m1,m2,m3]

type instance Element Key = 𝕄 𝕊

instance MonoFoldable Key where
  otoList (Key a b c d e) = [a,b,c,d,e]
  ofoldl' f x = foldl' f x ∘ otoList
  ofoldr f x = foldr f x ∘ otoList
  ofoldMap f = foldMap f ∘ otoList
  ofoldr1Ex f = foldr1 f ∘ otoList
  ofoldl1Ex' f   = foldl1 f ∘ otoList

type instance Element (Key,Key,Key,Key) = Key

instance MonoFoldable (Key,Key,Key,Key) where
  otoList (a,b,c,d) = [a,b,c,d]
  ofoldl' f x = foldl' f x ∘ otoList
  ofoldr f x = foldr f x ∘ otoList
  ofoldMap f = foldMap f ∘ otoList
  ofoldr1Ex f = foldr1 f ∘ otoList
  ofoldl1Ex' f   = foldl1 f ∘ otoList

c0,c1,c2,c3,c4,c5∷ (Key,Key,Key,Key)
[c0,c1,c2,c3,c4,c5] = leftColumns'


(¡) ∷ MonoFoldable χ ⇒ χ → ℕ → 𝕄 (Element χ)
xs ¡ i = otoList xs `atMay` fromIntegral i

(¡¡) ∷ (MonoFoldable χ, Element χ ~ Maybe α) ⇒ χ → ℕ → 𝕄 α
xs ¡¡ i = atDef Nothing (otoList xs) (fromIntegral i)

col0 ∷ [(𝕊,𝕊,𝕊,𝕊,𝕊)]
col0 = [ ("Q",   "!", "Ins"  , "", "' \"")
       , ("A",   "(", "Del"  , "", "a A")
       , ("Z",   "[", ""     , "", "; :")
       , ("Esc", "" , "Upper", "",  "")
       ]

col1 ∷ [(𝕊,𝕊,𝕊,𝕊,𝕊)]
col1 = [ ("W",   "@"   , "Home"  , "", "")
       , ("S",   "←"   , ""      , "", "")
       , ("X",   "]"   , "Vol+"  , "", "")
       , ("Tab", "Ins" , "Vol-"  , "", "")
       ]

col2 ∷ [(𝕊,𝕊,𝕊,𝕊,𝕊)]
col2 = [ ("E",   "↑" , ""  , "", "")
       , ("D",   "↓" , ""  , "", "")
       , ("C",   "#" , ""  , "", "")
       , ("Cmd", ""  , ""  , "", "")
       ]

col3 ∷ [(𝕊,𝕊,𝕊,𝕊,𝕊)]
col3 = [ ("R"    , "$" , "End"  , "", "")
       , ("F"    , "→" , ""     , "", "")
       , ("V"    , "{" , ""     , "", "")
       , ("Shift", ""  , ""     , "", "")
       ]

col4 ∷ [(𝕊,𝕊,𝕊,𝕊,𝕊)]
col4 = [ ("T"    , "$" , "End"  , "", "")
       , ("G"    , "→" , ""     , "", "")
       , ("B"    , "{" , ""     , "", "")
       , ("BkSp" , ""  , ""     , "", "")
       ]

col5 ∷ [(𝕊,𝕊,𝕊,𝕊,𝕊)]
col5 = [ ("T"    , "$" , "End"  , "", "")
       , ("BkSp" , ""  , ""     , "", "")
       ]

{- | Like `vsep`, but going upwards rather than downwards. -}

vsup ∷ (Floating (N δ), Juxtaposable δ, Monoid δ, HasOrigin δ, V δ ~ V2) ⇒
       N δ -> [δ] -> δ

vsup s = cat' (V2 0 1) (def & sep .~ s)
atreus_layout ∷ IO (Diagram B)
atreus_layout = do
  fonts ← getFonts @𝔻
  flip runReaderT fonts $ do
--    ks ← mapM (key_rot (-10@@deg)) layout
--    ks0 ← mapM key col0
    ks0 ← mapM key' (filter (not ∘ all (≡ Nothing) ∘ otoList) $ otoList c0)
    ks1 ← mapM key col1
    ks2 ← mapM key col2
    ks3 ← mapM key col3
    ks4 ← mapM key col4
--    ks5 ← mapM key col5
    ks5 ← mapM key' (filter (not ∘ all (≡ Nothing) ∘ otoList) $ otoList c5)

    let rot = -10@@deg

    return $ -- beside (angleV (-0@@deg)) -- (r2 (0.1,0))
             cat' (V2 1 0) (with & catMethod .~ Distrib & sep .~ (1.2 ÷ cosA rot))
             [
                       (cat' (V2 0 1) (with & sep .~ 0.1) (reverse ks0) # transform (rotation rot) # showOrigin # showEnvelope) -- # snugX 1.2
             ,
                     (vsup 0.1 (reverse ks1) # transform (rotation rot) # showOrigin # showEnvelope) -- # snugx
             , vsup 0.1 (reverse ks2) # transform (rotation rot)
             , vsup 0.1 (reverse ks3) # transform (translationY (-0.5)) # transform (rotation rot) # showOrigin
             , vsup 0.1 (reverse ks4) # transform (translationY (-1.0)) # transform (rotation rot) # showOrigin
             , vsup 0.1 (reverse ks5) # transform (translationY (-1.0)) # transform (rotation rot) # showOrigin
             ]
--    return $ hcat [
--                       vsep 0.1 ks0 # transform (rotation (-10@@deg)) -- # snugX 1.2
--                     , vsep 0.1 ks1 # transform (rotation (-10@@deg)) -- # snugX (-1.2)
--                     , vsep 0.1 ks2 # transform (rotation (-10@@deg)) # snugX (-3.6)
--                     , vsep 0.1 ks3 # transform (rotation (-10@@deg)) # snugX (-5.9) # transform (translationY (-0.5))
--                     , vsep 0.1 ks4 # transform (rotation (-10@@deg)) # snugX (-8.3) # transform (translationY (-1))
--                     , vsep 0.1 ks5 # transform (rotation (-10@@deg)) # snugX (-10.3) # transform (translationY (-1)) # showEnvelope
--                     , vsep 0.1 ks5 # transform (rotation (-10@@deg)) # transform (translationY (-1)) # showEnvelope
--                     , vsep 0.1 ks5 # transform (rotation (-10@@deg)) # transform (translationY (-1)) # pad 1.2 # showEnvelope
--                     , vsep 0.1 ks5 # transform (rotation (-10@@deg)) # transform (translationY (-1)) # frame 1.2 # showEnvelope
--                      vsep 0.1 ks5 # transform (rotation (-10@@deg)) # transform (translationY (-1)) # frame 1.2 # {- withEnvelope (square 1 ∷ D V2 𝔻) # -} showEnvelope
--                      (boundingRect (vsep 0.1 ks5)) ∷ Diagram B
--      setEnvelope @_ @_ @𝔻  (boundingRect (square 1)) (vsep 0.1 ks5) # showEnvelope
--                     ]

-- that's all, folks! ----------------------------------------------------------
