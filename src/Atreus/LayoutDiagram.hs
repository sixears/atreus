{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Atreus.LayoutDiagram
  ( atreus_layout )
where

import Control.Lens.Operators  ( (<&>) )
import Control.Lens.Tuple

--------------------------------------------------------------------------------

import Prelude  ( Double, RealFloat, fromIntegral )

-- aeson -------------------------------

import Data.Aeson  ( FromJSON, Value, eitherDecodeFileStrict' )

-- base --------------------------------

import Control.Applicative     ( ZipList( ZipList ), (<*>) )
import Control.Monad           ( MonadFail, (>>=), join, mapM, return,sequence )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False ), not )
import Data.Either             ( Either( Left, Right ) )
import Data.Foldable           ( all, foldl', foldl1, foldMap, foldr, foldr1
                               , length, toList )
import Data.Function           ( ($), (&), flip )
import Data.Functor            ( (<$>), fmap )
import Data.List               ( drop, filter, repeat, replicate
                               , reverse, splitAt, take, zipWith5 )
import Data.Maybe              ( Maybe( Just, Nothing ), fromMaybe, isNothing )
import Data.Monoid             ( Monoid, mconcat )
import Data.Ord                ( (>) )
import Data.String             ( String )
import Data.Traversable        ( Traversable )
import GHC.Float               ( Floating )
import GHC.Generics            ( Generic )
import System.IO               ( FilePath, IO )
import Text.Read               ( Read )
import Text.Show               ( Show, show )

-- base-unicode-symbols ----------------

import Prelude.Unicode          ( (÷) )
import Data.Eq.Unicode          ( (≡) )
import Data.List.Unicode        ( (∈) )
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

import Diagrams.Core            ( Diagram )
import Diagrams.Core.HasOrigin  ( HasOrigin, moveOriginBy )
import Diagrams.Core.Juxtapose  ( Juxtaposable )
import Diagrams.Core.Transform  ( transform )
import Diagrams.Core.V          ( N, V )

-- diagrams-lib ------------------------

import Diagrams.Angle             ( (@@), deg, cosA, rotation )
import Diagrams.Attributes        ( lw, none )
import Diagrams.Combinators       ( CatMethod( Distrib ), cat', catMethod, sep )
import Diagrams.TwoD              ( showEnvelope, showOrigin )
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

import Control.Lens.Setter  ( (.~) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )
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

instance Field1 Key Key (𝕄 𝕊) (𝕄 𝕊) where
  _1 k (Key a b c d e) = k a <&> \ a' -> (Key a' b c d e)
  {-# INLINE _1 #-}

instance Field2 Key Key (𝕄 𝕊) (𝕄 𝕊) where
  _2 k (Key a b c d e) = k b <&> \ b' -> (Key a b' c d e)
  {-# INLINE _2 #-}

instance Field3 Key Key (𝕄 𝕊) (𝕄 𝕊) where
  _3 k (Key a b c d e) = k c <&> \ c' -> (Key a b c' d e)
  {-# INLINE _3 #-}

instance Field4 Key Key (𝕄 𝕊) (𝕄 𝕊) where
  _4 k (Key a b c d e) = k d <&> \ d' -> (Key a b c d' e)
  {-# INLINE _4 #-}

instance Field5 Key Key (𝕄 𝕊) (𝕄 𝕊) where
  _5 k (Key a b c d e) = k e <&> \ e' -> (Key a b c d e')
  {-# INLINE _5 #-}
  
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
    let place ks y = vsup 0.1 (reverse ks) # transform (translationY y)
                                           # transform (rotation rot)
                                           -- # showOrigin
                                           -- # showEnvelope

    return $ cat' (V2 1 0)
                  (with & catMethod .~ Distrib & sep .~ (1.2 ÷ cosA rot))
                  [ place ks0 0
                  , place ks1 0
                  , place ks2 0
                  , place ks3 (-0.5)
                  , place ks4 (-1.0)
                  , place ks5 (-1.0)
                  ]

-- that's all, folks! ----------------------------------------------------------

data AtreusLayerKey = AtreusLayerKey { keyCode ∷ ℕ
                                     , label   ∷ 𝕊
                                     , verbose ∷ 𝕄 𝕊
                                     , extraLabel ∷ 𝕄 𝕊
                                     }
  deriving (Generic, Show)

instance FromJSON AtreusLayerKey

{- | A non-key, as represented in an atreus layer -}
atreusLayerNoKey ∷ AtreusLayerKey
atreusLayerNoKey = AtreusLayerKey 0 "Blocked" (Just "Disabled") Nothing

{- | A non-functioning-key, as represented in an atreus layer -}
atreusLayerEmptyKey ∷ AtreusLayerKey
atreusLayerEmptyKey = AtreusLayerKey 65535 "" (Just "Transparent") Nothing

------------------------------------------------------------

data AtreusLayer = AtreusLayer { keymap ∷ [AtreusLayerKey] }
  deriving (Generic, Show)

instance FromJSON AtreusLayer

type instance Element AtreusLayer = AtreusLayerKey

instance MonoFoldable AtreusLayer where
  otoList (AtreusLayer ks) = ks
  ofoldl' f x = foldl' f x ∘ otoList
  ofoldr f x = foldr f x ∘ otoList
  ofoldMap f = foldMap f ∘ otoList
  ofoldr1Ex f = foldr1 f ∘ otoList
  ofoldl1Ex' f   = foldl1 f ∘ otoList

{- | An empty atreus layer -}
atreusLayerEmpty ∷ AtreusLayer
atreusLayerEmpty = AtreusLayer $ replicate 48 atreusLayerEmptyKey

------------------------------------------------------------

data ETooManyLayers = ETooManyLayers ℕ
  deriving Show

data AtreusBoard = AtreusBoard AtreusLayer AtreusLayer AtreusLayer
                               AtreusLayer AtreusLayer
  deriving Show

boardFromLayers ∷ MonadError ETooManyLayers η ⇒ [AtreusLayer] → η AtreusBoard
boardFromLayers ls =
  if length ls > 5
  then throwError $ ETooManyLayers (fromIntegral $ length ls)
  else let [l0,l1,l2,l3,l4] = take 5 $ ls ⊕ repeat atreusLayerEmpty
        in return $ AtreusBoard l0 l1 l2 l3 l4


decode ∷ MonadIO μ ⇒ FilePath → μ (Either 𝕊 AtreusLayer)
decode fn = liftIO $ eitherDecodeFileStrict' @AtreusLayer fn

fns ∷ [FilePath]
fns = fmap ("/home/martyn/rc/atreus/default-layout/layer" ⊕) ["0","1","2"]

decodes ∷ MonadIO μ ⇒ [FilePath] → μ (Either 𝕊 AtreusBoard)
decodes fns = do
  fmap sequence (mapM decode fns) >>= \ case
    Left e   → return $ Left e
    Right ls → case boardFromLayers ls of
                 Left e' → return $ Left (show e')
                 Right b → return $ Right b

groupN n xs = if length xs > n then take n xs : groupN n (drop n xs) else [xs]

{- | Split an atreus layer def into rows, being ltop, rtop, lnext, rnext, ... -}
alKeys ∷ AtreusLayer → [[AtreusLayerKey]]
alKeys = groupN 6 ∘ otoList

board ∷ (MonadIO μ, MonadFail μ) ⇒
        μ [(AtreusLayerKey,AtreusLayerKey,AtreusLayerKey,AtreusLayerKey,
            AtreusLayerKey)]
board = do
  Right (AtreusBoard l0 l1 l2 l3 l4) ← decodes fns
  return $ toList $ (,,,,) <$> ZipList (otoList l0)
                           <*> ZipList (otoList l1)
                           <*> ZipList (otoList l2)
                           <*> ZipList (otoList l3)
                           <*> ZipList (otoList l4)

t5map ∷ (β → α) → (β,β,β,β,β) → (α,α,α,α,α)

t5map f (a,b,c,d,e) = (f a, f b, f c, f d, f e)

lrRows ∷ (MonadIO μ, MonadFail μ) ⇒ μ [[(𝕊,𝕊,𝕊,𝕊,𝕊)]]
lrRows = fmap (fmap (fmap $ t5map label)) (groupN 6 <$> board)

mkKey ∷ (𝕊,𝕊,𝕊,𝕊,𝕊) → Key
mkKey input =
  let (a,b,c,d,e) = t5map (\ s → if s ∈ [ "", "Blocked" ]
                                 then Nothing
                                 else Just s)
                          input
   in Key a b c d e

getRows = do
  [l0,r0,l1,r1,l2,r2,l3,r3] ← fmap (fmap $ fmap mkKey) $ lrRows
  return (l0,r0,l1,r1,l2,r2,l3,r3)

getCols ∷ IO ([(Key,Key,Key,Key)], [(Key,Key,Key,Key)])
getCols = do
  [l0,r0,l1,r1,l2,r2,l3,r3] ← fmap (fmap $ fmap mkKey) $ lrRows
--  return (l0,r0,l1,r1,l2,r2,l3,r3)
  return $ ( toList $ (,,,) <$> ZipList l0 <*> ZipList l1
                            <*> ZipList l2 <*> ZipList l3
           , toList $ (,,,) <$> ZipList r0 <*> ZipList r1
                            <*> ZipList r2 <*> ZipList r3
           )

lrRows' ∷ (MonadIO μ, MonadFail μ) ⇒ μ [[Diagram B]]
lrRows' = join $ fmap (mapM $ keys ∘ fmap mkKey) lrRows

-- getCols' ∷ IO ([(Diagram B,Diagram B,Diagram B,Diagram B)], [(Diagram B,Diagram B,Diagram B,Diagram B)])
getCols' = do
  [l0,r0,l1,r1,l2,r2,l3,r3] ← fmap (fmap $ fmap mkKey) $ lrRows
  join $ return $ mapM (mapM key) [l0,r0]
{-
  return $ ( toList $ (,,,) <$> ZipList l0 <*> ZipList l1
                            <*> ZipList l2 <*> ZipList l3
           , toList $ (,,,) <$> ZipList r0 <*> ZipList r1
                            <*> ZipList r2 <*> ZipList r3
           )
-}

{-
atreus_layout' ∷ IO (Diagram B)
atreus_layout' = do
  fonts ← getFonts @𝔻
  flip runReaderT fonts $ do
    ([c0,c1,c2,c3,c4,c5],[c6,c7,c8,c9,c10,c11]) ← getCols

    let rot = -10@@deg
    let place ks y = vsup 0.1 (reverse ks) # transform (translationY y)
                                           # transform (rotation rot)
                                           -- # showOrigin
                                           -- # showEnvelope

    return $ cat' (V2 1 0)
                  (with & catMethod .~ Distrib & sep .~ (1.2 ÷ cosA rot))
                  [ place c0 0
                  , place c1 0
                  , place c2 0
                  , place c3 (-0.5)
                  , place c4 (-1.0)
                  , place c5 (-1.0)
                  ]

-}
