{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnicodeSyntax             #-}
{-# LANGUAGE ViewPatterns              #-}

module Atreus.LayoutDiagram
  ( atreus_layout )
where

import Control.Lens.Operators  ( (<&>) )
import Control.Lens.Tuple

--------------------------------------------------------------------------------

import Prelude  ( Double, RealFloat, fromIntegral, undefined )

-- aeson -------------------------------

import Data.Aeson  ( FromJSON, Value, eitherDecodeFileStrict' )

-- base --------------------------------

import Control.Applicative     ( Applicative( (<*>), liftA2, pure )
                               , ZipList( ZipList ) )
import Control.Monad           ( MonadFail, (>>=), join, mapM, return,sequence )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False ), not )
import Data.Either             ( Either( Left, Right ) )
import Data.Foldable           ( Foldable, all, foldl', foldl1, foldMap, foldr
                               , foldr1, length, toList )
import Data.Function           ( ($), (&), flip )
import Data.Functor            ( Functor( fmap ), (<$>) )
import Data.List               ( drop, filter, repeat, replicate
                               , reverse, splitAt, take, zipWith5 )
import Data.Maybe              ( Maybe( Just, Nothing ), fromMaybe, isNothing )
import Data.Monoid             ( Monoid, mconcat, mempty )
import Data.Ord                ( (>) )
import Data.String             ( String )
import Data.Traversable        ( Traversable( traverse ) )
import GHC.Float               ( Floating )
import GHC.Generics            ( Generic )
import System.IO               ( FilePath, IO )
import Text.Read               ( Read )
import Text.Show               ( Show, show )

-- base-unicode-symbols ----------------

import Prelude.Unicode          ( (÷) )
import Data.Bool.Unicode        ( (∧) )
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
                             , MonoFunctor ( omap )
                             )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

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

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified Text.Printer  as  P

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

class IsNull α where
  isNull ∷ α → 𝔹

------------------------------------------------------------

data Key = Key (𝕄 𝕊) (𝕄 𝕊) (𝕄 𝕊) (𝕄 𝕊) (𝕄 𝕊)
  deriving Show

--------------------

instance IsNull Key where
  isNull (Key a b c d e) = all isNothing [a,b,c,d,e]

--------------------

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
key k@(Key c tl tr bl br) = liftIO $ do
  fonts ← getFonts
  flip runReaderT fonts $ do
    t0 ← text' 0.5  (fromMaybe "" c)  grey    0       0     centerXY
    t1 ← text' 0.35 (fromMaybe "" tr) red   (-0.45) (-0.45) alignTR
    t2 ← text' 0.35 (fromMaybe "" br) blue  (-0.45)   0.45  alignBR
    t3 ← text' 0.35 (fromMaybe "" tl) green   0.45  (-0.45) alignTL
    t4 ← text' 0.35 (fromMaybe "" bl) yellow  0.45    0.45  alignBL

    return $ if isNull k
             then mempty
             else mconcat [ box1, t0, t1, t2, t3, t4 ]

{- | Create a diagram for a key with the given labels.  Return an empty diagram
     if all the labels are `Nothing` (as opposed to, say, the empty string). -}
key' ∷ MonadReader (Fonts 𝔻) μ ⇒ Key → μ (Diagram B)
key' k@(Key c tl tr bl br) = do
  t0 ← text' 0.5  (fromMaybe "" c)  grey    0       0     centerXY
  t1 ← text' 0.35 (fromMaybe "" tr) red   (-0.45) (-0.45) alignTR
  t2 ← text' 0.35 (fromMaybe "" br) blue  (-0.45)   0.45  alignBR
  t3 ← text' 0.35 (fromMaybe "" tl) green   0.45  (-0.45) alignTL
  t4 ← text' 0.35 (fromMaybe "" bl) yellow  0.45    0.45  alignBL

  return $ if isNull k
           then mempty
           else mconcat [ box1, t0, t1, t2, t3, t4 ]

----------------------------------------

keys ∷ (MonadIO μ, MonoFoldable φ, Element φ ~ Key) ⇒ φ → μ [Diagram B]
keys = mapM key ∘ filter (not ∘ all isNothing ∘ otoList) ∘ otoList

------------------------------------------------------------

atreus_layout ∷ IO (Diagram B)
atreus_layout = do
  fonts ← getFonts @𝔻
  flip runReaderT fonts $ do
    -- XXX !!! Use types for, e.g., KeySpec; check lengths of lists !!!
    -- XXX !!! placeCols fn                                         !!!
--     ((ks0:ks1:ks2:ks3:ks4:ks5:_),_) ← keyCols_' fns
    

    ((L6 ks0 ks1 ks2 ks3 ks4 ks5),_) ← runExceptT (keyCols_'' fns) >>= \ case
                                         Right r → return r


    let rot = -10@@deg
    let place ks y = vsup 0.1 (reverse $ toList ks) # transform (translationY y)
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

groupN ∷ ℕ → [α] → [[α]]
groupN n@(fromIntegral → i) xs = if length xs > i then take i xs : groupN n (drop i xs) else [xs]

groupT6 ∷ MonadError ε η ⇒ ([α] → ε) → [α] → η [L6 α]
groupT6 _   []               = return []
groupT6 err (a:b:c:d:e:f:xs) = (L6 a b c d e f :) <$> (groupT6 err xs)
groupT6 err xs               = throwError $ err xs

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

board' ∷ (MonadIO μ, MonadFail μ) ⇒
        [FilePath]
      → μ [(AtreusLayerKey,AtreusLayerKey,AtreusLayerKey,AtreusLayerKey,
            AtreusLayerKey)]
board' fns = do
  Right (AtreusBoard l0 l1 l2 l3 l4) ← decodes fns
  return $ toList $ (,,,,) <$> ZipList (otoList l0)
                           <*> ZipList (otoList l1)
                           <*> ZipList (otoList l2)
                           <*> ZipList (otoList l3)
                           <*> ZipList (otoList l4)

data L4 α = L4 α α α α

instance Functor L4 where
  fmap f (L4 a b c d) = L4 (f a) (f b) (f c) (f d)

instance Foldable L4 where
  foldr f x (L4 a b c d) = foldr f x [a,b,c,d]

instance Traversable L4 where
  {-# INLINE traverse #-} -- so that traverse can fuse
  traverse f (L4 a b c d) = L4 <$> f a <*> f b <*> f c <*> f d

data L5 α = L5 α α α α α

instance Functor L5 where
  fmap f (L5 a b c d e) = L5 (f a) (f b) (f c) (f d) (f e)

instance Foldable L5 where
  foldr f x (L5 a b c d e) = foldr f x [a,b,c,d,e]

type AKey' = L5 AtreusLayerKey

{- | A six-tuple of a fixed type, or a list of 6, if you prefer. -}
data L6 α = L6 α α α α α α

{-
instance Applicative L6 where
  {-# INLINE pure #-}
  pure x    = L6 x x x x x x
  {-# INLINE (<*>) #-}
  (L6 f0 f1 f2 f3 f4 f5) <*> (L6 x0 x1 x2 x3 x4 x5) =
    L6 (f0 x0) (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5)
-}

instance Traversable L6 where
  {-# INLINE traverse #-} -- so that traverse can fuse
  traverse g (L6 a b c d e f) =
    L6 <$> g a <*> g b <*> g c <*> g d <*> g e <*> g f
{-
    foldr1 cons_f
      where cons_f x ys = _ -- liftA2 (:) (f x) ys
-}

instance Functor L6 where
  fmap g (L6 a b c d e f) = L6 (g a) (g b) (g c) (g d) (g e) (g f)

instance Foldable L6 where
  foldr g x (L6 a b c d e f) = foldr g x [a,b,c,d,e,f]

-- data AKey' = AKey' AtreusLayerKey AtreusLayerKey AtreusLayerKey AtreusLayerKey
--                    AtreusLayerKey

-- type instance Element AKey' = AtreusLayerKey

-- instance MonoFunctor AKey' where
--   omap f (AKey' a b c d e) = AKey' (f a) (f b) (f c) (f d) (f e)

{-
instance MonoFoldable AKey' where
  otoList (AKey' a b c d e) = [a,b,c,d,e]
  ofoldl' f x = foldl' f x ∘ otoList
  ofoldr f x = foldr f x ∘ otoList
  ofoldMap f = foldMap f ∘ otoList
  ofoldr1Ex f = foldr1 f ∘ otoList
  ofoldl1Ex' f   = foldl1 f ∘ otoList
-}

data AtreusLayoutE = AtreusFailedDecodeE 𝕊
                   | AtreusWrongRowCount [L6 KeySpec]
                   | AtreusWrongKeyCount [AKey']
                   | AtreusWrongColumnCount [KeyCol]

instance Printable AtreusLayoutE where
  print (AtreusFailedDecodeE s) = P.string $ "layer decode failed: " ⊕ s
  print (AtreusWrongRowCount rs) =
    P.string $ "got wrong number of rows: " ⊕ show (length rs)
  print (AtreusWrongKeyCount ks) =
    P.string $ "got wrong number of keys: " ⊕ show (length ks)
  print (AtreusWrongColumnCount cs) =
    P.string $ "got wrong column count: " ⊕ show (length cs)

{- | A list of keys, over 5 layers.  Only keys that are represented on all
     layers are returned. -}
board_' ∷ (MonadIO μ, MonadError AtreusLayoutE μ) ⇒ [FilePath] → μ [AKey']
board_' fns = do
  decodes fns >>= \ case
    Right (AtreusBoard l0 l1 l2 l3 l4) → 
      return $ toList $ L5 <$> ZipList (otoList l0)
                           <*> ZipList (otoList l1)
                           <*> ZipList (otoList l2)
                           <*> ZipList (otoList l3)
                           <*> ZipList (otoList l4)
    Left e → throwError $ AtreusFailedDecodeE e

t5map ∷ (β → α) → (β,β,β,β,β) → (α,α,α,α,α)

t5map f (a,b,c,d,e) = (f a, f b, f c, f d, f e)

mkKey ∷ (𝕊,𝕊,𝕊,𝕊,𝕊) → Key
mkKey input =
  let (a,b,c,d,e) = t5map (\ s → if s ∈ [ "", "Blocked" ]
                                 then Nothing
                                 else Just s)
                          input
   in Key a b c d e

mkKey' ∷ KeySpec → Key
mkKey' k =
  let (L5 a b c d e) = fmap (\ s → if s ∈ [ "", "Blocked" ]
                                 then Nothing
                                 else Just s)
                          k
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

lrRows ∷ (MonadIO μ, MonadFail μ) ⇒ μ [[(𝕊,𝕊,𝕊,𝕊,𝕊)]]
lrRows = fmap (fmap (fmap $ t5map label)) (groupN 6 <$> board)

lrRows_ ∷ (MonadIO μ, MonadFail μ) ⇒ [FilePath] → μ [[(𝕊,𝕊,𝕊,𝕊,𝕊)]]
lrRows_ fns = fmap (fmap (fmap $ t5map label)) (groupN 6 <$> board' fns)

data Board = Board [(𝕊,𝕊,𝕊,𝕊,𝕊)] [(𝕊,𝕊,𝕊,𝕊,𝕊)] [(𝕊,𝕊,𝕊,𝕊,𝕊)] [(𝕊,𝕊,𝕊,𝕊,𝕊)] [(𝕊,𝕊,𝕊,𝕊,𝕊)] [(𝕊,𝕊,𝕊,𝕊,𝕊)] [(𝕊,𝕊,𝕊,𝕊,𝕊)] [(𝕊,𝕊,𝕊,𝕊,𝕊)]

type KeySpec = L5 𝕊

type Row = L6 KeySpec

data Board' = Board' [KeySpec] [KeySpec] [KeySpec] [KeySpec] [KeySpec] [KeySpec] [KeySpec] [KeySpec]

type KeyCol = L4 KeySpec

type KeyRow = L6 KeySpec

data Board'' = Board'' KeyRow KeyRow KeyRow KeyRow KeyRow KeyRow KeyRow KeyRow

data EWrongRowCount = EWrongRowCount ℕ [ℕ]
  deriving Show

len ∷ Foldable ψ ⇒ ψ α → ℕ
len = fromIntegral ∘ length

lrRows__' ∷ (MonadIO μ, MonadError AtreusLayoutE μ) ⇒
            [FilePath] → μ [L6 (L5 𝕊)]
lrRows__' fns = join $ fmap (fmap (fmap (fmap $ fmap label))) (groupT6 AtreusWrongKeyCount <$> board_' fns)

lrRows__'' ∷ (MonadIO μ, MonadError AtreusLayoutE μ) ⇒ [FilePath] → μ Board''
lrRows__'' fns =
  lrRows__' fns >>= \ case
    [l0,r0,l1,r1,l2,r2,l3,r3] → return $ Board'' l0 r0 l1 r1 l2 r2 l3 r3
    rows                      → throwError $ AtreusWrongRowCount rows

lrCols ∷ (MonadIO μ, MonadFail μ) ⇒ μ ([[(𝕊,𝕊,𝕊,𝕊,𝕊)]],[[(𝕊,𝕊,𝕊,𝕊,𝕊)]])
lrCols = do
  [l0,r0,l1,r1,l2,r2,l3,r3] ← lrRows
  let (+++) a b c d = [a,b,c,d]

  return $ ( toList $ (+++) <$> ZipList l0 <*> ZipList l1
                            <*> ZipList l2 <*> ZipList l3
           , toList $ (+++) <$> ZipList r0 <*> ZipList r1
                            <*> ZipList r2 <*> ZipList r3
           )

lrCols_ ∷ (MonadIO μ, MonadFail μ) ⇒
          [FilePath] → μ ([[(𝕊,𝕊,𝕊,𝕊,𝕊)]],[[(𝕊,𝕊,𝕊,𝕊,𝕊)]])
lrCols_ fns = do
  [l0,r0,l1,r1,l2,r2,l3,r3] ← lrRows_ fns
  let (+++) a b c d = [a,b,c,d]

  return $ ( toList $ (+++) <$> ZipList l0 <*> ZipList l1
                            <*> ZipList l2 <*> ZipList l3
           , toList $ (+++) <$> ZipList r0 <*> ZipList r1
                            <*> ZipList r2 <*> ZipList r3
           )

lrCols_' ∷ (MonadIO μ, MonadError AtreusLayoutE μ) ⇒
           [FilePath] → μ ([KeyCol],[KeyCol])
lrCols_' fns = do
  Board'' l0 r0 l1 r1 l2 r2 l3 r3 ← lrRows__'' fns

  let kcol [x0,x1,x2,x3,x4,x5] = return $ L6 x0 x1 x2 x3 x4 x5
      kcol xs                  = throwError $ AtreusWrongColumnCount xs

  return $ ( toList $ L4 <$> ZipList (toList l0) <*> ZipList (toList l1)
                            <*> ZipList (toList l2) <*> ZipList (toList l3)
           , toList $ L4 <$> ZipList (toList r0) <*> ZipList (toList r1)
                            <*> ZipList (toList r2) <*> ZipList (toList r3)
           )

lrCols_'' ∷ (MonadIO μ, MonadError AtreusLayoutE μ) ⇒
            [FilePath] → μ (L6 KeyCol,L6 KeyCol)
lrCols_'' fns = do
  Board'' l0 r0 l1 r1 l2 r2 l3 r3 ← lrRows__'' fns

  let kcol [x0,x1,x2,x3,x4,x5] = return $ L6 x0 x1 x2 x3 x4 x5
      kcol xs                  = throwError $ AtreusWrongColumnCount xs

  l ∷ L6 KeyCol ← kcol $ toList $ L4 <$> ZipList (toList l0)
                                     <*> ZipList (toList l1)
                                     <*> ZipList (toList l2)
                                     <*> ZipList (toList l3)

  r ∷ L6 KeyCol ← kcol $ toList $ L4 <$> ZipList (toList r0)
                                     <*> ZipList (toList r1)
                                     <*> ZipList (toList r2)
                                     <*> ZipList (toList r3)


  return $ ( -- toList $ L4 <$> ZipList (toList l0) <*> ZipList (toList l1) <*> ZipList (toList l2) <*> ZipList (toList l3)
            l, r
--            , toList $ L4 <$> ZipList (toList r0) <*> ZipList (toList r1) <*> ZipList (toList r2) <*> ZipList (toList r3)
           )


keyCols ∷ (MonadIO μ, Traversable ψ, Traversable φ) ⇒
          ψ (φ (𝕊,𝕊,𝕊,𝕊,𝕊)) → μ (ψ (φ (Diagram B)))
keyCols = mapM (mapM (key ∘ mkKey))

keyCols_ ∷ (MonadReader (Fonts 𝔻) η, Traversable ψ, Traversable φ) ⇒
          ψ (φ (𝕊,𝕊,𝕊,𝕊,𝕊)) → η (ψ (φ (Diagram B)))
keyCols_ = mapM (mapM $ key' ∘ mkKey)

keyCols' ∷ (MonadIO μ, MonadFail μ) ⇒ μ ([[Diagram B]], [[Diagram B]])
keyCols' = do
  (l,r) ← lrCols
  l'    ← keyCols l
  r'    ← keyCols r
  return (l',r')

keyCols_' ∷ (MonadIO μ, MonadFail μ, MonadReader (Fonts 𝔻) μ) ⇒
            [FilePath] → μ ([[Diagram B]], [[Diagram B]])
keyCols_' fns = do
  (l,r) ← lrCols_ fns
  l'    ← keyCols_ l
  r'    ← keyCols_ r
  return (l',r')

_keyCols ∷ (MonadReader (Fonts 𝔻) η, Traversable ψ, Traversable φ) ⇒
           ψ (φ KeySpec) → η (ψ (φ (Diagram B)))
_keyCols = mapM (mapM $ key' ∘ mkKey')

type DiagramB = Diagram B

keyCols_'' ∷ (MonadIO μ, MonadError AtreusLayoutE μ, MonadReader (Fonts 𝔻) μ) ⇒
             [FilePath] → μ (L6 (L4 DiagramB), L6 (L4 DiagramB))
keyCols_'' fns = do
  (l,r) ← lrCols_'' fns
  l'    ← _keyCols l
  r'    ← _keyCols r
  return (l',r')

