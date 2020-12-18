{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Atreus.LayoutDiagram
  ( atreus_layout )
where

--------------------------------------------------------------------------------

import Prelude  ( Double, RealFloat, (*) )

-- aeson -------------------------------

import Data.Aeson  ( eitherDecodeFileStrict' )

-- base --------------------------------

import Control.Applicative     ( Applicative( (<*>) ), ZipList( ZipList ) )
import Control.Monad           ( (>>=), join, mapM, return, sequence )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False ) )
import Data.Either             ( Either( Left, Right ), either )
import Data.Foldable           ( all, length, toList )
import Data.Function           ( ($), (&) )
import Data.Functor            ( Functor( fmap ), (<$>) )
import Data.List               ( lookup, repeat, reverse, take )
import Data.Maybe              ( Maybe, fromMaybe, maybe )
import Data.Monoid             ( Monoid, mconcat, mempty )
import Data.Ord                ( (>) )
import Data.String             ( String )
import GHC.Float               ( Floating )
import System.Exit             ( ExitCode( ExitFailure ), exitWith )
import System.IO               ( FilePath, IO, hPutStrLn, stderr )
import Text.Read               ( Read )
import Text.Show               ( show )

-- base-unicode-symbols ----------------

import Prelude.Unicode          ( (÷) )
import Data.Eq.Unicode          ( (≡) )
import Data.Function.Unicode    ( (∘) )
import Data.Monoid.Unicode      ( (⊕) )

-- colour ------------------------------

import Data.Colour        ( Colour )
import Data.Colour.Names  ( blue, green, grey, red, yellow )

-- data-default ------------------------

import Data.Default  ( def )

-- data-monotraversable ----------------

import Data.MonoTraversable  ( MonoFoldable( otoList ) )

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
import Diagrams.TwoD.Align        ( alignBL, alignBR, alignTL, alignTR
                                  , centerXY )
import Diagrams.TwoD.Attributes   ( fc )
import Diagrams.TwoD.Path         ( strokeP )
import Diagrams.TwoD.Shapes       ( roundedRect )
import Diagrams.TwoD.Size         ( width )
import Diagrams.TwoD.Transform    ( translationY )
import Diagrams.TwoD.Types        ( V2( V2 ) )
import Diagrams.Util              ( (#), with )

-- diagrams-svg ------------------------

import Diagrams.Backend.SVG.CmdLine  ( B )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
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

-- text-printer ------------------------

import qualified Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Atreus.Types  ( AtreusBoardSpecT( AtreusBoardSpec ), AtreusBoardSpec
                     , AtreusKeySpecsT( AtreusKeySpecs )
                     , AtreusKeySpecs
                     , AtreusLayerSpec, Board
                     , KeyColT( KeyCol ), KeyCol
                     , KeyRow( KeyRow )
                     , KeyLabelsT( KeyLabels ), KeyLabels
                     , atreusLayerEmpty, fullLabel, readBoard
                     )
import FixedList     ( AsL4( l4 ), AsL6( l6 ), AsL8( l8 ), L4, L6( L6 )
                     , L8( L8 ), groupL6, l5map )

--------------------------------------------------------------------------------

type 𝔻 = Double
type 𝕄 = Maybe
type 𝕊 = String

type DiagramB = Diagram B

------------------------------------------------------------

{- | Various errors that may occur. -}

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

------------------------------------------------------------

data Fonts ν = Fonts { lin ∷ PreparedFont ν }

{- | Read in a Fonts datum -}
getFonts ∷ (Read ν, RealFloat ν) ⇒ IO (Fonts ν)
getFonts = do
  l ← SF.lin
  return $ Fonts { lin = l }

------------------------------------------------------------

{- | A width-one square with slightly rounded corners. -}
box1 ∷ DiagramB
box1 = roundedRect 1 1 0.05

----------------------------------------

{- | Like `vsep`, but going upwards rather than downwards. -}

vsup ∷ (Floating (N δ), Juxtaposable δ, Monoid δ, HasOrigin δ, V δ ~ V2) ⇒
       N δ -> [δ] -> δ
vsup s = cat' (V2 0 1) (def & sep .~ s)

----------------------------------------

{- | Like `hcat`, but separates by origin rather than envelope (see `Distrib`).
 -}
htac ∷ (Floating (N δ), Juxtaposable δ, Monoid δ, HasOrigin δ, V δ ~ V2) ⇒
       N δ -> [δ] -> δ
htac d = cat' (V2 1 0) (with & catMethod .~ Distrib & sep .~ d)

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

{- | Create a text diagram of given height and maximum width (using the `SF.lin`
     font).  The height will be scaled down if using the requested height would
     cause the width to be exceeded.
 -}
text ∷ MonadReader (Fonts 𝔻) η ⇒ 𝔻 → 𝔻 → 𝕊 → η DiagramB
text h w t = do
  o ∷ TextOpts 𝔻 ← topts INSIDE_H h 1 -- the width is irrelevant with INSIDE_H
  let dia ∷ DiagramB = strokeP (textSVG' o t) # lw none
  if w > width dia
  then return $ strokeP (textSVG' o t) # lw none
  else do o' ← topts INSIDE_H (h * w ÷ width dia) 1
          let dia' = strokeP (textSVG' o' t) # lw none
          return dia'
  
----------------------------------------

replacements ∷ [(𝕊,𝕊)]
replacements = [ ("ShiftTo 0", "⓪")
               , ("ShiftTo 1", "①")
               , ("ShiftTo 2", "②")
               , ("ShiftTo 3", "③")
               , ("ShiftTo 4", "④")
               , ("ShiftTo 5", "⑤")
               , ("ShiftTo 6", "⑥")
               , ("ShiftTo 7", "⑦")
               , ("ShiftTo 8", "⑧")
               , ("ShiftTo 9", "⑨")
               , ("ShiftTo 10", "⑩")
               , ("MoveTo 0", "⓿")
               ]
-- ⓪ ① ② ③ ④ ⑤ ⑥ ⑦ ⑧ ⑨ ⑩ ⑪ ⑫ ⑬ ⑭ ⑮ ⑯ ⑰ ⑱ ⑲ ⑳
-- ⓿ ❶ ❷ ❸ ❹ ❺ ❻ ❼ ❽ ❾ ❿ ⓫ ⓬ ⓭ ⓮ ⓯ ⓰ ⓱ ⓲ ⓳ ⓴ 

{- | Create a text diagram of given height (using the `SF.lin` font);
     with given height, colour, alignment; and position. -}
text' ∷ MonadReader (Fonts 𝔻) η ⇒
      𝔻 → 𝔻 → 𝕊 → Colour 𝔻 → 𝔻 → 𝔻 → (DiagramB → DiagramB) → η DiagramB
text' h w t c x y a = do
  t' ∷ DiagramB ← text h w (t `fromMaybe` (t `lookup` replacements))
  return (moveOriginBy (V2 x y) $ t' # fc c # a)

----------------------------------------

{- | Read & parse an atreus layer file -}
decode ∷ (MonadIO μ, MonadError AtreusLayoutE μ) ⇒ FilePath → μ AtreusLayerSpec
decode = let ethrow = either (throwError ∘ AtreusFailedDecodeE) return
          in join ∘ liftIO ∘ fmap ethrow ∘ eitherDecodeFileStrict'

----------------------------------------

{- | Create an atreus keyboard definition from a list of up to 5 layers.
     If fewer than 5 layers are provided, the remainder will be made up of
     empty layers.
     If more than 5 layers are provided, will throw an `AtreusTooManyLayers`
     exception.
 -}
boardFromLayers ∷ MonadError AtreusLayoutE η ⇒
                  [AtreusLayerSpec] → η AtreusBoardSpec
boardFromLayers ls =
  if length ls > 5
  then throwError $ AtreusTooManyLayers ls
  else let [layer0,layer1,layer2,layer3,layer4] =
             take 5 $ ls ⊕ repeat atreusLayerEmpty
        in return $ AtreusBoardSpec layer0 layer1 layer2 layer3 layer4

----------------------------------------

{- | Read a list of (up to 5) layers, to create a board (missing layers will
     be treated as empty. -}
decodes ∷ (MonadIO μ, MonadError AtreusLayoutE μ) ⇒
          [FilePath] → μ AtreusBoardSpec
decodes fns = mapM decode fns >>= boardFromLayers

----------------------------------------

{- | A list of keys, over 5 layers as read from a list of files.  Only keys that
     are represented on all layers are returned. -}
board ∷ (MonadIO μ, MonadError AtreusLayoutE μ) ⇒ [FilePath] → μ [AtreusKeySpecs]
board fns =
    (\ (AtreusBoardSpec layer0 layer1 layer2 layer3 layer4) → 
      toList $ AtreusKeySpecs <$> ZipList (otoList layer0)
                              <*> ZipList (otoList layer1)
                              <*> ZipList (otoList layer2)
                              <*> ZipList (otoList layer3)
                              <*> ZipList (otoList layer4)) <$> decodes fns

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
  t0 ← text' 0.5  0.4 (kblank c)  grey    0       0     centerXY
  t1 ← text' 0.35 0.4 (kblank tr) red   (-0.45) (-0.45) alignTR
  t2 ← text' 0.35 0.4 (kblank br) blue  (-0.45)   0.45  alignBR
  t3 ← text' 0.35 0.4 (kblank tl) green   0.45  (-0.45) alignTL
  t4 ← text' 0.35 0.4 (kblank bl) yellow  0.45    0.45  alignBL

  return $ if isNull k
           then mempty
           else mconcat [ box1, t0, t1, t2, t3, t4 ]

----------------------------------------

fmap2 ∷ (Functor ψ, Functor φ) ⇒ (α → β) → ψ (φ α) → ψ (φ β)
fmap2 = fmap ∘ fmap

(<$$>) ∷ (Functor ψ, Functor φ) ⇒ (α → β) → ψ (φ α) → ψ (φ β)
(<$$>) = fmap2

----------------------------------------

{- | Group keys into 6s. -}
group6Keys ∷ (MonadIO μ, MonadError AtreusLayoutE μ) ⇒
             [FilePath] → μ [L6 AtreusKeySpecs]
group6Keys = join ∘ fmap (groupL6 AtreusWrongKeyCount) ∘ board

----------------------------------------

{- | Read some layer files, group the keys together into 8 rows of 6 each. -}
lrRows ∷ (MonadIO μ, MonadError AtreusLayoutE μ) ⇒ [FilePath] → μ Board
lrRows fns = do
  keyspecss ∷ [L6 AtreusKeySpecs] ← group6Keys fns
  let keylabelss ∷ [L6 KeyLabels] = l5map fullLabel <$$> keyspecss
      maybeE ∷ MonadError AtreusLayoutE η ⇒ [L6 KeyLabels] → 𝕄 α → η α
      maybeE ls = maybe (throwError $ AtreusWrongRowCount ls) return
  maybeE keylabelss $ readBoard (KeyRow <$> keylabelss)

----------------------------------------

{- | Two (6-long) lists of (4-high) columns of keys, as diagrams; split into
     left & right.
 -}
lrCols ∷ (MonadReader (Fonts 𝔻) η) ⇒
          Board → η (L6 (L4 DiagramB),L6 (L4 DiagramB))
lrCols b = do
  -- each of l0,r0,…,r3 is ∷ L6 KeyLabels
  let L8 l0 r0 l1 r1 l2 r2 l3 r3 = view l6 <$> view l8 b

  let l ∷ L6 KeyCol = KeyCol <$> l0 <*> l1 <*> l2 <*> l3
      r ∷ L6 KeyCol = KeyCol <$> r0 <*> r1 <*> r2 <*> r3

  l' ← sequence $ fmap (mapM key ∘ view l4) l
  r' ← sequence $ fmap (mapM key ∘ view l4) r
  return (l',r')

------------------------------------------------------------

makeLayout ∷ MonadReader (Fonts 𝔻) η ⇒ Board → η DiagramB
makeLayout b =  do
  (l,r) ← lrCols b

  let (L6 lt0 lt1 lt2 lt3 lt4 lt5) = l
      (L6 rt0 rt1 rt2 rt3 rt4 rt5) = r

  let lrot = -10@@deg
      rrot =  10@@deg
      place ks y rot = vsup 0.1 (reverse $ toList ks)
                                                  # transform (translationY y)
                                                  # transform (rotation rot)

  return $ htac (1.2 ÷ cosA lrot)
                (htac 0.5 [[ place lt0 0      lrot
                           , place lt1 0      lrot
                           , place lt2 0      lrot
                           , place lt3 (-0.5) lrot
                           , place lt4 (-1.0) lrot
                           , place lt5 (-1.0) lrot
                           ]
                          ,[ place rt0 (-1.0) rrot
                           , place rt1 (-1.0) rrot
                           , place rt2 (-0.5) rrot
                           , place rt3 0      rrot
                           , place rt4 0      rrot
                           , place rt5 0      rrot
                           ]
                          ])

atreus_layout ∷ [FilePath] → IO DiagramB
atreus_layout fns = do
  fonts ← getFonts

  runExceptT (runReaderT (lrRows fns >>= makeLayout) fonts) >>= \ case
    Right r → return r
    Left  e → do hPutStrLn stderr (toString e)
                 exitWith (ExitFailure 255)

-- that's all, folks! ----------------------------------------------------------
