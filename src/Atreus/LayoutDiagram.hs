{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
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

import Control.Monad           ( mapM, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False ) )
import Data.Function           ( ($), flip )
import Data.Monoid             ( mconcat, (<>) )
import Data.String             ( String )
import System.IO               ( IO )
import Text.Read               ( Read )

-- colour ------------------------------

import Data.Colour        ( Colour )
import Data.Colour.Names  ( blue, green, grey, red, yellow )

-- diagrams-core -----------------------

import Diagrams.Core            ( Diagram )
import Diagrams.Core.HasOrigin  ( moveOriginBy )
import Diagrams.Core.Transform  ( transform )

-- diagrams-lib ------------------------

import Diagrams.Angle             ( Angle, (@@), deg, rotation )
import Diagrams.Attributes        ( lw, none )
import Diagrams.TwoD.Align        ( alignBL, alignBR, alignTL, alignTR
                                  , centerXY, snugL, snugR )
import Diagrams.TwoD.Attributes   ( fc )
import Diagrams.TwoD.Combinators  ( hsep, vsep )
import Diagrams.TwoD.Path         ( strokeP )
import Diagrams.TwoD.Shapes       ( roundedRect )
import Diagrams.TwoD.Types        ( V2( V2 ) )
import Diagrams.Util              ( (#) )

-- diagrams-svg ------------------------

import Diagrams.Backend.SVG.CmdLine  ( B )

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

type 𝔻 = Double
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

    return $ mconcat [ box1, t0', t1', t2', t3', t4' ]

col0 ∷ [(𝕊,𝕊,𝕊,𝕊,𝕊)]
col0 = [ ("Q",   "!", "Ins"  , "", "' \"")
       , ("A",   "(", "Del"  , "", "a A")
       , ("Z",   "[", ""     , "", "; :")
       , ("Esc", "" , "Upper", "",  "")
       ]

col1 ∷ [(𝕊,𝕊,𝕊,𝕊,𝕊)]
col1 = [ ("W",   "@"   , "Home"  , "", "' \"")
       , ("S",   "←"   , ""  , "", "a A")
       , ("X",   "]"   , "Vol+"     , "", "; :")
       , ("Tab", "Ins" , "Vol-", "",  "")
       ]

atreus_layout ∷ IO (Diagram B)
atreus_layout = do
  fonts ← getFonts @𝔻
  flip runReaderT fonts $ do
--    ks ← mapM (key_rot (-10@@deg)) layout
    ks0 ← mapM key col0
    ks1 ← mapM key col1
    return $ vsep 0.1 ks0 # transform (rotation (-10@@deg)) # snugR
           <> vsep 0.1 ks1 # transform (rotation (-10@@deg)) # snugL

   
-- that's all, folks! ----------------------------------------------------------
