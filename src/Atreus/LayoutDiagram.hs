{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UnicodeSyntax             #-}

module Atreus.LayoutDiagram
  ( atreus_layout )
where

--------------------------------------------------------------------------------

import Prelude  ( Double, RealFloat )

-- base --------------------------------

import Control.Monad           ( return )
import Data.Bool               ( Bool( False ) )
import Data.Function           ( ($), flip )
import Data.Monoid             ( mconcat )
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

import Diagrams.Angle             ( (@@), deg, rotation )
import Diagrams.Attributes        ( lw, none )
import Diagrams.TwoD.Align        ( alignBL, alignBR, alignTL, alignTR
                                  , centerXY )
import Diagrams.TwoD.Attributes   ( fc )
import Diagrams.TwoD.Combinators  ( (|||) )
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

data Fonts ν = Fonts { lin2 ∷ PreparedFont ν }

------------------------------------------------------------

box ∷ Diagram B
box = roundedRect 1 1 0.05

topts_ ∷ MonadReader (Fonts ν) η ⇒ ν → ν → η (TextOpts ν)
topts_ w h = do
  l2 ← asks lin2
  return TextOpts { textFont = l2, mode = INSIDE_H, spacing = KERN
                  , underline = False, textWidth = w, textHeight = h }

getFonts ∷ (Read ν, RealFloat ν) ⇒ IO (Fonts ν)
getFonts = do
  l2 ← SF.lin2
  return $ Fonts { lin2 = l2 }

ss ∷ TextOpts 𝔻 → 𝕊 → Diagram B
ss o t = strokeP $ textSVG' o t

xx' ∷ MonadReader (Fonts 𝔻) η ⇒ 𝔻 → 𝔻 → 𝕊 → η (Diagram B)
xx' h w t = do
  o ← topts_ w h
  return $ ss o t # lw none

tx'' ∷ MonadReader (Fonts 𝔻) η ⇒
      𝔻 → 𝔻 → 𝕊 → Colour 𝔻 → 𝔻 → 𝔻 → (Diagram B → Diagram B) → η (Diagram B)
tx'' h w t c x y a = do
  t' ← xx' h w t
  return (moveOriginBy (V2 x y) $ t' # fc c # a)

tx' ∷ MonadReader (Fonts 𝔻) η ⇒
      𝕊 → Colour 𝔻 → 𝔻 → 𝔻 → (Diagram B → Diagram B) → η (Diagram B)
tx' = tx'' 0.35 0.5

atreus_layout ∷ IO (Diagram B)
atreus_layout = do
  fonts ← getFonts
  flip runReaderT fonts $ do
    t0' ← tx'' 0.5 0.5 "Q" grey 0 0 centerXY
    t1' ← tx' "PgUp" red (-0.45) (-0.45) alignTR
    t2' ← tx' "Ins" green (-0.45) 0.45 alignBR
    t3' ← tx' "7" blue 0.45 (-0.45) alignTL
    t4' ← tx' "TAB" yellow 0.45 0.45 alignBL

    return $
          mconcat [ box, t0', t1', t2', t3', t4' ] # transform (rotation $ -10@@deg)
      ||| (mconcat [ box, t0', t1', t2', t3', t4' ]) # transform (rotation $ -10@@deg)

-- that's all, folks! ----------------------------------------------------------
