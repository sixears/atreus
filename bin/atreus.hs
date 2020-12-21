{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

import Prelude  ( fromIntegral )

-- base --------------------------------

import Control.Applicative  ( (<*>), (<**>), some )
import Data.Function        ( ($) )
import Data.Functor         ( (<$>) )
import System.IO            ( FilePath, IO )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode      ( (⊕) )
import Numeric.Natural.Unicode  ( ℕ )

-- diagrams-lib ------------------------

import Diagrams.TwoD.Size  ( mkWidth )

-- diagrams-svg ------------------------

import Diagrams.Backend.SVG  ( renderPretty )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( auto, flag, fullDesc, help, info, long
                                    , metavar, option, progDesc, short
                                    , strArgument, strOption, value
                                    )
import Options.Applicative.Extra    ( execParser, helper )
import Options.Applicative.Types    ( Parser, ParserInfo )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Atreus.LayoutDiagram  ( LayoutRemap( REMAP_NONE, REMAP_DVORAK )
                             , atreus_layout )

--------------------------------------------------------------------------------

data Options = Options { layerFns ∷ [FilePath]
                       , outputFn ∷ FilePath
                       , width    ∷ ℕ
                       , remap    ∷ LayoutRemap
                       }

parseOpts ∷ Parser Options
parseOpts =
  let
    dvorak_help = help "remap to dvorak layout"
  in
    Options <$> some (strArgument $ metavar "LAYER-FILE+")
            <*> strOption (short 'o' ⊕ long "output"
                                     ⊕ metavar "OUTPUT-FILE")
            <*> option auto (short 'w' ⊕ value 400)
            <*> flag REMAP_NONE REMAP_DVORAK (short 'D' ⊕ long "dvorak"
                                                        ⊕ dvorak_help)

main ∷ IO ()
main = do
  let prog_desc = progDesc "create an atreus layout svg from layer descriptions"
      parser ∷ ParserInfo Options
      parser = info (parseOpts <**> helper) (fullDesc ⊕ prog_desc)
  opts ← execParser parser
  dia ← atreus_layout (layerFns opts) (remap opts)
  renderPretty (outputFn opts) (mkWidth (fromIntegral $ width opts)) dia

-- that's all, folks! ----------------------------------------------------------
