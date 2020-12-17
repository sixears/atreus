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

import Options.Applicative.Builder  ( auto, fullDesc, info, long, metavar
                                    , option, progDesc, short, strArgument
                                    , strOption, value
                                    )
import Options.Applicative.Extra    ( execParser, helper )
import Options.Applicative.Types    ( Parser, ParserInfo )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Atreus.LayoutDiagram  ( atreus_layout )

--------------------------------------------------------------------------------

data Options = Options { layerFns ∷ [FilePath]
                       , outputFn ∷ FilePath
                       , width    ∷ ℕ
                       }

parseOpts ∷ Parser Options
parseOpts = Options <$> some (strArgument $ metavar "LAYER-FILE+")
                    <*> strOption (short 'o' ⊕ long "output"
                                             ⊕ metavar "OUTPUT-FILE")
                    <*> option auto (short 'w' ⊕ value 400)

main ∷ IO ()
main = do
  let prog_desc = progDesc "create an atreus layout svg from layer descriptions"
      parser ∷ ParserInfo Options
      parser = info (parseOpts <**> helper) (fullDesc ⊕ prog_desc)
  opts ← execParser parser
  dia ← atreus_layout (layerFns opts)
  renderPretty (outputFn opts) (mkWidth (fromIntegral $ width opts)) dia

-- that's all, folks! ----------------------------------------------------------
