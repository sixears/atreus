{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- base --------------------------------

import Control.Applicative     ( (<**>), some )
import Control.Monad           ( (>>=) )
import Control.Monad.IO.Class  ( liftIO )
import Data.Function           ( ($) )
import Data.Functor            ( (<$>) )
import System.IO               ( FilePath, IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- diagrams-lib ------------------------

import Diagrams.TwoD.Size  ( mkWidth )

-- diagrams-svg ------------------------

import Diagrams.Backend.SVG          ( renderPretty )
import Diagrams.Backend.SVG.CmdLine  ( mainWith )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( fullDesc, header, info, metavar, progDesc
                                    , strArgument )
import Options.Applicative.Extra    ( execParser, helper )
import Options.Applicative.Types    ( Parser, ParserInfo )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Atreus.LayoutDiagram  ( atreus_layout )

--------------------------------------------------------------------------------

data Options = Options { layerFns ∷ [FilePath] }

parseOpts ∷ Parser Options
parseOpts = Options <$> some (strArgument $ metavar "LAYER-FILE+")

main ∷ IO ()
main = do
  let parser ∷ ParserInfo Options
      parser = info (parseOpts <**> helper)
                    (fullDesc ⊕ progDesc "Print a greeting for TARGET"
                              ⊕ header "hello - a test for optparse" )
  opts ← execParser parser
--  liftIO ∘ mainWith $ atreus_layout (layerFns opts)
  atreus_layout (layerFns opts) >>= renderPretty "atreus.svg" (mkWidth 400)

-- that's all, folks! ----------------------------------------------------------
