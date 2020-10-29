{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- base --------------------------------

import Control.Monad.IO.Class  ( liftIO )
import Data.Function           ( ($) )
import System.IO               ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- diagrams-svg ------------------------

import Diagrams.Backend.SVG.CmdLine  ( mainWith )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Atreus.LayoutDiagram  ( atreus_layout )

--------------------------------------------------------------------------------

main ∷ IO ()
main = liftIO ∘ mainWith $ atreus_layout

-- that's all, folks! ----------------------------------------------------------
