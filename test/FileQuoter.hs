module FileQuoter where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

literally :: String -> Q Exp
literally = return . LitE . StringL

lit :: QuasiQuoter
lit = QuasiQuoter { quoteExp  = literally
                  , quotePat  = undefined
                  , quoteType = undefined
                  , quoteDec  = undefined
                  }

litFile :: QuasiQuoter
litFile = quoteFile lit
