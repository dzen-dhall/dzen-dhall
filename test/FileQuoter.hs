module FileQuoter where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified GHC.IO.Encoding
import qualified System.IO

literally :: String -> Q Exp
literally s = do
  runIO $
    GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
  return . LitE . StringL $ s

lit :: QuasiQuoter
lit = QuasiQuoter { quoteExp  = literally
                  , quotePat  = undefined
                  , quoteType = undefined
                  , quoteDec  = undefined
                  }

litFile :: QuasiQuoter
litFile = quoteFile lit
