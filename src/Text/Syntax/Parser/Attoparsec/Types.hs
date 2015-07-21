module Text.Syntax.Parser.Attoparsec.Types (runResult, runIResult) where

import qualified Data.Attoparsec.Types as A
import           Data.Monoid
import qualified Text.Syntax.Poly      as S

runResult :: (Monoid tsc) => A.IResult tsc a -> Either ([String], String) a
runResult r' = case r' of
  A.Fail _ estack msg -> Left (estack, msg)
  A.Partial f         -> runResult (f mempty)
  A.Done _ r          -> Right r

runIResult :: A.IResult tsc a -> S.IResult tsc a ([String], String)
runIResult r' = case r' of
  A.Fail tks estack msg -> S.Fail tks (estack, msg)
  A.Partial f           -> S.Partial $ runIResult . f
  A.Done tks r          -> S.Done tks r
