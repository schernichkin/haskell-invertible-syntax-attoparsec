{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Syntax.Parser.Attoparsec.Text (
  runAsAttoparsec', runAsAttoparsec,
  runAsIAttoparsec'
  ) where

import           Data.Attoparsec.Types                (Parser)
import           Text.Syntax.Parser.Instances         ()
import           Text.Syntax.Poly                     (RunAsIParser,
                                                       RunAsParser, Syntax (..), TryAlternative (try, (<|>)),
                                                       (<||>))

import qualified Data.Attoparsec.Text                 as A (anyChar, parse, try)
import qualified Data.Attoparsec.Text.Lazy            as L
import           Data.Text                            (Text)
import qualified Data.Text.Lazy                       as L (Text)
import           Text.Syntax.Parser.Attoparsec.RunResult (runIResult, runResult)

instance TryAlternative (Parser Text) where
  try = A.try
  p <|> q = try p <||> q

instance Syntax Char (Parser Text) where
  token = A.anyChar

runAsAttoparsec' :: RunAsParser Char Text a ([String], String)
runAsAttoparsec' parser tks = runResult $ A.parse parser tks where

runAsAttoparsec :: RunAsParser Char L.Text a ([String], String)
runAsAttoparsec parser tks =
  case L.parse parser tks of
    L.Fail _ estack msg -> Left (estack, msg)
    L.Done _ r          -> Right r

runAsIAttoparsec' :: RunAsIParser Char Text a ([String], String)
runAsIAttoparsec' parser tks = runIResult $ A.parse parser tks
