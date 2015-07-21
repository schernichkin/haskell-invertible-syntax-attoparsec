{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Syntax.Parser.Attoparsec.ByteString (
  runAsAttoparsec', runAsAttoparsec,
  runAsAttoparsecChar8', runAsAttoparsecChar8,
  runAsIAttoparsec',
  runAsIAttoparsecChar8'
  ) where

import           Text.Syntax.Parser.Attoparsec.RunResult (runIResult, runResult)
import           Text.Syntax.Parser.Instances        ()
import           Text.Syntax.Poly                    (RunAsIParser, RunAsParser,
                                                      Syntax (..), TryAlternative (try, (<|>)),
                                                      (<||>))


import qualified Data.Attoparsec.ByteString          as A (anyWord8, parse, try)
import qualified Data.Attoparsec.ByteString.Char8    as A (anyChar)
import qualified Data.Attoparsec.ByteString.Lazy     as L
import           Data.Attoparsec.Types               (Parser)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Lazy                as L (ByteString)

import           Data.Word                           (Word8)

instance TryAlternative (Parser ByteString) where
  try = A.try
  p <|> q = try p <||> q

instance Syntax Word8 (Parser ByteString) where
  token = A.anyWord8

runAsAttoparsec' :: RunAsParser Word8 ByteString a ([String], String)
runAsAttoparsec' parser tks = runResult $ A.parse parser tks

runAsAttoparsec :: RunAsParser Word8 L.ByteString a ([String], String)
runAsAttoparsec parser tks =
  case L.parse parser tks of
    L.Fail _ estack msg -> Left (estack, msg)
    L.Done _ r          -> Right r


instance Syntax Char (Parser ByteString) where
  token = A.anyChar

runAsAttoparsecChar8' :: RunAsParser Char ByteString a ([String], String)
runAsAttoparsecChar8' parser tks = runResult $ A.parse parser tks

runAsAttoparsecChar8 :: RunAsParser Char L.ByteString a ([String], String)
runAsAttoparsecChar8 parser tks =
  case L.parse parser tks of
    L.Fail _ estack msg -> Left (estack, msg)
    L.Done _ r          -> Right r

runAsIAttoparsec' :: RunAsIParser Word8 ByteString a ([String], String)
runAsIAttoparsec' parser tks = runIResult $ A.parse parser tks

runAsIAttoparsecChar8' :: RunAsIParser Char ByteString a ([String], String)
runAsIAttoparsecChar8' parser tks = runIResult $ A.parse parser tks

