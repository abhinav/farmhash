module FarmHashSpec (spec) where

import Test.Hspec
import Control.Applicative
import Data.ByteString (ByteString)
import Test.QuickCheck
import Control.Monad
import Test.Hspec.QuickCheck

import qualified Data.ByteString as B

import FarmHash

byteStringOfLength :: NonNegative Int -> IO ByteString
byteStringOfLength (NonNegative l) = generate $ B.pack <$> vector l

spec :: Spec
spec = do
    checkBasic [
        ("hash", hash)
      , ("hash32", hash32)
      , ("fingerprint32", fingerprint32)
      ] 
    checkBasic [("hash64", hash64), ("fingerprint64", fingerprint64)]
    checkSeeded "hash32WithSeed" hash32WithSeed
    checkSeeded "hash64WithSeed" hash64WithSeed

    describe "hash64WithSeeds" $ do
        prop "operates on bytestrings" $ \len seed1 seed2 -> do
            s <- byteStringOfLength len
            hash64WithSeeds s seed1 seed2 `shouldSatisfy` (> 0)

        prop "operates on subsrings" $ \len seed1 seed2 tak drp -> do
            s <- byteStringOfLength len
            hash64WithSeeds (B.take tak (B.drop drp s)) seed1 seed2
                `shouldSatisfy` (> 0)
  where
    checkBasic h = forM_ h $ \(name, func) -> describe name $ do
        prop "operates on bytestrings" $ \len -> do
            s <- byteStringOfLength len
            func s `shouldSatisfy` (> 0)

        prop "operates on substrings" $ \tak drp len -> do
            s <- byteStringOfLength len
            func (B.take tak (B.drop drp s))
                `shouldSatisfy` (> 0)

    checkSeeded name func = describe name $ do
        prop "operates on bytestrings" $ \len seed -> do
            s <- byteStringOfLength len
            func s seed `shouldSatisfy` (> 0)

        prop "operates on subsrings" $ \len seed tak drp -> do
            s <- byteStringOfLength len
            func (B.take tak (B.drop drp s)) seed
                `shouldSatisfy` (> 0)
