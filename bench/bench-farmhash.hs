{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Criterion.Main
import Data.Word      (Word32, Word64)

import qualified Data.ByteString as B

import FarmHash

main :: IO ()
main = defaultMain $ do
    -- Try strings of length 8 to 4096 in exponents of 8.
    stringLength <- take 4 . drop 1 $ iterate (*16) 1

    let string = B.replicate stringLength 120
        seed32 = fromIntegral stringLength :: Word32
        seed64 = fromIntegral stringLength :: Word64
    string `seq` seed32 `seq` seed64 `seq` return ()

    return $ bgroup ("length:" ++ show stringLength) [
        bgroup "32-bit" [
            bench "hash" $ whnf hash string
          , bench "hash32" $ whnf hash32 string
          , bench "hash32WithSeed" $ whnf (hash32WithSeed string) seed32
          , bench "fingerprint32" $ whnf fingerprint32 string
          ]
      , bgroup "64-bit" [
            bench "hash64" $ whnf hash64 string
          , bench "hash64WithSeed" $ whnf (hash64WithSeed string) seed64
          , bench "hash64WithSeeds" $
              whnf (hash64WithSeeds string seed64) seed64
          , bench "fingerprint64" $ whnf fingerprint64 string
          ]
      ]
