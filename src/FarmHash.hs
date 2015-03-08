-- |
-- Module      :  FarmHash
-- Copyright   :  (c) Abhinav Gupta 2015
-- License     :  BSD3
--
-- Maintainer  :  Abhinav Gupta <mail@abhinavg.net>
-- Stability   :  experimental
--
-- FarmHash is a family of very fast hash functions. They are not suitable for
-- cryptography.
--
-- See <https://code.google.com/p/farmhash/>.
module FarmHash (

    -- * Hash

    -- | These functions are platform-__dependent__. Their output may be
    -- different on different platforms for the same string.

      hash
    , hash32
    , hash32WithSeed

    , hash64
    , hash64WithSeed
    , hash64WithSeeds

    -- * Fingerprint

    -- | These functions are platform-__independent__. They are guaranteed to
    -- produce the same output for the same string on different platforms.

    , fingerprint32
    , fingerprint64
    ) where

import Data.ByteString (ByteString)
import Data.Word       (Word32, Word64)

import FarmHash.Internal

-- | Hash function for a byte array.
--
-- May change from time to time, and may differ on platforms.
hash :: ByteString -> Word32
hash = withByteString cHash

-- | Hash function for a byte array. Most useful in 32-bit binaries.
--
-- May change from time to time, and may differ on platforms.
hash32 :: ByteString -> Word32
hash32 = withByteString cHash32

-- | Hash function for a byte array. For convenience, a 32-bit seed is also
-- hashed into the result.
--
-- May change from time to time, and may differ on platforms.
hash32WithSeed :: ByteString -> Word32 -> Word32
hash32WithSeed = withByteString cHash32WithSeed

-- | Hash function for a byte array.
--
-- May change from time to time, and may differ on platforms.
hash64 :: ByteString -> Word64
hash64 = withByteString cHash64

-- | Hash function for a byte array. For convenience, a 64-bit seed is also
-- hashed into the result.
--
-- May change from time to time, and may differ on platforms.
hash64WithSeed :: ByteString -> Word64 -> Word64
hash64WithSeed = withByteString cHash64WithSeed

-- | Hash function for a byte array. For convenience, two seeds are also
-- hashed into the result.
--
-- May change from time to time, and may differ on platforms.
hash64WithSeeds :: ByteString -> Word64 -> Word64 -> Word64
hash64WithSeeds = withByteString cHash64WithSeeds

-- | Fingerprint function for a byte array. Most useful in 32-bit binaries.
--
-- Portable, forever-fixed hash function.
fingerprint32 :: ByteString -> Word32
fingerprint32 = withByteString cFingerprint32

-- | Fingerprint function for a byte array.
--
-- Portable, forever-fixed hash function.
fingerprint64 :: ByteString -> Word64
fingerprint64 = withByteString cFingerprint64
