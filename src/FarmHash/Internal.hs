{-# LANGUAGE ForeignFunctionInterface #-}
module FarmHash.Internal
    ( cHash
    , cHash32
    , cHash32WithSeed

    , cHash64
    , cHash64WithSeed
    , cHash64WithSeeds

    , cFingerprint32
    , cFingerprint64

    , withByteString
    ) where

import Data.ByteString    (ByteString)
import Data.Word          (Word32, Word64, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr        (Ptr, plusPtr)
import System.IO.Unsafe   (unsafePerformIO)

import qualified Data.ByteString.Internal as BI


foreign import ccall unsafe "farmhash_c.h Hash"
    cHash :: Ptr Word8 -> Int -> Word32

foreign import ccall unsafe "farmhash_c.h Hash32"
    cHash32 :: Ptr Word8 -> Int -> Word32

foreign import ccall unsafe "farmhash_c.h Hash32WithSeed"
    cHash32WithSeed :: Ptr Word8 -> Int -> Word32 -> Word32

foreign import ccall unsafe "farmhash_c.h Hash64"
    cHash64 :: Ptr Word8 -> Int -> Word64

foreign import ccall unsafe "farmhash_c.h Hash64WithSeed"
    cHash64WithSeed :: Ptr Word8 -> Int -> Word64 -> Word64

foreign import ccall unsafe "farmhash_c.h Hash64WithSeeds"
    cHash64WithSeeds :: Ptr Word8 -> Int -> Word64 -> Word64 -> Word64

foreign import ccall unsafe "farmhash_c.h Fingerprint32"
    cFingerprint32 :: Ptr Word8 -> Int -> Word32

foreign import ccall unsafe "farmhash_c.h Fingerprint64"
    cFingerprint64 :: Ptr Word8 -> Int -> Word64

withByteString :: (Ptr Word8 -> Int -> b) -> ByteString -> b
withByteString f (BI.PS fp off len) =
    unsafePerformIO . withForeignPtr fp $ \p ->
        return $! f (p `plusPtr` off) len
