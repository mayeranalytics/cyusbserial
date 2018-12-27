{-|
Module      : System.Cypress.USBSerial.Extras
Description : Some functions that are not part of the original API.
Copyright   : (c) Markus Mayer
License     : LGPL-2.1
Maintainer  : mmayer@mayeranalytics.com
Stability   : experimental
Portability : non-portable (needs dll/so/dylib)

Here we have various functions that are not part of the original API.

- Reading and writing of the config flash.

__Note:__ `progConfigFlash` reconfigures the device, use this function with great care! 
__It is very possible that you brick your device!__ So make sure you only program known good config sets.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE DataKinds                #-}

module System.Cypress.USBSerial.Extras (
    -- * Config flash reading and writing - use with care!
    readConfigFlash,
    progConfigFlash
) where

import           Foreign
import           Foreign.C.Types
import           Foreign.Storable
import           Foreign.C.String
import           Data.Char (ord)
import           Data.Int
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString, useAsCStringLen, packCStringLen)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackCStringLen)
import           Data.Bits  (shift, (.&.), (.|.))
import           System.Cypress.USBSerial.Internal.USBSerial
import           Control.Monad.Except

#include "extras.h"

------------------------------------------------  CyReadConfigFlash  ---------------------------------------------------
-- |Read the config flash. This is a "safe" operation in the sense that it won't brick your device.
--
-- Returns a `ByteString` of length 512 containing the config data, or
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device.
--
-- See Also
--
-- * `progConfigFlash`
--
readConfigFlash :: Handle -- ^ Valid device handle
                -> Word32 -- ^ Timeout value of the API
                -> IO (Either ReturnStatus ByteString)  -- ^ Failed `ReturnStatus` or `DataBuffer`
readConfigFlash h t = allocaBytes {#const CONFIG_FLASH_SIZE#} $ \p -> do
    status <- toReturnStatus <$> _cyReadConfigFlash'_ h p (fromIntegral t)
    returnIfSuccess status $ packCStringLen (castPtr p, {#const CONFIG_FLASH_SIZE#})

foreign import ccall unsafe "extras.h CyReadConfigFlash"
    _cyReadConfigFlash'_ :: Ptr ()    -- ^ Valid device handle
                         -> Ptr ()    -- ^ data buffer containing enough memory for config flash
                         -> CUInt     -- ^ Timeout value of the API
                         -> IO CInt

------------------------------------------------  CyProgConfigFlash  ------------------------------------------------
-- |Write the config flash. The `ByteString` must have length 512 and __must contain a good device configuration set__.
--
-- __Use with extreme caution!__ It is very possible that you brick your device with an inappropriate config set!
--
-- Again: __Be careful or you will brick your device!__
--
-- Returns `Success` or:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device.
-- * `ErrorOther` when the `ByteString` does not have length 512
--
-- See Also
--
-- * `readConfigFlash`
--
progConfigFlash :: Handle -- ^ Valid device handle
                -> ByteString -- ^ `ByteString` of length 512 containing the config data
                -> Word32 -- ^ Timeout value of the API
                -> IO ReturnStatus
progConfigFlash h bs t = 
    useAsCStringLen bs $ \(p, l) -> 
        if (l /= {#const CONFIG_FLASH_SIZE#})
            then return (ErrorOther "config flash size is not 512")
            else toReturnStatus <$> _cyProgConfigFlash'_ h (castPtr p) (fromIntegral t)

foreign import ccall unsafe "extras.h CyProgConfigFlash"
    _cyProgConfigFlash'_ :: Ptr ()    -- ^ Valid device handle
                         -> Ptr ()    -- ^ data buffer containing enough memory for config flash
                         -> CUInt     -- ^ Timeout value of the API
                         -> IO CInt
