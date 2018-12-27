{-|
Module      : System.Cypress.USBSerial.JTAG
Description : JTAG part of the Cypress USBSeral library
Copyright   : (c) Markus Mayer
License     : LGPL-2.1
Maintainer  : mmayer@mayeranalytics.com
Stability   : experimental
Portability : non-portable (needs dll/so/dylib)

[JTAG](https://en.wikipedia.org/wiki/JTAG) is an industry standard for 
that also specifies a serial debug port communication for verifying testing
printed circuit boards.
It is supported by [CY7C65211](http://www.cypress.com/documentation/datasheets/cy7c65211-usb-serial-single-channel-uarti2cspi-bridge-capsense-and-bcd)
and [CY7C65215](http://www.cypress.com/documentation/datasheets/cy7c65215-usb-serial-dual-channel-uarti2cspi-bridge-capsense-and-bcd).

See section 9 of the Cypress USB Serial Library API Guide, Rev 1.0.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE DataKinds                #-}

module System.Cypress.USBSerial.JTAG (
    -- * Enable and disable
    enable,
    disable,
    -- * Reading and writing
    read,
    write
) where

import           Prelude hiding (read, write)
import           Foreign
import           Foreign.C.Types
import           Foreign.Storable
import           Foreign.C.String
import           System.Cypress.USBSerial.Internal.USBSerial
import           Control.Monad (liftM, when)
import           Data.Char (ord)
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Data.Bits  (shift, (.&.), (.|.))

#include "CyUSBSerial.h"

------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------  JTAG API  ------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------  9.1 CyJtagEnable  --------------------------------------------------
-- |Enables the JTAG module.
--
-- All other functions are disabled until `disable`.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid in case of Linux/Mac.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if request was timed out.
-- * `ErrorRequestFailed` if request was failed by device or if device type is not JTAG.
--
-- See Also
--
-- * `disable`
--
-- From: 9.1 CyJtagEnable
{#fun unsafe CyJtagEnable as enable
  { id `Handle' -- ^ Valid device handle
  }
  -> `ReturnStatus' toReturnStatus
#}

-------------------------------------------------  9.2 CyJtagDisable  --------------------------------------------------
-- |Disables the Jtag interface. 
--
-- `disable` must be invoked before exiting the application if `enable` was previously invoked.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid in case of Linux/Mac.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if request was timed out.
-- * `ErrorRequestFailed` if request was failed by device or if device type is not JTAG.
--
-- See Also
--
-- * `enable`
--
-- From: 9.2 CyJtagDisable
{#fun unsafe CyJtagDisable as disable
  { id `Handle' -- ^ Valid device handle
  }
  -> `ReturnStatus' toReturnStatus
#}

---------------------------------------------------  9.3 CyJtagRead  ---------------------------------------------------
-- |Reads data.
--
-- Returns data, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid in case of Linux/Mac.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorRequestFailed` if device type is not JTAG or when encountered unknown libusb errors in Linux/Mac.
-- * `ErrorIoTimeout` if transfer was timed out.
-- * `ErrorDeviceNotFound` if device was disconnected.
-- * `ErrorBufferOverflow` if data received from USB Serial device is more than requested.
--
-- See Also
--
-- * `write`
-- * `enable`
--
-- From: 9.3 CyJtagRead
read :: Handle      -- ^ Valid device handle
     -> Word32      -- ^ read length
     -> Word32      -- ^ API timeout value
     -> IO (Either ReturnStatus ByteString) -- ^ Failed `ReturnStatus` or `ByteString` with read data
read h l t = genericRead h l t _cyJtagRead'_

-- |The readBuffer need to be initialized with buffer
-- and length of data to be written before invoking the API. Upon return, transferCount field in `CY_DATA_BUFFER
-- `structure is updated with actual number of bytes read.
--
foreign import ccall unsafe "CyUSBSerial.h CyJtagRead"
  _cyJtagRead'_ :: Ptr () -- ^ Valid device handle
                -> Ptr () -- ^ Read buffer details
                -> CUInt  -- ^ API timeout value
                -> IO CInt

--------------------------------------------------  9.4 CyJtagWrite  ---------------------------------------------------
-- |Write data.
--
-- Returns number of bytes written, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid in case of Linux/Mac.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorRequestFailed` if device type is not JTAG or when encountered unknown libusb errors in Linux/Mac.
-- * `ErrorPipeHalted` if there was any pipe error during transaction.
-- * `ErrorIoTimeout` if transfer was timed out.
-- * `ErrorDeviceNotFound` if device was disconnected.
--
-- See Also
--
-- * `read`
-- * `enable`
--
-- From: 9.4 CyJtagWrite
write :: Handle      -- ^ Valid device handle
      -> ByteString  -- ^ data to write
      -> Word32      -- ^ API timeout value
      -> IO (Either ReturnStatus Word32) -- ^ Failed `ReturnStatus` or number of bytes written
write h bs t = genericWrite h bs t _cyI2cWrite'_

-- |The writeBuffer needs
-- to be initialized with buffer and length of data to be written before invoking the API. Upon return, transferCount
-- field in `DataBuffer` is updated with actual number of bytes written.
--
foreign import ccall unsafe "CyUSBSerial.h CyI2cWrite"
  _cyI2cWrite'_ :: Ptr () -- ^ Valid device handle
                -> Ptr () -- ^ Read buffer details
                -> CUInt  -- ^ API timeout value
                -> IO CInt