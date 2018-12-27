{-|
Module      : System.Cypress.Safe.USBSerial.JTAG
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

This is the 'safe' version of `System.Cypress.USBSerial.JTAG.JTAG`.

See section 9 of the Cypress USB Serial Library API Guide, Rev 1.0.
-}

module System.Cypress.Safe.USBSerial.JTAG (
    -- * Enable and disable
    enable,
    disable,
    -- * Reading and writing
    read,
    write
) where

    
import           Prelude hiding (read, write)
import           Control.Monad.Reader (lift, ask, liftIO)
import           Control.Error.Safe (tryRight)
import           Control.Error.Util hiding (tryIO)
import           System.Cypress.USBSerial.JTAG hiding (read, write, enable, disable)
import qualified System.Cypress.USBSerial.JTAG as Cy
import           System.Cypress.Safe.USBSerial
import           System.Cypress.Safe.USBSerial.Internal.Utils
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import           Data.Word

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
disable ::  USBWithHandle ReturnStatus
disable = do
    h <- lift ask
    liftIO $ Cy.disable h

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
enable ::  USBWithHandle ReturnStatus
enable = do
    h <- lift ask
    liftIO $ Cy.enable h

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
read :: Word32      -- ^ read length
     -> Word32      -- ^ API timeout value
     ->  USBWithHandle (Either ReturnStatus ByteString) -- ^ Failed `ReturnStatus` or `ByteString` with read data
read l t = do
    h <- lift ask
    liftIO $ Cy.read h l t

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
write :: ByteString  -- ^ data to write
      -> Word32      -- ^ API timeout value
      -> USBWithHandle (Either ReturnStatus Word32) -- ^ Failed `ReturnStatus` or number of bytes written
write bs t = do
    h <- lift ask
    liftIO $ Cy.write h bs t
    