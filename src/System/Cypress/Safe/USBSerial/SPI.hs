{-|
Module      : System.Cypress.Safe.USBSerial.SPI
Description : SPI part of the Cypress USBSeral library
Copyright   : (c) Markus Mayer
License     : LGPL-2.1
Maintainer  : mmayer@mayeranalytics.com
Stability   : experimental
Portability : non-portable (needs dll/so/dylib)

[SPI](https://en.wikipedia.org/wiki/Serial_Peripheral_Interface) is a 
full duplex, single master synchronous serial communication interface.
It is supported by [CY7C65211](http://www.cypress.com/documentation/datasheets/cy7c65211-usb-serial-single-channel-uarti2cspi-bridge-capsense-and-bcd)
and [CY7C65215](http://www.cypress.com/documentation/datasheets/cy7c65215-usb-serial-dual-channel-uarti2cspi-bridge-capsense-and-bcd).

* Data rate up to 3 MHz for SPI master and 1 MHz for SPI slave
* Data width: 4 bits to 16 bits
* 256 bytes for each transmit and receive buffer
* Supports Motorola, TI, and National SPI modes

This is the `safe` version of `System.Cypress.USBSerial.SPI.SPI`.

See section 8 of the Cypress USB Serial Library API Guide, Rev 1.0.
-}

module System.Cypress.Safe.USBSerial.SPI (
    -- * Types
    Config(..),
    SpiProtocol(..),
    -- * Configuration setting and getting
    setConfig,
    getConfig,
    -- * Reading and writing
    readWrite,
    read,
    write
) where

import           Prelude hiding (read, write)
import           Control.Monad.Reader (lift, ask, liftIO)
import           Control.Error.Safe (tryRight)
import           Control.Error.Util hiding (tryIO)
import           System.Cypress.USBSerial.SPI hiding (setConfig, getConfig, read, write, readWrite)
import qualified System.Cypress.USBSerial.SPI as Cy
import           System.Cypress.Safe.USBSerial
import           System.Cypress.Safe.USBSerial.Internal.Utils
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import           Data.Word

------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------  SPI API  -------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------  8.1 CySetSpiConfig  -------------------------------------------------
-- |Set the configuration of the SPI module.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device or when device type is not SPI.
--
-- See Also
--
-- * `Config`
-- * `getConfig`
--
-- From: 8.1 CySetSpiConfig
setConfig :: Config -- ^ SPI configuration
          -> USBWithHandle (Either ReturnStatus ())
setConfig c = do
    h <- lift ask
    throwUnlessSuccess $ liftIO $ Cy.setConfig h c

-------------------------------------------------  8.2 CyGetSpiConfig  -------------------------------------------------
-- |Retrieve the configuration of the SPI module.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device or when device type is not SPI.
--
-- See Also
--
-- * `Config`
-- * `setConfig`
--
-- From: 8.2 CyGetSpiConfig
getConfig :: Handle                          -- ^ Valid device handle
          ->  USBWithHandle (Either ReturnStatus Config) -- ^ Failed `ReturnStatus` or `Config`
getConfig h = do
    h <- lift ask
    liftIO $ Cy.getConfig h

-------------------------------------------------  8.3 CySpiReadWrite  -------------------------------------------------
-- |Read *and* write data to the SPI device.
--
-- To perform read only operation, pass NULL as argument for writeBuffer and to perform write only operation pass NULL as
-- an argument for readBuffer. On return, the transferCount field will contain the number of bytes read and/or
-- written.
--
-- Returns data, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid in case of Linux/Mac.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorRequestFailed` if the device type is not SPI or when libusb reported unknown error in case of Linux/Mac.
-- * `ErrorIoTimeout` if transfer was timed out.
-- * `ErrorPipeHalted` if pipe was stalled during data transfer.
-- * `ErrorDeviceNotFound` if device was disconnected.
-- * `ErrorBufferOverflow` if data received from USB Serial device is more than requested.
--
-- See Also
--
-- * `getConfig`
-- * `setConfig`
--
-- From: 8.3 CySpiReadWrite
readWrite :: ByteString  -- ^ data to write
          -> Word32      -- ^ API timeout value
          ->  USBWithHandle (Either ReturnStatus ByteString) -- ^ Failed `ReturnStatus` or `ByteString` with read data
readWrite bs t = do
    h <- lift ask
    liftIO $ Cy.readWrite h bs t

--------------------------------------------------  specialised read  --------------------------------------------------
-- |Only read from SPI.
--
-- Returns data, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid in case of Linux/Mac.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorRequestFailed` if the device type is not SPI or when libusb reported unknown error in case of Linux/Mac.
-- * `ErrorIoTimeout` if transfer was timed out.
-- * `ErrorPipeHalted` if pipe was stalled during data transfer.
-- * `ErrorDeviceNotFound` if device was disconnected.
-- * `ErrorBufferOverflow` if data received from USB Serial device is more than requested.
read :: Word32      -- ^ read length
     -> Word32      -- ^ API timeout value
     -> USBWithHandle (Either ReturnStatus ByteString) -- ^ Failed `ReturnStatus` or `ByteString` with read data
read l t = do
    h <- lift ask
    liftIO $ Cy.read h l t

-------------------------------------------------- specialised write  ---------------------------------------------------
-- |Only write to SPI.
--
-- Returns number of bytes written, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid in case of Linux/Mac.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorRequestFailed` if the device type is not SPI or when libusb reported unknown error in case of Linux/Mac.
-- * `ErrorIoTimeout` if transfer was timed out.
-- * `ErrorPipeHalted` if pipe was stalled during data transfer.
-- * `ErrorDeviceNotFound` if device was disconnected.
-- * `ErrorBufferOverflow` if data received from USB Serial device is more than requested.
write :: ByteString  -- ^ data to write
      -> Word32      -- ^ API timeout value
      -> USBWithHandle (Either ReturnStatus Word32) -- ^ Failed `ReturnStatus` or number of bytes written
write bs t = do
    h <- lift ask
    liftIO $ Cy.write h bs t
