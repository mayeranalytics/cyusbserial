{-|
Module      : System.Cypress.Safe.USBSerial.I2C
Description : I2C part of the Cypress USBSeral library
Copyright   : (c) Markus Mayer
License     : LGPL-2.1
Maintainer  : mmayer@mayeranalytics.com
Stability   : experimental
Portability : non-portable (needs dll/so/dylib)

[I2C](https://en.wikipedia.org/wiki/I%C2%B2C) is a synchronous, multi-master, multi-slave serial bus.
It is supported by [CY7C65211](http://www.cypress.com/documentation/datasheets/cy7c65211-usb-serial-single-channel-uarti2cspi-bridge-capsense-and-bcd)
and [CY7C65215](http://www.cypress.com/documentation/datasheets/cy7c65215-usb-serial-dual-channel-uarti2cspi-bridge-capsense-and-bcd).

* Single-channel configurable I2C interface
* Master/slave up to 400 kHz
* 256 bytes each transmit and receive buffer
* Supports multi-master I2C

This is the `safe` version of `System.Cypress.USBSerial.I2C.I2C`.

See section 7 of the Cypress USB Serial Library API Guide, Rev 1.0.
-}

module System.Cypress.Safe.USBSerial.I2C (
    -- * Types
    Config(..),
    DataConfig(..),
    -- * Configuration setting and getting
    setConfig,
    getConfig,
    -- * Reading and writing
    read,
    write,
    -- * Reset
    reset
) where

import           Prelude hiding (read, write)
import           Control.Monad.Reader (lift, ask, liftIO)
import           Control.Error.Safe (tryRight)
import           Control.Error.Util hiding (tryIO)
import           System.Cypress.USBSerial.I2C hiding (setConfig, getConfig, read, write, reset)
import qualified System.Cypress.USBSerial.I2C as Cy
import           System.Cypress.Safe.USBSerial
import           System.Cypress.Safe.USBSerial.Internal.Utils
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import           Data.Word
    
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------  I2C API  -------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------  7.1 CySetI2cConfig  -------------------------------------------------
-- |Configure the I2C module.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device or when device type is not I2C.
--
-- See Also
--
-- * `Config`
-- * `getConfig`
--
-- From: 7.1 CySetI2cConfig
setConfig :: Config -- ^ I2C configuration
          ->  USBWithHandle ReturnStatus
setConfig c = do
    h <- lift ask
    liftIO $ Cy.setConfig h c
   
-------------------------------------------------  7.2 CyGetI2cConfig  -------------------------------------------------
-- |Retrieve the configuration of I2C module.
--
-- Returns `Config`, or on failure:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device or when device type is not I2C.
--
-- See Also
--
-- * `Config`
-- * `setConfig`
--
-- From: 7.2 CyGetI2cConfig
getConfig ::  USBWithHandle (Either ReturnStatus Config) -- ^ Failed `ReturnStatus` or `Config`
getConfig = do
    h <- lift ask
    liftIO $ Cy.getConfig h
            
---------------------------------------------------  7.3 CyI2cRead  ----------------------------------------------------
-- |Read data.
--
-- Returns data, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if input parameters were invalid.
-- * `ErrorRequestFailed` if the device type is not I2C
-- * `ErrorIoTimeout` if transfer was timed out.
-- * `ErrorDeviceNotFound` if device was disconnected.
-- * `ErrorBufferOverflow` if data received from USB Serial device is more than requested. 
-- * `ErrorAllocationFailed` if transaction transmit buffer allocation was failed (Only in Windows). 
-- * `ErrorI2cDeviceBusy` if I2C device was busy processing previous request.  
-- * `ErrorI2cNakError` if request was nacked by I2C device.
-- * `ErrorI2cArbitrationError` if a I2C bus arbitration error occured.
-- * `ErrorI2cBusError` if there was any I2C bus error while an on going transaction.
-- * `ErrorI2cStopBitSet` if stop bit was set by I2C master.
--
--
-- See Also
--
-- * `DataConfig`
-- * `write`
--
-- From: 7.3 CyI2cRead
read :: Config      -- ^ I2C configuration value
     -> Word32      -- ^ read length
     -> Word32      -- ^ API timeout value
     ->  USBWithHandle (Either ReturnStatus ByteString) -- ^ Failed `ReturnStatus` or `ByteString` with read data
read c l t = do
    h <- lift ask
    liftIO $ Cy.read h c l t

---------------------------------------------------  7.4 CyI2cWrite  ---------------------------------------------------
-- |Write data.
--
-- Returns number of bytes written, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if input parameters were invalid.
-- * `ErrorRequestFailed` if the device type is not I2C
-- * `ErrorIoTimeout` if transfer was timed out.
-- * `ErrorPipeHalted` if pipe was stalled during data transfer.
-- * `ErrorDeviceNotFound` if device was disconnected.
-- * `ErrorBufferOverflow` if data received from USB Serial device is more than requested.
-- * `ErrorAllocationFailed` if transaction transmit buffer allocation was failed (Only in Windows).
-- * `ErrorI2cDeviceBusy` if I2C device was busy processing previous request.
-- * `ErrorI2cNakError` if request was nacked by I2C device.
-- * `ErrorI2cArbitrationError` if a I2C bus arbitration error occured.
-- * `ErrorI2cBusError` if there was any I2C bus error while an on going transaction.
-- * `ErrorI2cStopBitSet` if stop bit was set by I2C master.
--
-- See Also
--
-- * `CY_DATA_BUFFER`
-- * `CY_I2C_DATA_CONFIG`
-- * `CyI2cRead`
--
-- From: 7.4 CyI2cWrite
write :: Config      -- ^ I2C configuration value
      -> ByteString  -- ^ data to write
      -> Word32      -- ^ API timeout value
      -> USBWithHandle (Either ReturnStatus Word32) -- ^ Failed `ReturnStatus` or number of bytes written
write c bs t = do
    h <- lift ask
    liftIO $ Cy.write h c bs t

---------------------------------------------------  7.5 CyI2cReset  ---------------------------------------------------
-- |Reset the I2C module.
--
-- Do this whenever there is an error in a data transaction. 
-- 
-- A resetMode of
--
-- * `False` resets the __read__ module,
-- * `True` resets the __write__ module.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device or when device type is not I2C.
--
-- See Also
--
-- * `CyI2cRead`
-- * `CyI2cWrite`
--
-- From: 7.5 CyI2cReset
reset :: Bool      -- ^ resetMode: Reset mode
      ->  USBWithHandle (Either ReturnStatus ())
reset r = do
    h <- lift ask
    throwUnlessSuccess $ liftIO $ Cy.reset h r
        
