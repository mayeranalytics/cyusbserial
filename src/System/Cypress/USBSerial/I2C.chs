{-|
Module      : System.Cypress.USBSerial.I2C
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

See section 7 of the Cypress USB Serial Library API Guide, Rev 1.0.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE DataKinds                #-}

module System.Cypress.USBSerial.I2C (
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
-----------------------------------------------------  Data Types  -----------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------  3.24 CY_I2C_CONFIG  -------------------------------------------------
-- |Holds the configuration of the I2C module.
--
-- Use `getConfig` and `setConfig` to retrieve and configure the I2C module.
--
-- See Also
--
-- * `getConfig`
-- * `setConfig`
--
-- From: 3.24 CY_I2C_CONFIG
data Config = Config
    { i2cConfig'frequency :: Word32    -- ^ I2C clock frequency 1KHz to 400KHz
    , i2cConfig'slaveAddress :: Word8  -- ^ Slave address of the I2C module, when it is configured as slave
    , i2cConfig'isMaster :: Bool       -- ^ Set to True if USB Serial is I2C Master, False if USB Serial is I2C slave.
    , i2cConfig'isClockStretch :: Bool -- ^ true: Stretch clock in case of no data availability (valid only for slave mode). false: Do not           use clock stretching.
    } deriving (Show, Eq)

instance Storable Config where
    sizeOf _  = {#sizeof  CY_I2C_CONFIG#}
    alignment _ = {#alignof CY_I2C_CONFIG#}
    peek p = Config
           <$> liftM fromIntegral ({#get CY_I2C_CONFIG->frequency #} p) -- UINT32
           <*> liftM fromIntegral ({#get CY_I2C_CONFIG->slaveAddress #} p) -- UINT8
           <*> {#get CY_I2C_CONFIG->isMaster #} p -- BOOL
           <*> {#get CY_I2C_CONFIG->isClockStretch #} p -- BOOL
    poke p x = do
        {#set CY_I2C_CONFIG.frequency #} p (fromIntegral $ i2cConfig'frequency x) -- UINT32
        {#set CY_I2C_CONFIG.slaveAddress #} p (fromIntegral $ i2cConfig'slaveAddress x) -- UINT8
        {#set CY_I2C_CONFIG.isMaster #} p (i2cConfig'isMaster x) -- BOOL
        {#set CY_I2C_CONFIG.isClockStretch #} p (i2cConfig'isClockStretch x) -- BOOL

----------------------------------------------  3.26 CY_I2C_DATA_CONFIG  -----------------------------------------------
-- |Holds the configuration of each I2C data transaction.
--
-- This structure is needed for each data transaction. It includes setting slave address (when device is in I2C slave mode), 
-- stopbit and Nak bit.
--
-- See Also
-- * `write`
-- * `read`
--
-- From: 3.26 CY_I2C_DATA_CONFIG
data DataConfig = DataConfig
    { i2cDataConfig'slaveAddress :: Word8 -- ^ Slave address the master will communicate with
    , i2cDataConfig'isStopBit :: Bool     -- ^ Set when stop bit is used
    , i2cDataConfig'isNakBit :: Bool      -- ^ Set when I2C master wants to NAK the slave after read. Applicable only when doing I2C read
    } deriving (Show, Eq)

instance Storable DataConfig where
    sizeOf _  = {#sizeof  CY_I2C_DATA_CONFIG#}
    alignment _ = {#alignof CY_I2C_DATA_CONFIG#}
    peek p = DataConfig
           <$> liftM fromIntegral ({#get CY_I2C_DATA_CONFIG->slaveAddress #} p) -- UCHAR
           <*> (charToBool <$> peekByteOff p {#offsetof CY_I2C_DATA_CONFIG->isStopBit #})
           <*> (charToBool <$> peekByteOff p {#offsetof CY_I2C_DATA_CONFIG->isNakBit #})
    poke p x = do
        {#set CY_I2C_DATA_CONFIG.slaveAddress #} p (fromIntegral $ i2cDataConfig'slaveAddress x) -- UCHAR
        pokeByteOff p {#offsetof CY_I2C_DATA_CONFIG.isStopBit #} (boolToChar $ i2cDataConfig'isStopBit x) -- BOOL
        pokeByteOff p {#offsetof CY_I2C_DATA_CONFIG.isNakBit #} (boolToChar $ i2cDataConfig'isNakBit x) -- BOOL


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
{#fun unsafe CySetI2cConfig as setConfig
  { id `Handle'          -- ^ Valid device handle
  , withConfig* `Config' -- ^ I2C configuration
  }
  -> `ReturnStatus' toReturnStatus
#}

withConfig :: Config -> (Ptr () -> IO a) -> IO a
withConfig c f = with c (f . castPtr)

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
getConfig :: Handle                          -- ^ Valid device handle
          -> IO (Either ReturnStatus Config) -- ^ Failed `ReturnStatus` or `Config`
getConfig h = toEither <$> _cyGetI2cConfig h

{#fun unsafe CyGetI2cConfig as _cyGetI2cConfig
  { id `Handle'                  -- ^ Valid device handle
  , allocaConfig- `Config' peekConfig* -- ^ I2C configuration
  }
  -> `ReturnStatus' toReturnStatus
#}

peekConfig :: Ptr () -> IO Config
peekConfig = peek . castPtr

allocaConfig :: (Ptr () -> IO a) -> IO a
allocaConfig f = alloca $ \(p :: Ptr Config) -> f (castPtr p)


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
read :: Handle      -- ^ Valid device handle
     -> Config      -- ^ I2C configuration value
     -> Word32      -- ^ read length
     -> Word32      -- ^ API timeout value
     -> IO (Either ReturnStatus ByteString) -- ^ Failed `ReturnStatus` or `ByteString` with read data
read h c l t = 
    allocaDataBuffer (fromIntegral l) $ \pDat -> 
    with c $ \pConf -> do
        setDataBufferLength pDat l
        status <- toReturnStatus <$> _cyI2cRead'_ (castPtr h) (castPtr pConf) (castPtr pDat) (fromIntegral t)
        returnIfSuccess status $ toByteString pDat

-- |The readBuffer
-- parameter needs to be initialized with buffer pointer, number of bytes to be read before invoking the API. On
-- return, the transferCount field will contain the number of bytes read back from device. `CY_I2C_DATA_CONFIG
-- `structure specifies parameters such as setting stop bit, NAK and slave address of the I2C device.
--
foreign import ccall unsafe "CyUSBSerial.h CyI2cRead"
  _cyI2cRead'_ :: Ptr () -- ^ Valid device handle
               -> Ptr () -- ^ i2cConfig: I2C configuration value read back
               -> Ptr () -- ^ Read buffer details
               -> CUInt  -- ^ API timeout value
               -> IO CInt

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
write :: Handle      -- ^ Valid device handle
      -> Config      -- ^ I2C configuration value
      -> ByteString  -- ^ data to write
      -> Word32      -- ^ API timeout value
      -> IO (Either ReturnStatus Word32) -- ^ Failed `ReturnStatus` or number of bytes written
write h c bs t = 
    withByteString bs $ \pDat -> 
    with c $ \pConf -> do
        status <- toReturnStatus <$> _cyI2cWrite'_ (castPtr h) (castPtr pConf) (castPtr pDat) (fromIntegral t)
        returnIfSuccess status $ getDataBufferTransferCount pDat

-- |The writeBuffer parameter
-- needs to be initialized with buffer pointer, number of bytes to be written before invoking the API. On return,
-- transferCount field contains number of bytes actually written to the device. `CY_I2C_DATA_CONFIG
-- `structure specifies parameter such as setting stop bit, Nak and slave address of the I2C device being
-- communicated when USB Serial is master.
--
foreign import ccall unsafe "CyUSBSerial.h CyI2cWrite"
  _cyI2cWrite'_ :: Ptr () -- ^ Valid device handle
                -> Ptr () -- ^ i2cConfig: I2C configuration value read back
                -> Ptr () -- ^ Read buffer details
                -> CUInt  -- ^ API timeout value
                -> IO CInt


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
{#fun unsafe CyI2cReset as reset
  { id `Handle' -- ^ Valid device handle
  , `Bool'      -- ^ resetMode: Reset mode
  }
  -> `ReturnStatus' toReturnStatus
#}

