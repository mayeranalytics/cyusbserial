{-|
Module      : System.Cypress.USBSerial.SPI
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

See section 8 of the Cypress USB Serial Library API Guide, Rev 1.0.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE DataKinds                #-}

module System.Cypress.USBSerial.SPI (
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

-------------------------------------------------  3.28 CY_SPI_CONFIG  -------------------------------------------------
-- |Holds the configuration of the SPI module .
--
-- See Also
--
-- * `SpiProtocol`
-- * `getConfig`
-- * `setConfig`
--
-- From: 3.28 CY_SPI_CONFIG
data Config = Config
    { spiConfig'frequency :: Word32 -- ^ SPI clock frequency. IMPORTANT: The frequency range supported by SPI module is 1000(1KHz) to 3000000(3MHz)
    , spiConfig'dataWidth :: Word8 -- ^ Data width in bits. The valid values are from 4 to 16.
    , spiConfig'protocol :: SpiProtocol -- ^ SPI protocol to be used as defined in CY_SPI_PROTOCOL
    , spiConfig'isMsbFirst :: Bool -- ^ false -> least significant bit is sent out first. true -> most significant bit is sent out           first
    , spiConfig'isMaster :: Bool -- ^ false --> Slave mode selected true --> Master mode selected
    , spiConfig'isContinuousMode :: Bool -- ^ true - Slave select line is not toggled for every word, and stays asserted during the entire           transaction. false- Slave select line is toggled after each word of data is transferred.
    , spiConfig'isSelectPrecede :: Bool -- ^ Valid only when protocol is CY_SPI_TI. true - The start pulse precedes the first data false - The start pulse is in sync with first           data.
    , spiConfig'isCpha :: Bool -- ^ false - Clock phase is 0 true - Clock phase is 1.
    , spiConfig'isCpol :: Bool -- ^ false - Clock polarity is 0 true - Clock polarity is 1.
    } deriving (Show, Eq)

instance Storable Config where
    sizeOf _  = {#sizeof  CY_SPI_CONFIG#}
    alignment _ = {#alignof CY_SPI_CONFIG#}
    peek p = Config
           <$> liftM fromIntegral ({#get CY_SPI_CONFIG->frequency #} p) -- UINT32
           <*> liftM fromIntegral ({#get CY_SPI_CONFIG->dataWidth #} p) -- UCHAR
           <*> liftM toSpiProtocol ({#get CY_SPI_CONFIG->protocol #} p)
           <*> (charToBool <$> peekByteOff p {#offsetof CY_SPI_CONFIG->isMsbFirst #})
           <*> (charToBool <$> peekByteOff p {#offsetof CY_SPI_CONFIG->isMaster #})
           <*> (charToBool <$> peekByteOff p {#offsetof CY_SPI_CONFIG->isContinuousMode #})
           <*> (charToBool <$> peekByteOff p {#offsetof CY_SPI_CONFIG->isSelectPrecede #})
           <*> (charToBool <$> peekByteOff p {#offsetof CY_SPI_CONFIG->isCpha #})
           <*> (charToBool <$> peekByteOff p {#offsetof CY_SPI_CONFIG->isCpol #})
    poke p x = do
        {#set CY_SPI_CONFIG.frequency #} p (fromIntegral $ spiConfig'frequency x) -- UINT32
        {#set CY_SPI_CONFIG.dataWidth #} p (fromIntegral $ spiConfig'dataWidth x) -- UCHAR
        {#set CY_SPI_CONFIG.protocol #} p (toEnum . fromEnum $ spiConfig'protocol x) -- CY_SPI_PROTOCOL
        pokeByteOff p {#offsetof CY_SPI_CONFIG.isMsbFirst #} (boolToChar $ spiConfig'isMsbFirst x) -- BOOL
        pokeByteOff p {#offsetof CY_SPI_CONFIG.isMaster #} (boolToChar $ spiConfig'isMaster x) -- BOOL
        pokeByteOff p {#offsetof CY_SPI_CONFIG.isContinuousMode #} (boolToChar $ spiConfig'isContinuousMode x) -- BOOL
        pokeByteOff p {#offsetof CY_SPI_CONFIG.isSelectPrecede #} (boolToChar $ spiConfig'isSelectPrecede x) -- BOOL
        pokeByteOff p {#offsetof CY_SPI_CONFIG.isCpha #} (boolToChar $ spiConfig'isCpha x) -- BOOL
        pokeByteOff p {#offsetof CY_SPI_CONFIG.isCpol #} (boolToChar $ spiConfig'isCpol x) -- BOOL

toSpiProtocol :: CInt -> SpiProtocol
toSpiProtocol = toEnum . fromEnum

------------------------------------------------  3.30 CY_SPI_PROTOCOL  ------------------------------------------------
-- |Enumeration defining SPI protocol types supported by the SPI module.
--
-- See Also
--
-- * `Config`
-- * `getConfig`
-- * `setConfig`
--
-- From: 3.30 CY_SPI_PROTOCOL
{#enum CY_SPI_PROTOCOL as SpiProtocol {underscoreToCase} with prefix = "CY_SPI_" add prefix = "Spi'" deriving (Eq, Show)#}

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
{#fun unsafe CySetSpiConfig as setConfig
  { id `Handle'          -- ^ Valid device handle
  , withConfig* `Config' -- ^ SPI configuration
  }
  -> `ReturnStatus' toReturnStatus
#}

withConfig :: Config -> (Ptr () -> IO a) -> IO a
withConfig c f = with c (f . castPtr)


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
          -> IO (Either ReturnStatus Config) -- ^ Failed `ReturnStatus` or `Config`
getConfig h = toEither <$> _cyGetSpiConfig h

{#fun unsafe CyGetSpiConfig as _cyGetSpiConfig
  { id `Handle' -- ^ Valid device handle
  , allocaConfig- `Config' peekConfig* -- ^ spiConfig: SPI configuration structure value read back
  }
  -> `ReturnStatus' toReturnStatus
#}

peekConfig :: Ptr () -> IO Config
peekConfig = peek . castPtr

allocaConfig :: (Ptr () -> IO a) -> IO a
allocaConfig f = alloca $ \(p :: Ptr Config) -> f (castPtr p)

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
readWrite :: Handle      -- ^ Valid device handle
          -> ByteString  -- ^ data to write
          -> Word32      -- ^ API timeout value
          -> IO (Either ReturnStatus ByteString) -- ^ Failed `ReturnStatus` or `ByteString` with read data
readWrite h bs t = do
    let l = fromIntegral $ BS.length bs
    withByteString bs $ \pDatW -> 
        allocaDataBuffer l $ \pDatR -> do
            setDataBufferLength pDatR l
            status <- toReturnStatus <$> _cySpiReadWrite'_ (castPtr h) (castPtr pDatR) (castPtr pDatW) (fromIntegral t)
            returnIfSuccess status $ toByteString pDatR

foreign import ccall unsafe "CyUSBSerial.h CySpiReadWrite"
    _cySpiReadWrite'_ :: Ptr () -- ^ Valid device handle
                      -> Ptr () -- ^ Read data buffer
                      -> Ptr () -- ^ Write data buffer
                      -> CUInt  -- ^ Time out value of the API
                      -> IO CInt

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
read :: Handle      -- ^ Valid device handle
     -> Word32      -- ^ read length
     -> Word32      -- ^ API timeout value
     -> IO (Either ReturnStatus ByteString) -- ^ Failed `ReturnStatus` or `ByteString` with read data
read h l t = genericRead h l t call
    where call h r t = _cySpiReadWrite'_ h r nullPtr t


--------------------------------------------------  specialised write ---------------------------------------------------
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
write :: Handle      -- ^ Valid device handle
      -> ByteString  -- ^ data to write
      -> Word32      -- ^ API timeout value
      -> IO (Either ReturnStatus Word32) -- ^ Failed `ReturnStatus` or number of bytes written
write h bs t = genericWrite h bs t call 
    where call h w t = _cySpiReadWrite'_ h nullPtr w t
