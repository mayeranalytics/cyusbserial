{-|
Module      : System.Cypress.USBSerial.UART
Description : UART part of the Cypress USBSeral library
Copyright   : (c) Markus Mayer
License     : LGPL-2.1
Maintainer  : mmayer@mayeranalytics.com
Stability   : experimental
Portability : non-portable (needs dll/so/dylib)

[UART](https://en.wikipedia.org/wiki/Universal_asynchronous_receiver-transmitter) is A universal asynchronous
serial communication device.
It is supported by all Cypress USB bridging chips.

* Data rates up to 3 Mbps
* 190 bytes for each transmit and receive buffer
* Supports 2-pin,4-pin and 6-pin UART interface
* 7 to 8 data bits
* 1 to 2 stop bits
* Parity: none, even, odd, mark, space
* Supports parity, overrun, and framing errors
* Supports flow control using CTS, RTS, DTR, DSR
* Supports UART break signal

See section 6 of the Cypress USB Serial Library API Guide, Rev 1.0.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE DataKinds                #-}

module System.Cypress.USBSerial.UART (
    -- * Types
    Config(..),
    UartBaudRate(..),
    UartParity(..),
    UartStopBit(..),
    FlowControl(..),
    -- * Configuration setting and getting
    setConfig,
    getConfig,
    -- * Reading and writing
    read,
    write,
    -- * Parameter setting and getting
    setDtr,
    clearDtr,
    setRts,
    clearRts,
    setHwFlowControl,
    getHwFlowControl,
    setBreak  
)
where

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

------------------------------------------------  3.18 CY_UART_CONFIG  -------------------------------------------------
-- |Holds the configuration of the UART module.
--
-- Use `setConfig` and `getConfig` APIs are used to configure and retrieve the UART configuration information.
--
-- See Also
--
-- * `setConfig`
-- * `getConfig`
--
-- From: 3.18 CY_UART_CONFIG
data Config = Config
    { uartConfig'baudRate :: UartBaudRate -- ^ Baud rate as defined in CY_UART_BAUD_RATE
    , uartConfig'dataWidth :: Word8 -- ^ Data width: valid values 7 or 8
    , uartConfig'stopBits :: UartStopBit -- ^ Number of stop bits to be used 1 or 2
    , uartConfig'parity :: UartParity -- ^ UART parity mode as defined in CY_UART_PARITY_MODE
    , uartConfig'isDropOnRxErrors :: Bool -- ^ Whether to ignore framing as well as parity errors and receive data
    } deriving (Show, Eq)

instance Storable Config where
    sizeOf _  = {#sizeof  CY_UART_CONFIG#}
    alignment _ = {#alignof CY_UART_CONFIG#}
    peek p = Config
           <$> liftM (toEnum . fromEnum) ({#get CY_UART_CONFIG->baudRate #} p) -- CY_UART_BAUD_RATE
           <*> liftM (fromIntegral) ({#get CY_UART_CONFIG->dataWidth #} p) -- UINT8
           <*> liftM (toEnum . fromEnum) ({#get CY_UART_CONFIG->stopBits #} p) -- CY_UART_STOP_BIT
           <*> liftM (toEnum . fromEnum) ({#get CY_UART_CONFIG->parityMode #} p) -- CY_UART_PARITY_MODE
           <*> (charToBool <$> peekByteOff p {#offsetof CY_UART_CONFIG->isDropOnRxErrors #})
    poke p x = do
        {#set CY_UART_CONFIG.baudRate #} p (toEnum . fromEnum $ uartConfig'baudRate x) -- CY_UART_BAUD_RATE
        {#set CY_UART_CONFIG.dataWidth #} p (fromIntegral $ uartConfig'dataWidth x) -- UINT8
        {#set CY_UART_CONFIG.stopBits #} p (toEnum . fromEnum $ uartConfig'stopBits x) -- CY_UART_STOP_BIT
        {#set CY_UART_CONFIG.parityMode #} p (toEnum . fromEnum $ uartConfig'parity x) -- CY_UART_PARITY_MODE
        pokeByteOff p {#offsetof CY_UART_CONFIG.isDropOnRxErrors #} (boolToChar $ uartConfig'isDropOnRxErrors x) -- BOOL


-----------------------------------------------  3.20 CY_UART_BAUD_RATE  -----------------------------------------------
-- |The enumeration defines UART baud rates supported by the UART module.
--
-- See Also
--
-- * `Config`
-- * `setConfig`
-- * `getConfig`
--
-- From: 3.20 CY_UART_BAUD_RATE
{#enum CY_UART_BAUD_RATE as UartBaudRate {underscoreToCase} with prefix = "CY_UART_BAUD_" add prefix = "BaudRate'" deriving (Eq, Show) #}

----------------------------------------------  3.21 CY_UART_PARITY_MODE  ----------------------------------------------
-- |The enumeration defines the different parity modes supported by the UART module.
--
-- It supports odd, even, mark andspace and parity modes.
--
-- See Also
--
-- * `Config`
-- * `setConfig`
-- * `getConfig`
--
-- From: 3.21 CY_UART_PARITY_MODE
{#enum CY_UART_PARITY_MODE as UartParity {underscoreToCase} with prefix = "CY_DATA_PARITY_" add prefix = "Parity'" deriving (Eq, Show)#}

-----------------------------------------------  3.22 CY_UART_STOP_BIT  ------------------------------------------------
-- |The enumeration defines the different stop bit values supported by the UART module.
--
-- See Also
--
-- * `Config`
-- * `setConfig`
-- * `getConfig`
--
-- From: 3.22 CY_UART_STOP_BIT
{#enum CY_UART_STOP_BIT as UartStopBit {underscoreToCase} with prefix = "CY_UART_" add prefix = "StopBit'" deriving (Eq, Show)#}

---------------------------------------------  3.23 CY_FLOW_CONTROL_MODES  ---------------------------------------------
-- |The enumeration defines flow control modes supported by the UART module.
--
-- See Also
--
-- * `uartSetHwFlowControl`
-- * `uartGetHwFlowControl`
--
-- From: 3.23 CY_FLOW_CONTROL_MODES
{#enum CY_FLOW_CONTROL_MODES as FlowControl {underscoreToCase} with prefix = "CY_UART_FLOW_CONTROL_" add prefix = "FlowControl'" deriving (Eq, Show)#}


------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------  UART API  ------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------  6.1 CySetUartConfig  -------------------------------------------------
-- |Set the UART configuration.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device or when device type is not UART.
--
-- See Also
--
-- * `Config`
-- * `getConfig`
--
-- 6.1 CySetUartConfig
{#fun unsafe CySetUartConfig as setConfig
  { id `Handle'           -- ^ Valid device handle
  , withConfig* `Config'  -- ^ UART configuration
  }
  -> `ReturnStatus' toReturnStatus
#}

withConfig :: Config -> (Ptr () -> IO a) -> IO a
withConfig c f = with c (f . castPtr)


------------------------------------------------  6.2 CyGetUartConfig  -------------------------------------------------
-- |Retrieve the UART configuration.
--
-- Returns `Config`, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device or when device type is not UART.
--
-- See Also
--
-- * `Config`
-- * `setConfig`
--
-- From: 6.2 CyGetUartConfig
getConfig :: Handle                           -- ^ Valid device handle
          -> IO (Either ReturnStatus Config)  -- ^ Failed `ReturnStatus` or `Config`
getConfig h = toEither <$> _cyGetUartConfig h

{#fun unsafe CyGetUartConfig as _cyGetUartConfig
  { id `Handle' -- ^ Valid device handle
  , allocaConfig- `Config' peekConfig* -- ^ *uartConfig: UART configuration value read back
  }
  -> `ReturnStatus' toReturnStatus
#}

peekConfig :: Ptr () -> IO Config
peekConfig = peek . castPtr

allocaConfig :: (Ptr () -> IO a) -> IO a
allocaConfig f = alloca $ \(p :: Ptr Config) -> f (castPtr p)

---------------------------------------------------  6.3 CyUartRead  ---------------------------------------------------
-- |Read data from UART device.
--
-- Returns data, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if input parameters were invalid.
-- * `ErrorRequestFailed` if the device type is not UART.
-- * `ErrorIoTimeout` if transfer was timed out.
-- * `ErrorPipeHalted` if pipe was stalled during data transfer.
-- * `ErrorDeviceNotFound` if device was disconnected.
-- * `ErrorBufferOverflow` if data received from USB Serial device is more than requested.
-- * `ErrorAllocationFailed` if transaction transmit buffer allocation was failed (Only in Windows).
--
-- See Also
--
-- * `write`
--
-- From: 6.3 CyUartRead
read :: Handle      -- ^ Valid device handle
     -> Word32      -- ^ read length
     -> Word32      -- ^ API timeout value
     -> IO (Either ReturnStatus ByteString) -- ^ Failed `ReturnStatus` or `ByteString` with read data
read h l t = genericRead h l t _cyUartRead'_

-- |The user has to initialize the readBuffer with buffer pointer,
-- number of bytes to read before invoking this API. On return the transferCount parameter in `CY_DATA_BUFFER
-- `will contain the number of bytes read.
foreign import ccall unsafe "CyUSBSerial.h CyUartRead"
  _cyUartRead'_ :: Ptr () -- ^ Valid device handle
                -> Ptr () -- ^ Read buffer details
                -> CUInt  -- ^ API timeout value
                -> IO CInt
  

--------------------------------------------------  6.4 CyUartWrite  ---------------------------------------------------
-- |Writes the data to UART device.
--
-- Returns number of bytes written, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if input parameters were invalid.
-- * `ErrorRequestFailed` if the device type is not UART.
-- * `ErrorIoTimeout` if transfer was timed out.
-- * `ErrorPipeHalted` if pipe was stalled during data transfer.
-- * `ErrorDeviceNotFound` if device was disconnected.
-- * `ErrorBufferOverflow` if data received from USB Serial device is more than requested.
-- * `ErrorAllocationFailed` if transaction transmit buffer allocation was failed (Only in Windows).
--
-- See Also
--
-- * `read`
--
-- From: 6.4 CyUartWrite
write :: Handle      -- ^ Valid device handle
      -> ByteString  -- ^ data to write
      -> Word32      -- ^ API timeout value
      -> IO (Either ReturnStatus Word32) -- ^ Failed `ReturnStatus` or number of bytes written
write h bs t = genericWrite h bs t _cyUartWrite'_

-- |The user has to initialize the writeBuffer with buffer pointer, number of
-- bytes to write before invoking the API. On return the transferCount parameter in `DataBuffer`
-- will contain the number of bytes written.
--
foreign import ccall unsafe "CyUSBSerial.h CyUartWrite"
  _cyUartWrite'_ :: Ptr () -- ^ Valid device handle
                 -> Ptr () -- ^ Read buffer details
                 -> CUInt  -- ^ API timeout value
                 -> IO CInt

--------------------------------------------------  6.5 CyUartSetDtr  --------------------------------------------------
-- |Set the DTR signal.
--
-- The DTR pin is set to to logical low.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device or when device type is not UART.
--
-- See Also
--
-- * `clearRts`
-- * `setRts`
-- * `clearDtr`
--
-- From: 6.5 CyUartSetDtr
{#fun unsafe CyUartSetDtr as setDtr
  { id `Handle' -- ^ Valid device handle
  }
  -> `ReturnStatus' toReturnStatus
#}

-------------------------------------------------  6.6 CyUartClearDtr  -------------------------------------------------
-- |Clear DTR signal.
--
-- The DTR pin is set to logical high.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device or when device type is not UART.
--
-- See Also
--
-- * `setRts`
-- * `setDtr`
-- * `clearRts`
--
-- From: 6.6 CyUartClearDtr
{#fun unsafe CyUartClearDtr as clearDtr
  { id `Handle' -- ^ Valid device handle
  }
  -> `ReturnStatus' toReturnStatus
#}

--------------------------------------------------  6.7 CyUartSetRts  --------------------------------------------------
-- |Set the RTS signal.
--
-- This API is used to set the RTS pin to logical low..
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device or when device type is not UART.
--
-- See Also
--
-- * `clearRts`
-- * `setDtr`
-- * `clearDtr`
--
-- From: 6.7 CyUartSetRts
{#fun unsafe CyUartSetRts as setRts
  { id `Handle' -- ^ Valid device handle
  }
  -> `ReturnStatus' toReturnStatus
#}

-------------------------------------------------  6.8 CyUartClearRts  -------------------------------------------------
-- |Clear RTS signal.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device or when device type is not UART.
--
-- See Also
--
-- * `setRts`
-- * `setDtr`
-- * `clearDtr`
--
-- From: 6.8 CyUartClearRts
{#fun unsafe CyUartClearRts as clearRts
  { id `Handle' -- ^ Valid device handle
  }
  -> `ReturnStatus' toReturnStatus
#}

---------------------------------------------  6.9 CyUartSetHwFlowControl  ---------------------------------------------
-- |Enables hardware flow control.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` on error if handle is invalid in case of Linux/Mac.
-- * `ErrorInvalidParameter` on error if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` on error if request was timed out.
-- * `ErrorRequestFailed` on error if request was failed by device or if device type is not UART.
--
-- See Also
--
-- * `getHwFlowControl`
--
-- From: 6.9 CyUartSetHwFlowControl
{#fun unsafe CyUartSetHwFlowControl as setHwFlowControl
  { id `Handle' -- ^ Valid device handle
  , `FlowControl' -- ^ CY_FLOW_CONTROL_MODES mode: Flow control mode
  }
  -> `ReturnStatus' toReturnStatus
#}

--------------------------------------------  6.10 CyUartGetHwFlowControl  ---------------------------------------------
-- |Retrieve the current hardware flow control status.
--
-- Returns `FlowControl`, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device or when device type is not UART.
--
-- See Also
--
-- * `setHwFlowControl`
--
-- From: 6.10 CyUartGetHwFlowControl
getHwFlowControl :: Handle -- ^ Valid device handle
                 -> IO (Either ReturnStatus FlowControl) -- ^ Failed `ReturnStatus` or `FlowControl`
getHwFlowControl h = toEither <$> _cyUartGetHwFlowControl h

{#fun unsafe CyUartGetHwFlowControl as _cyUartGetHwFlowControl
  { id `Handle' -- ^ Valid device handle
  , allocaFlowControl- `FlowControl' peekFlowControl* -- ^ mode: Flow control mode
  }
  -> `ReturnStatus' toReturnStatus
#}

peekFlowControl :: Ptr CInt -> IO FlowControl
peekFlowControl p = (toEnum . fromEnum) <$> peek p

allocaFlowControl :: (Ptr CInt -> IO a) -> IO a
allocaFlowControl f = alloca $ \(p :: Ptr ()) -> f (castPtr p)

------------------------------------------------  6.11 CyUartSetBreak  -------------------------------------------------
-- |Set break timeout value.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device or when device type is not UART.
--
-- See Also
--
-- * `setHwFlowControl`
--
-- From: 6.11 CyUartSetBreak
{#fun unsafe CyUartSetBreak as setBreak
  { id `Handle'      -- ^ Valid device handle
  , `Word16' -- ^ timeout: Break timeout value in milliseconds
  }
  -> `ReturnStatus' toReturnStatus
#}

