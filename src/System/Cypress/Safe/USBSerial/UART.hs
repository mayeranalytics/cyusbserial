{-|
Module      : System.Cypress.Safe.USBSerial.UART
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

This is the `safe` version of `System.Cypress.USBSerial.UART.UART`.

See section 6 of the Cypress USB Serial Library API Guide, Rev 1.0.
-}

module System.Cypress.Safe.USBSerial.UART (
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
import           Control.Monad.Reader (lift, ask, liftIO)
import           Control.Error.Safe (tryRight)
import           Control.Error.Util hiding (tryIO)
import           System.Cypress.USBSerial.UART hiding (setConfig, getConfig, read, write, setDtr, clearDtr,
                        setRts, clearRts, setHwFlowControl, getHwFlowControl, setBreak)
import qualified System.Cypress.USBSerial.UART as Cy
import           System.Cypress.Safe.USBSerial
import           System.Cypress.Safe.USBSerial.Internal.Utils
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import           Data.Word
    
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
setConfig :: Config  -- ^ UART configuration
          -> USBWithHandle (Either ReturnStatus ())
setConfig c = do
    h <- lift ask
    throwUnlessSuccess $ liftIO $ Cy.setConfig h c

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
getConfig :: USBWithHandle (Either ReturnStatus Config)  -- ^ Failed `ReturnStatus` or `Config`
getConfig = do
    h <- lift ask
    liftIO $ Cy.getConfig h

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
read :: Word32      -- ^ read length
     -> Word32      -- ^ API timeout value
     -> USBWithHandle (Either ReturnStatus ByteString) -- ^ Failed `ReturnStatus` or `ByteString` with read data
read l t = do
    h <- lift ask
    liftIO $ Cy.read h l t

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
write :: ByteString  -- ^ data to write
      -> Word32      -- ^ API timeout value
      -> USBWithHandle (Either ReturnStatus Word32) -- ^ Failed `ReturnStatus` or number of bytes written
write bs t = do
    h <- lift ask
    liftIO $ Cy.write h bs t

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
setDtr :: USBWithHandle ReturnStatus
setDtr = do
    h <- lift ask
    liftIO $ Cy.setDtr h

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
clearDtr :: USBWithHandle ReturnStatus
clearDtr = do
    h <- lift ask
    liftIO $ Cy.clearDtr h

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
setRts :: USBWithHandle ReturnStatus
setRts = do
    h <- lift ask
    liftIO $ Cy.setRts h

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
clearRts :: USBWithHandle ReturnStatus
clearRts = do
    h <- lift ask
    liftIO $ Cy.clearRts h

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
setHwFlowControl :: FlowControl -- ^ CY_FLOW_CONTROL_MODES mode: Flow control mode
                 -> USBWithHandle ReturnStatus
setHwFlowControl c = do
    h <- lift ask
    liftIO $ Cy.setHwFlowControl h c
                 
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
getHwFlowControl :: USBWithHandle (Either ReturnStatus FlowControl) -- ^ Failed `ReturnStatus` or `FlowControl`
getHwFlowControl = do
    h <- lift ask
    liftIO $ Cy.getHwFlowControl h

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
setBreak :: Word16 -- ^ timeout: Break timeout value in milliseconds
         -> USBWithHandle (Either ReturnStatus ())
setBreak t = do
    h <- lift ask
    throwUnlessSuccess $ liftIO $ Cy.setBreak h t

