{-|
Module      : System.Cypress.Safe.USBSerial
Description : Constants, data types, USB Initialization and Common API of the Cypress USBSeral library
Copyright   : (c) Markus Mayer
License     : LGPL-2.1
Maintainer  : mmayer@mayeranalytics.com
Stability   : experimental
Portability : non-portable (needs dll/so/dylib)

Constants, data types, USB Initialization and Common API of the Cypress USBSeral library.

* The two monads

    * `USB` in which all USB actions run
    * `USBWithHandle` in which all USB actions run that require a `Handle`

* Device enumeration
* Opening and closing
* Version information and signature
* Events
* GPIO getting and setting

        * GPIO pins (CY'11: 10x, CY'13: 8x, CY'15: 17x)
        * See `getGpioValue`, `setGpioValue`

* Recycling and resetting
* User flash reading and writing

    * 512-byte flash for storing user parameters
    * See `readUserFlash`, `progUserFlash`

This is the `safe` version of `System.Cypress.USBSerial.USBSerial`.

-}

{-# LANGUAGE CPP                      #-}

module System.Cypress.Safe.USBSerial (
    -- * The USB monad
    USB(..),
    runUSB,
    -- * Error handling
    try,
    throwUnlessSuccess,
    handleError,
    throwErrorOther,
    -- * Version information
    version,
    versionString,
    versionMajor,
    versionMinor,
    versionPatch,
    versionBuild,
    -- * Types
    Handle,
    ReturnStatus(..),
    errorString,
    DeviceClass(..),
    DeviceInfo(..),
    DeviceID, InterfaceID,
    ManufacturerName,
    ProductName,
    SerialNum,
    DeviceFriendlyName,
    DeviceSerialBlock(..),
    DeviceType(..),
    VidPid(..),
    LibraryVersion(..),
    FirmwareVersion(..),
    CallbackEvents(..),
    EventNotification,
    -- * Device enumeration
    getListOfDevices,
    getDeviceInfo,
    getDeviceInfoVidPid,
    findDevicesWithVidPid,
    findDeviceWithVidPidTypeClass,
    -- * Opening and closing
    USBWithHandle(..),
    withOpen,
    -- * Version information and signature
    getLibraryVersion,
    getFirmwareVersion,
    getSignature,
    Signature(..),
    -- * Events
    withEventNotification,
    abortEventNotification,
    -- * GPIO getting and setting
    getGpioValue,
    setGpioValue,
    -- * Recycling and resetting
#ifndef linux_HOST_OS
    cyclePort,
#endif
    resetDevice,
    -- * Flash reading and writing
    readUserFlash,
    progUserFlash,
) where

import           Control.Monad (liftM, when)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Error.Safe (tryRight)
import           Control.Error.Util hiding (tryIO)
import           Data.Word
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified System.Cypress.USBSerial as Cy
import           System.Cypress.USBSerial hiding (getListOfDevices, getDeviceInfo, getDeviceInfoVidPid,
                    getLibraryVersion, getFirmwareVersion, getSignature, withEventNotification,
                    abortEventNotification, getGpioValue, setGpioValue, resetDevice, readUserFlash, progUserFlash
                    )
import           System.Cypress.Safe.USBSerial.Internal.Utils

-----------------------------------------------------  USB monad  --------------------------------------------------
-- |The USB Monad transformer
type USB a = ExceptT ReturnStatus IO a

-- |Run a `USB` action. On failure the action is aborted and the failed `ReturnStatus` is returned. On success the value @a@ is returned.
runUSB :: USB a -> IO (Either ReturnStatus a)
runUSB act = runExceptT $ do
    status <- liftIO libraryInit
    if status == Success then do
        a <- act
        exitStatus <- liftIO libraryExit
        if exitStatus == Success
        then return a
        else throwError exitStatus
    else throwError status

-- |Performs an action that fails if `ReturnStatus` if not `Success`.
--
-- There are two types of return values in this API, `USB` `ReturnStatus` and `USB` (`Either` `ReturnStatus` @a@).
-- `try` applies to the latter. Use `throwUnlessSuccess` if you have the former.
--
-- The same applies to `USBWithHandle`.
try :: Monad m => ExceptT ReturnStatus m (Either ReturnStatus a) -> ExceptT ReturnStatus m a
try act = tryRight =<< act

-- |Handle error that may occur in an @action@ using a @handler@. This is `catchError` with flipped arguments.
handleError :: MonadError e m => (e -> m a) -> m a -> m a
handleError = flip catchError

-- |Same as `try`, just for `IO` instead of `USB`
tryIO ::  MonadIO m => IO (Either ReturnStatus a) -> ExceptT ReturnStatus m a
tryIO act = tryRight =<< liftIO act

-- |Throw an `ErrorOther`.
throwErrorOther :: Monad m => String -> ExceptT ReturnStatus m a
throwErrorOther s = throwError (ErrorOther s)

-----------------------------------------------  4.3 CyGetListofDevices  -----------------------------------------------
-- |Retrieve the number of USB devices connected to the host.
--
-- In Windows family of operating systems the API retrieves only the number of devices that are attached to 
-- CyUSB3.SYS driver. For other operating systems, it retrieves the total number of USB devices present on the 
-- bus. It includes both USB Serial device as well as other devices.
--
-- Returns number of devices connecte, or on failure one of:
--
-- * `ErrorRequestFailed` on failure
-- * `ErrorDeviceNotFound` if there are no devices attached.
-- * `ErrorRequestFailed` if library was not initialized.
--
-- See Also
--
-- * `getDeviceInfo`
-- * `getDeviceInfoVidPid`
-- * `open`
-- * `close`
--
-- From: 4.3 CyGetListofDevices
getListOfDevices ::  USB Word8
getListOfDevices = tryIO Cy.getListOfDevices

------------------------------------------------  4.4 CyGetDeviceInfo  -------------------------------------------------
-- |Retrieve the device information of a USB device.
--
-- In order to get the device information on
-- particular device user needs to provide the device number. To identify the device of interest, the application
-- needs to loop through all devices connected and obtain the information.
--
-- Returns `DeviceInfo`, or on failure one of:
--
-- * `ErrorRequestFailed` if library is not initialized (Only for Linux and Mac).
-- * `ErrorInvalidParameter` if the input parameters are invalid.
-- * `ErrorDeviceInfoFetchFailed` if failed to fetch device information.
-- * `ErrorAccessDenied` if access is denied by operating system.
-- * `ErrorDeviceNotFound` if specified device number is invalid.
--
-- See Also
--
-- * `DeviceInfo`
-- * `DeviceType`
-- * `DeviceClass`
-- * `getListOfDevices`
-- * `getDeviceInfoVidPid`
-- * `open`
-- * `close`
--
-- From: 4.4 CyGetDeviceInfo
getDeviceInfo :: Word8  -- ^ Device number
              -> USB (Either ReturnStatus DeviceInfo)
getDeviceInfo = liftIO . Cy.getDeviceInfo

---------------------------------------------  4.5 CyGetDeviceInfoVidPid  ----------------------------------------------
-- |Retrieve the information of all devices with specified Vendor ID and Product ID.
--
-- The deviceIdList contains the device numbers of all the devices with specified VID and PID. 
-- The device of interest can be identified from the information in `DeviceInfo`, which includes interface number,
-- string descriptor, deviceType and deviceClass.
--
-- Returns a list of `DeviceID`, `DeviceInfo` tuples, or on failure one of:
--
-- * `ErrorRequestFailed` on if library is not initialized (Only for Linux and Mac)
-- * `ErrorInvalidParameter` if the input parameters are invalid.
-- * `ErrorDeviceInfoFetchFailed` if failed to fetch device information.
-- * `ErrorAccessDenied` if access is denied by operating system.
-- * `ErrorDeviceNotFound` if specified device number is invalid.
--
-- See Also
--
-- * `DeviceInfo`
-- * `DeviceClass`
-- * `DeviceType`
-- * `getListOfDevices`
-- * `getDeviceInfo`
-- * `open`
-- * `close`
--
-- From: 4.5 CyGetDeviceInfoVidPid
getDeviceInfoVidPid :: VidPid   -- ^ vidPid: VID and PID of device of interest
                    -> Word8    -- ^ Total length of the deviceInfoList allocated (Size of deviceInfoList array)
                    -> USB (Either ReturnStatus [(DeviceID, DeviceInfo)])
getDeviceInfoVidPid a b = liftIO $ Cy.getDeviceInfoVidPid a b

-----------------------------------------------------  USBWithHandle monad ---------------------------------------------
-- |`USBWithHandle` is the monad in which all functions that need a `Handle` must run. 
-- The `USBWithHandle` context is created only by `withOpen`.
--
-- It is essentially an `ExceptT` monad transformer with a `ReaderT` monad inside. It can be used 
-- similarly to `USB`. It is always assured that handles are closed.
type USBWithHandle a = ExceptT ReturnStatus (ReaderT Handle IO) a

-----------------------------------------------------  withOpen  -------------------------------------------------------
-- |Open the USB Serial device and run an action in the `USBWithHandle` monad.
--
-- Use device number and interface number to identify the device.
--
-- Onn failure it returns one of:
--
-- * `ErrorRequestFailed` on if library is not initialized (Only for Linux and Mac)
-- * `ErrorInvalidParameter` if the input parameters are invalid.
-- * `ErrorDriverOpenFailed` if open was unsuccessful.
-- * `ErrorAccessDenied` if access is denied by operating system.
-- * `ErrorAllocationFailed` if memory allocation was failed.
-- * `ErrorDeviceNotFound` if specified device number is invalid.
--
-- See Also
--
-- * `USBWithOpen`
-- * `getListOfDevices`
-- * `getDeviceInfoVidPid`
-- * `getDeviceInfo`
--
-- From: 4.6 CyOpen, 4.7 CyClose
withOpen :: DeviceID                    -- ^ Device ID (number)
         -> InterfaceID                 -- ^ Interface ID (Number)
         -> USBWithHandle a             -- ^ action to perform with handle
         -> USB (Either ReturnStatus a) -- ^ Failed `ReturnStatus` or `Handle`
withOpen d i act = do
    h <- tryIO $ open d i
    result <- liftIO $ runReaderT (runExceptT act) h
    statusClose <- liftIO $ close h
    case result of 
        Left err -> if statusClose == Success 
                    then throwError err             -- bad result but handle successfully closed
                    else throwError (ErrorOther s)  -- bad result and handle not successfully closed
                        where s = "Action failed with " ++ show err ++ ", close failed with " ++ show statusClose
        Right result -> 
            if statusClose == Success
            then return $ Right result   -- the result is good and the handle was closed successfully
            else throwError statusClose  -- the result is good but the handle couldn't be closed

----------------------------------------------  3.12 CY_LIBRARY_VERSION  -----------------------------------------------
-- |Retrieve the version of USB Serial library.
--
-- Note that this function will never return a failure `ReturnStatus`.
--
-- See Also
--
-- * `System.Cypress.Safe.USBSerial.getFirmwareVersion`
--
-- From: 3.12 CY_LIBRARY_VERSION
getLibraryVersion :: USBWithHandle (Either ReturnStatus LibraryVersion)
getLibraryVersion = do
    h <- lift ask
    ver <- liftIO $ Cy.getLibraryVersion h
    return $ Right ver

----------------------------------------------  3.14 CY_FIRMWARE_VERSION  ----------------------------------------------
-- |Retrieve the firmware version of the USB Serial device.
--
-- See Also
--
-- * `System.Cypress.Safe.USBSerial.getLibraryVersion`
--
-- From: 3.14 CY_FIRMWARE_VERSION
getFirmwareVersion :: USBWithHandle (Either ReturnStatus FirmwareVersion)
getFirmwareVersion = do
    h <- lift ask
    liftIO $ Cy.getFirmwareVersion h

-------------------------------------------------  5.3 CyGetSignature  -------------------------------------------------
-- |Retrieve the `Signature` of the device firmware. It is a `String` like 'CYUS'.
--
-- Returns `Signature` (a `String`), or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device.
--
-- 5.3 CyGetSignature
getSignature :: USBWithHandle(Either ReturnStatus Signature) -- ^ Failed `ReturnStatus` or `Signature`
getSignature = do
    h <- lift ask
    liftIO $ Cy.getSignature h

---------------------------------------------  5.4 CySetEventNotification  ---------------------------------------------
-- |Register a callback for error/event notifications during UART/SPI data transfers.
--
-- A callback will be issued based on the error/events sent by the device.
--
-- Returns good value, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorStatusMonitorExist` if notification callback is already registered.
--
-- See Also
-- 
-- * `CallbackEvents`
-- * `EventNotificationCallback`
-- * `abortEventNotification`
--
-- From: 5.4 CySetEventNotification
withEventNotification :: EventNotification
                      -> (Handle -> IO a) -- ^ action to execute
                      -> USBWithHandle(Either ReturnStatus a) -- ^ Failed `ReturnStatus` or good value
withEventNotification e f = do
    h <- lift ask
    liftIO $ Cy.withEventNotification e h f
                                                 
--------------------------------------------  5.5 CyAbortEventNotification  --------------------------------------------
-- |Unregister the event callback.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorRequestFailed` if API is called before registering callback.
--
-- See Also
--
-- * `setEventNotification`
--
-- From: 5.5 CyAbortEventNotification
abortEventNotification :: USBWithHandle ReturnStatus
abortEventNotification = do
    h <- lift ask
    liftIO $ Cy.abortEventNotification h

-------------------------------------------------  5.6 CyGetGpioValue  -------------------------------------------------
-- |Retrieve the value of a GPIO.
--
-- Returns current state of the GPIO, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device.
--
-- See Also
--
-- * `setGpioValue`
--
-- From: 5.6 CyGetGpioValue
getGpioValue :: Word8  -- ^ GPIO number
             -> USBWithHandle(Either ReturnStatus Word8) -- ^ Failed `ReturnStatus` or current state of the GPIO
getGpioValue n = do
    h <- lift ask
    liftIO $ Cy.getGpioValue h n

-------------------------------------------------  5.7 CySetGpioValue  -------------------------------------------------
-- |Set the value of a GPIO.
--
-- The GPIO must be configured as an output.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` on error when request is failed by USB Serial device.
--
-- See Also
--
-- * `getGpioValue`
--
-- From: 5.7 CySetGpioValue
setGpioValue :: Word8 -- ^ GPIO number
             -> Word8 -- ^ Value that needs to be set
             -> USBWithHandle (Either ReturnStatus ())
setGpioValue n v = do
    h <- lift ask
    throwUnlessSuccess $ liftIO $ Cy.setGpioValue h n v

-------------------------------------------------  5.8 CyResetDevice  --------------------------------------------------
-- |Reset the device by sending a vendor request.
--
-- The device will be re-enumerated.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device.
--
-- See Also
--
-- * `cyclePort`
--
-- From: 5.8 CyResetDevice
resetDevice :: USBWithHandle ReturnStatus
resetDevice = do
    h <- lift ask
    liftIO $ Cy.resetDevice h

------------------------------------------------  5.9 CyReadUserFlash  -------------------------------------------------
-- |Read from the flash address specified.
--
-- The total space available is 512 bytes. The flash area address offset is from 0x0000 to 0x00200
-- and should be read page wise (page size is 128 bytes).
--
-- Returns data buffer containing data or 
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device.
--
-- See Also
--
-- * `progUserFlash`
--
-- From: 5.9 CyReadUserFlash
readUserFlash :: Word32 -- ^ flashAddress: Address from which the data is read
              -> Word32 -- ^ length to read
              -> Word32 -- ^ Timeout value of the API
              -> USBWithHandle(Either ReturnStatus ByteString)  -- ^ Failed `ReturnStatus` or `DataBuffer`
readUserFlash a b c = do
    h <- lift ask
    liftIO $ Cy.readUserFlash h a b c

------------------------------------------------  5.10 CyProgUserFlash  ------------------------------------------------
-- |Write to the user flash area on the USB Serial device.
--
-- The total space available is 512 bytes. The flash area address offset is from 0x0000
-- to 0x00200 and should be written page wise (page size is 128 bytes).
--
-- Returns number of bytes written or
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device.
--
-- See Also
--
-- * `readUserFlash`
--
-- From: 5.10 CyProgUserFlash
progUserFlash :: Word32     -- ^ Address to which the data is written
              -> ByteString -- ^ Data to write
              -> Word32     -- ^ Timeout value of the API
              -> USBWithHandle (Either ReturnStatus Word32) -- ^ Failed `ReturnStatus` or number of bytes written
progUserFlash a b c = do
    h <- lift ask
    liftIO $ Cy.progUserFlash h a b c

------------------------------------------------  findDeviceWithVidPid  ---------------------------------------------
-- |Find all devices with a given `Vid` and `Pid`.
--
-- Returns non-empty list of tuples of `DeviceID` and `DeviceInfo`, or on failure one of
--
-- * `ErrorRequestFailed` on failure
-- * `ErrorDeviceNotFound` if there are no devices attached.
-- * `ErrorRequestFailed` if library was not initialized.
-- * `ErrorInvalidParameter` if the input parameters are invalid.
-- * `ErrorDeviceInfoFetchFailed` if failed to fetch device information.
-- * `ErrorAccessDenied` if access is denied by operating system.
-- * `ErrorDeviceNotFound` if specified device number is invalid.
-- * `ErrorOther` if all else is good but no matching device was found.
--
-- See Also
--
-- * `findDeviceWithVidPidTypeClass`
findDevicesWithVidPid :: Vid                -- ^ Vendor ID
                      -> Pid                -- ^ Product ID
                      -> USB (Either ReturnStatus [(DeviceID, DeviceInfo)])
findDevicesWithVidPid vid pid = do
    numDevs <- getListOfDevices
    devInfos <- try $ getDeviceInfoVidPid (VidPid vid pid) numDevs
    if null devInfos
        then throwErrorOther ("findDevicesWithVidPid: No devices found with vid="++show vid++", pid="++show pid)
        else return (Right devInfos)

------------------------------------------------  findDeviceWithVidPidTypeClass  ---------------------------------------
-- |Find all devices with a given `Vid`, `Pid`, `DeviceType` and `DeviceClass`
--
-- Returns non-empty list of tuples of `DeviceID` and `InterfaceID`, or on failure one of
--
-- * `ErrorRequestFailed` on failure
-- * `ErrorDeviceNotFound` if there are no devices attached.
-- * `ErrorRequestFailed` if library was not initialized.
-- * `ErrorInvalidParameter` if the input parameters are invalid.
-- * `ErrorDeviceInfoFetchFailed` if failed to fetch device information.
-- * `ErrorAccessDenied` if access is denied by operating system.
-- * `ErrorDeviceNotFound` if specified device number is invalid.
-- * `ErrorOther` if all else is good but no matching device was found.
--
-- See Also
--
-- * `findDevicesWithVidPid`
findDeviceWithVidPidTypeClass :: Vid          -- ^ Vendor ID
                              -> Pid          -- ^ Product ID
                              -> DeviceType   -- ^ Filter by this `DeviceType`
                              -> DeviceClass  -- ^ Filter by this `DeviceClass`
                              -> USB (Either ReturnStatus [(DeviceID, InterfaceID)])
findDeviceWithVidPidTypeClass vid pid typ cls = do
    numDevs <- getListOfDevices
    xs <- try $ getDeviceInfoVidPid (VidPid vid pid) numDevs
    let devIDs = fst <$> xs :: [DeviceID]
        devTypesClasses' = (\x->zip3 [0..] (deviceInfo'deviceType $ snd x) (deviceInfo'deviceClass $ snd x)) <$> xs 
            :: [[(InterfaceID, DeviceType, DeviceClass)]]
        devTypesClasses = filter (\(_,t,c)->t==typ && c==cls) <$> devTypesClasses' :: [[(InterfaceID, DeviceType, DeviceClass)]]
        fst3 (i,_,_) = i
        ifaceIDs = map (map fst3) devTypesClasses
        infos = concat $ (\(d,is)->(,) <$> [d] <*> is) <$> zip devIDs ifaceIDs
    if null infos
        then throwErrorOther ("findDeviceWithVidPidTypeClass: No devices found with vid="++show vid++", pid="++show pid++", type="++show typ++", class="++show cls)
        else return (Right infos)
