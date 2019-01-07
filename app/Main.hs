{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad(forM_)
import           Control.Concurrent (threadDelay)
import           Control.Error.Safe
import           Control.Error.Util
import           Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import           System.Cypress.USBSerial
import qualified System.Cypress.Safe.USBSerial as Safe
import           System.Cypress.Safe.USBSerial (USB, runUSB, try, throwErrorOther)
import qualified System.Cypress.Safe.USBSerial.UART as UART
import qualified System.Cypress.Safe.USBSerial.I2C as I2C
import           System.Exit (exitSuccess, exitFailure)

run :: IO (Either ReturnStatus ())
run = withLibraryInit $ do
    numDevs <- tryRight =<< liftIO getListOfDevices
    liftIO $ putStrLn $ show numDevs ++ " devices"
    forM_ [(0::DeviceID)..(numDevs-1)] $ \i -> do
        liftIO $ putStrLn $ "---- " ++ show i ++ " ----"
        dev <- tryRight =<< liftIO (getDeviceInfo i)
        liftIO $ print dev
    return ()

main :: IO ()
main = do
    putStrLn versionString
    status' <- run
    case status' of
        Left status -> libraryExit >> error ("FAIL" ++ show status)
        Right _ -> return ()
    {-
    putStrLn ""
    status <- run2
    putStrLn ""
    x <- run3
    liftIO $ print x
    --putStrLn "\nrun4"
    --infos <- run4
    --print infos
    -}
    putStrLn "\nrun6"
    result <- run6
    print result
    return ()

printError :: ReturnStatus -> USB ()
printError status = liftIO $ putStrLn $ show status ++ ": " ++ errorString status

run2 :: IO (Either ReturnStatus ())
run2 = runUSB $ do
    numDevs <- Safe.getListOfDevices
    liftIO $ putStrLn $ show numDevs ++ " devices"
    forM_ [0..(numDevs-1)] $ \i -> do
            liftIO $ putStrLn $ "---- " ++ show i ++ " ----"
            dev <- try $ Safe.getDeviceInfo i
            liftIO $ print dev
    `catchError` printError

run3 :: IO (Either ReturnStatus [DeviceInfo])
run3 = runUSB $ do
    numDevs <- Safe.getListOfDevices
    forM [0..(numDevs-1)] $ \i -> try $ Safe.getDeviceInfo i

run4 :: IO (Either ReturnStatus [(DeviceID, DeviceInfo)])
run4 = runUSB $ try $ Safe.findDevicesWithVidPid 0x04b4 0x0002

run5 :: IO (Either ReturnStatus (LibraryVersion, FirmwareVersion, Signature, UART.Config))
run5 = runUSB $ do
    infos <- try $ Safe.findDeviceWithVidPidTypeClass 0x04b4 0x0004 DeviceType'UART DeviceClass'Vendor
    let (devID, ifaceID) = head infos
    liftIO $ print (devID, ifaceID)
    try $ Safe.withOpen devID ifaceID $ do
        libVer <- try Safe.getLibraryVersion
        fwVer <- try Safe.getFirmwareVersion
        sig <- try Safe.getSignature
        cfg <- try UART.getConfig
        forM_ [1..] $ \i->do
            out <- try $ UART.write (BS.pack $ replicate 10000 'a') 1000
            liftIO $ do
                print out
                threadDelay 1000000   -- 50ms
        --try $ Safe.setGpioValue 0 0
        return (libVer, fwVer, sig, cfg)

run6 :: IO (Either ReturnStatus (LibraryVersion, FirmwareVersion, Signature, I2C.Config))
run6 = runUSB $ do
    infos <- try $ Safe.findDeviceWithVidPidTypeClass 0x04b4 0x0004 DeviceType'I2C DeviceClass'Vendor
    let (devID, ifaceID) = head infos
    liftIO $ print (devID, ifaceID)
    try $ Safe.withOpen devID ifaceID $ do
        libVer <- try Safe.getLibraryVersion
        fwVer <- try Safe.getFirmwareVersion
        sig <- try Safe.getSignature
        cfg <- try I2C.getConfig
        return (libVer, fwVer, sig, cfg)