{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import           System.Cypress.USBSerial as Cy
import           Control.Monad.IO.Class (liftIO)
import           Data.Either (isRight)
import           Control.Error.Safe (tryRight)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic (assert, monadicIO, run)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import           Foreign.C.Types

C.context (C.baseCtx)
C.include "CyUSBSerial.h"
C.include "extras.h"

propEnumSize :: Property
propEnumSize = monadicIO $ do
    b <- run $ (==1) <$> [C.exp| int {sizeof(CY_DEVICE_TYPE) == sizeof(int)} |]
    assert b

propBoolSize :: Property
propBoolSize = monadicIO $ do
    b <- run $ (==1) <$> [C.exp| int {sizeof(BOOL) == sizeof(char)} |]
    assert b

propConfigSize :: Property
propConfigSize = monadicIO $ do
    b <- run $ (==1) <$> [C.exp| int {sizeof(CONFIG_FLASH) == CONFIG_FLASH_SIZE} |]
    assert b

propInit :: Property
propInit = monadicIO $ do
    success <- liftIO libraryInit
    assert (success == Cy.Success)
    success <- liftIO libraryExit
    assert (success == Cy.Success)

propNumDevs :: Property
propNumDevs = monadicIO $ do
    n <- liftIO $ withLibraryInit $ tryRight =<< liftIO getListOfDevices
    assert $ isRight n
   

main :: IO ()
main = do
    quickCheck $ withMaxSuccess 1 propEnumSize
    quickCheck $ withMaxSuccess 1 propBoolSize
    quickCheck $ withMaxSuccess 1 propConfigSize
    quickCheck propInit
    quickCheck propNumDevs
