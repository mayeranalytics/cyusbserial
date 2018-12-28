import           Data.Maybe
import           Distribution.PackageDescription    hiding (Flag)
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           System.Directory
import qualified System.Info as Info (os, arch)
import           System.FilePath(joinPath)
import           Data.Char (toLower)

-- https://github.com/snakamura/volare/blob/master/Setup.hs

-- main = defaultMain

main = defaultMainWithHooks simpleUserHooks
    { confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
    , postCopy = copyExtLib
    }

os :: String
os = map toLower Info.os

arch :: String
arch = map toLower Info.arch

libPath :: String
libPath | os == "darwin" = joinPath ["cypress", os, "libcyusbserial.dylib"]
        | os == "linux" = joinPath ["cypress", os, "libcyusbserial.so"]
        | os == "windows" || os == "mingw32" = joinPath ["cypress", os, arch, "cyusbserial.dll"]
        | otherwise = error ("Unknown os: " ++ os)

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
    let packageDescription = localPkgDescr localBuildInfo
        lib = fromJust $ library packageDescription
        libBuild = libBuildInfo lib 
    dir <- getCurrentDirectory
    let extraLibDir | os == "mingw32" = joinPath [dir, "cypress", os, arch]
                    | otherwise       = joinPath [dir, "cypress", os]
    putStrLn $ "extraLibDir=" ++ extraLibDir
    return localBuildInfo {
        localPkgDescr = packageDescription {
            library = Just $ lib {
                libBuildInfo = libBuild {
                    extraLibDirs = extraLibDir :
                        extraLibDirs libBuild
                }
            }
        }
    }

copyExtLib :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyExtLib _ flags pkg_descr lbi = do
    let libPref = libdir . absoluteInstallDirs pkg_descr lbi
                . fromFlag . copyDest
                $ flags
    let verbosity = fromFlag $ copyVerbosity flags
    putStrLn $ "copyExtLib: " ++ libPath ++ " -> " ++ libPref
    rawSystemExit verbosity "cp" [libPath, libPref]
