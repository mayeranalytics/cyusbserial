import           Data.Maybe
import           Distribution.PackageDescription    hiding (Flag)
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           System.Directory
import           System.Info(os, arch)
import           System.FilePath(joinPath)
import           Data.Char (toLower)

-- https://github.com/snakamura/volare/blob/master/Setup.hs

-- main = defaultMain

main = defaultMainWithHooks simpleUserHooks
    { confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
    , postCopy = copyExtLib
    }

sysInfoString :: String
sysInfoString = map toLower os

libExtension :: String
libExtension | sysInfoString == "darwin" = ".dylib"
             | sysInfoString == "linux" = ".so"
             | sysInfoString == "windows" || os == "mingw32" = ".dll"
             | otherwise = error ("Unknown os: " ++ os)

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
    let packageDescription = localPkgDescr localBuildInfo
        lib = fromJust $ library packageDescription
        libBuild = libBuildInfo lib 
    dir <- getCurrentDirectory
    let extraLibDir | os == "mingw32" = joinPath [dir, "cypress", sysInfoString, arch]
                    | otherwise       = joinPath [dir, "cypress", sysInfoString]
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
        path = joinPath ["cypress", sysInfoString, "libcyusbserial"++libExtension]
    putStrLn $ "copy " ++ path ++ " -> " ++ libPref
    rawSystemExit verbosity "cp" [path, libPref]
