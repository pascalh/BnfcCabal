{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where
import Prelude hiding (mod)
import Data.Char (toUpper)
import Data.Maybe (fromMaybe,catMaybes)
import Control.Monad(liftM,liftM2)
import Control.Exception(Exception,throw)
import Data.Data(Typeable)

import Parser (parseIntList)

import Distribution.Simple hiding (Language)
import Distribution.PackageDescription.Parse (writePackageDescription) 
import Distribution.ModuleName(ModuleName) 
import Distribution.PackageDescription
import Distribution.Text (simpleParse)

import System.Process(rawSystem)
import System.FilePath(dropExtension)
import System.Directory (findExecutable)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))

import Text.ParserCombinators.Parsec (ParseError)

-- |a datatype for possibles runtime error 
data BnfcCabalErr 
  = BnfcExeNotFoundOnPath       -- ^ Bnfc executable can not be found
  | BnfcError                   -- ^ Bnfc failed to run
  | VersionParseFail ParseError -- ^ Fails to parse version from console
  | NumberOfArgs                -- ^ Incorrect number of CLI-arguments
  deriving (Typeable)

instance Show BnfcCabalErr where
  show BnfcExeNotFoundOnPath = "Bnfc executable cannot be found on path."
  show (VersionParseFail p) = "Couldn't parse version number: "++show p
  show NumberOfArgs  = 
    "exactly one command line argument expected (labelled grammar file)."
  show BnfcError = "A bnfc error occured."

instance Exception BnfcCabalErr 

-- |the name of a language is a string
type Language = String

-- |storage for configuration data
data Config = Config
  { cVersion :: [Int]    -- ^ version of package to be installed
  , cBnfc    :: FilePath -- ^ path to bnfc executable
  }

-- * Main functions

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file]-> readConfig >>= writePackageDesc file 
    _     -> throw NumberOfArgs 

-- |runs bnfc on cf file and writes package description to current directory
writePackageDesc :: FilePath -> Config -> IO ()
writePackageDesc file config =
  let lang = dropExtension file in do
    runBnfc (cBnfc config) lang
    writePackageDescription 
      ("language-"++lang++".cabal")   
      (updatePd config lang emptyPackageDescription) 

-- |runs bnfc on file @lang@.cf 
runBnfc :: FilePath -> Language -> IO ()
runBnfc bnfc lang = do
  run <- rawSystem bnfc
              [ "-p" , "Language."++toUpperFirst lang
              , "-haskell" , lang++".cf"
              ]
  case run of 
    ExitSuccess   -> return ()
    ExitFailure _ -> throw BnfcError

-- * Input functions

-- |use 'readVersion' and 'findBnfc' to build a config value
readConfig :: IO Config
readConfig = liftM2 Config readVersion findBnfc 

-- |asks for a version number and returns it (throws an error if parse fails)
readVersion :: IO [Int]
readVersion = do
  putStrLn "Type a version number, e.g. [0,1] for version 0.1" 
  either (throw . VersionParseFail) return . parseIntList =<< getLine

-- |returns bnfc executable or throws an error if bnfc binary can not be found
findBnfc :: IO FilePath
findBnfc = 
  liftM (fromMaybe (throw BnfcExeNotFoundOnPath)) (findExecutable "bnfc")

-- * building the package description

-- |adds modules to skeleton package description 
updatePd :: Config -> Language -> PackageDescription -> PackageDescription
updatePd config lang p = p
  { package = 
    PackageIdentifier 
      { pkgName = PackageName ("language-"++lang)
      , pkgVersion = Version (cVersion config) []}
  , library = 
    Just Library 
      { exposedModules = exposedMods lang
      , reexportedModules = []
      , requiredSignatures = []
      , exposedSignatures = []
      , libExposed = True
      , libBuildInfo = emptyBuildInfo 
          { buildable    = True
          , hsSourceDirs = ["."]
          , targetBuildDepends = dependencies
          }
      }
  , extraSrcFiles = 
    [ "Language/"++toUpperFirst lang++"/Par"++lang++".y"
    , "Language/"++toUpperFirst lang++"/Lex"++lang++".x"
    ]
  , buildType = Just Simple
   }

dependencies :: [Dependency]
dependencies = 
    [ Dependency (PackageName "base") (laterVersion $ Version [4] []) 
    , Dependency (PackageName "containers") anyVersion
    , Dependency (PackageName "array") anyVersion 
    ]

-- |returns a list of all exposed modules for a given language
exposedMods :: Language -> [ModuleName]
exposedMods lang = catMaybes $ 
  (simpleParse $ "Language."++toUpperFirst lang++".ErrM")
  : map 
      (simpleParse . makemod lang) 
      ["Abs"
      ,"Par"
      ,"Print"
      ,"Lex"
      ] 

-- |given a language and a modulename 'makemod' builds a
-- new module name based on the language
-- > makemod "C" "Abs" ==> "Language.C.Absc"
makemod :: Language -> String -> String
makemod lang mod = "Language." ++ toUpperFirst lang ++ "." ++ mod ++ lang

-- |capitalizes the first character of a string 
toUpperFirst :: String -> String
toUpperFirst [] = []
toUpperFirst (x:xs) = toUpper x : xs

