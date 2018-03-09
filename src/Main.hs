{-# LANGUAGE LambdaCase,TupleSections #-}

module Main (main) where

import Control.Exception
import Data.List
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import qualified System.Info as SysInfo
import System.IO
import System.Process

import BFMonad
import Compiler


main :: IO ()
main = getOpt Permute options <$> getArgs >>= \case
  (args,f_,[]) -> evalOpts f_ $ foldr ($) (build,Nothing,Nothing) args
  (_,_,err)    -> die (concat err)

  where
    evalOpts args (run,Just src,Just out) = parsed run src out args
    evalOpts args (run,src@(Just _),_) = evalOpts args (run,src,Just "bf.out")
    evalOpts (f:args) (run,_,out) = openFile f ReadMode
                                >>= hGetContents
                                >>= evalOpts args . (run,,out) . Just
    evalOpts _ _ = die "argument FILE required\n"

    parsed run src out args = case parseProg src of
      Left   err -> ioFail . intercalate "\n " . lines $ show err
      Right prog -> run prog out args

    options =
      [ Option "e" ["expr"] (ReqArg setSrc "EXPR") "specify source"
      , Option "o" ["out"]  (ReqArg setOut "FILE") "output file"
      , Option "s" ["asm"]  (NoArg  setAsm) "only generate asm code"
      , Option "x" ["exec"] (NoArg  setExe) "compile & run the source"
      ]

    setSrc src (run,_,out) = (run,Just src,out)
    setOut out (run,src,_) = (run,src,Just out)
    setExe (_,src,out) = (exec,src,out)
    setAsm (_,src,out) = (ioCompile,src,out)

    die s = ioFail $ s ++ "\n" ++ usageInfo usage options
    usage = "usage: brainflax86 [-s | -x] [-e SRC | SRC_FILE] [-o OUT_FILE]\n"


withOS :: (OS -> IO a) -> IO a
withOS io = io =<< case SysInfo.os of
  "darwin"  -> return osx
  "linux"   -> return linux
  "mingw32" -> return windows
  os -> ioFail $ "operating system '" ++ os ++ "' not supported.."


ioCompile :: Prog -> String -> [String] -> IO ()
ioCompile src out _ = withOS (ioCompileOS src out)

ioCompileOS :: Prog -> String -> OS -> IO ()
ioCompileOS src out os =
    bracket file hClose $ flip hPutStr (execBF $ compile src os)
  where file = case out of
          "bf.out" -> openFile "bf.out.s" WriteMode
          "-" -> return stdout
          f -> openFile f WriteMode


build :: Prog -> String -> [String] -> IO ()
build _   "-" _ = ioFail "please use a file other than stdout"
build src out _ = withOS (buildOS src out)

buildOS :: Prog -> String -> OS -> IO ()
buildOS src out os = do
  let sf = out ++ ".s"
  ioCompileOS src sf os
  (_,_,_,ph) <- createProcess $ proc (gcc os) ["-m32","-o",out ++ exe os,sf]
  waitForProcess ph >>= \case
    ExitSuccess -> removeFile sf
    _ -> ioFail $ concat ["'",gcc os," -m32 -o ",out,exe os," ",sf,"' failed.."]


exec :: Prog -> String -> [String] -> IO ()
exec src out args = withOS (execOS src out args)

execOS :: Prog -> String -> [String] -> OS -> IO ()
execOS src out args os = do
  buildOS src out os
  (_,_,_,ph) <-createProcess . shell $ concat ["./",out,exe os," ",unwords args]
  e <- waitForProcess ph
  removeFile out
  exitWith e


ioFail :: String -> IO err
ioFail err = ioError . userError $ "\n  " ++ err ++"\n"
