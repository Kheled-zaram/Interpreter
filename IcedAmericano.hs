module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile
  )
import System.IO
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )
import Data.Map (Map)
import qualified Data.Map as Map

import AbsIcedAmericano   ( Program )
import LexIcedAmericano   ( Token, mkPosToken )
import ParIcedAmericano   ( pProgram, myLexer )
import PrintIcedAmericano ( Print, printTree )
import Interpreter  ( runInterpreter )
import TypeChecker ( checkProgram )
import Control.Monad.Reader
import Control.Monad.Except

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = readFile f >>= run v p

checkType :: Program -> IO ()
checkType tree = do
  case (runReaderT (checkProgram tree) Map.empty) of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right () -> return ()

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
--      putStrLn "\nParse              Failed...\n"
--      putStrV v "Tokens:"
--      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      hPutStrLn stderr err
      exitFailure
    Right tree -> do
--      putStrLn "\nParse Successful!"
      checkType tree
      runInterpreter tree
--      showTree v tree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> getContents >>= run 2 pProgram
    fs         -> mapM_ (runFile 0 pProgram) fs
