{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Interpreter where

import Prelude
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import System.IO
import TypeChecker (printlnName, printlnArgIdent, printlnTopDef)
import ErrorMessage (showPos)
import qualified ErrorMessage as ErrM
import AbsIcedAmericano (Type, Ident(..), Expr)
import qualified AbsIcedAmericano as Abs

data TValue = TInt Integer | TBool Bool | TStr String
  deriving Eq

instance Ord TValue where
  (TInt x) <= (TInt y) = x <= y
  (TStr x) <= (TStr y) = x <= y
  (TBool x) <= (TBool y) = True
  compare (TInt x) (TInt y) = compare x y
  compare (TStr x) (TStr y) = compare x y
  compare (TBool x) (TBool y) = EQ

instance Show TValue where
  show (TInt x) = show x
  show (TStr x) = show x
  show (TBool x) = show x

type Loc = Int

-- In location 0 the result of the most recent function execution is stored
newtype VarState = VarState (Map.Map Ident Loc, Map.Map Ident (Abs.TopDef, VarState))
type LocState = (Map.Map Loc (Maybe TValue))

type IM a = ReaderT VarState (StateT LocState (ExceptT String IO)) a -- type IM a = ReaderT VarState (StateT LocState (ExceptT String IO)) a

findVariable :: Abs.BNFC'Position -> Ident -> IM (Maybe TValue)
findVariable pos x = do
  VarState (identToLoc, fS) <- ask
  locToTValue <- get
  case (Map.lookup x identToLoc) of
    Just loc -> do
       case (Map.lookup loc locToTValue) of
         Just mVal -> return mVal
         _ -> throwError (ErrM.unexpectedErrMsg pos ++ "could not find the location of the variable " ++ (show x) ++ "'.")
    _ -> throwError (ErrM.unexpectedErrMsg pos ++ "could not find the variable '" ++ (show x) ++ "'.")

putVariable :: Ident -> Maybe TValue -> IM VarState
putVariable name val = do
  VarState (identToLoc, fS) <- ask
  locToTValue <- get
  let loc = findEmptyKey locToTValue
  put (Map.insert loc val locToTValue)
  return $ VarState (Map.insert name loc identToLoc, fS)

updateVariable :: Abs.BNFC'Position -> Ident -> Maybe TValue -> IM ()
updateVariable pos name val = do
  VarState (identToLoc, fS) <- ask
  locToTValue <- get
  case (Map.lookup name identToLoc) of
    Just loc -> put (Map.insert loc val locToTValue)
    Nothing -> throwError (ErrM.unexpectedErrMsg pos ++ "could not find and update the variable '" ++ (show name) ++ "'.")

findEmptyKey :: Map.Map Loc (Maybe TValue) -> Loc
findEmptyKey m = (Map.size m)

removeUnusedLocations :: Loc -> IM ()
removeUnusedLocations firstLoc = do
  state <- get
  let lastLoc = (Map.size state) - 1
  deleteLocs firstLoc lastLoc where
    deleteLocs :: Loc -> Loc -> IM()
    deleteLocs first last
      | first <= last = do
        s <- get
        put (Map.delete first s)
        deleteLocs (first + 1) last
      | otherwise = return ()

exprToInteger :: Abs.BNFC'Position -> Abs.Expr -> IM Integer
exprToInteger pos expr = do
  val <- evalExpr expr
  case val of
    TInt x -> return x
    _ -> throwError (ErrM.unexpectedErrMsg pos ++ "invalid type.")

exprToBool:: Abs.BNFC'Position -> Abs.Expr -> IM Bool
exprToBool pos expr = do
  val <- evalExpr expr
  case val of
   TBool x -> return x
   _ -> throwError (ErrM.unexpectedErrMsg pos ++ "invalid type.")

exprToString:: Abs.BNFC'Position -> Abs.Expr -> IM String
exprToString pos expr = do
  val <- evalExpr expr
  case val of
   TStr x -> return x
   _ -> throwError (ErrM.unexpectedErrMsg pos ++ "invalid type.")

evalExpr :: Abs.Expr -> IM TValue
evalExpr (Abs.EVar pos x) = do
  foundVal <- findVariable pos x
  case foundVal of
    Just val -> return val
    _ -> throwError $ ErrM.varNotInitErrMsg pos
evalExpr (Abs.ELitInt _ x) = return $ TInt x
evalExpr (Abs.ELitTrue _) = return $ TBool True
evalExpr (Abs.ELitFalse _) = return $ TBool False
evalExpr (Abs.EString _ s) = return $ TStr s
evalExpr (Abs.Neg pos expr) = do
  x <- exprToInteger pos expr
  return $ TInt (-x)
evalExpr (Abs.Not pos expr) = do
  x <- exprToBool pos expr
  return $ TBool (not x)
evalExpr (Abs.EMul pos expr1 op expr2) = do
  x <- exprToInteger pos expr1
  y <- exprToInteger pos expr2
  case op of
    Abs.Times _ -> return $ TInt (x * y)
    Abs.Div divPos -> case y of
      0 -> throwError $ ErrM.divByZeroErrMsg divPos
      _ -> return $ TInt (div x y)
    Abs.Mod modPos -> case y of
      0 -> throwError $ ErrM.divByZeroErrMsg modPos
      _ -> return $ TInt (mod x y)
evalExpr (Abs.EAdd pos expr1 op expr2) = do
  x <- exprToInteger pos expr1
  y <- exprToInteger pos expr2
  case op of
    Abs.Plus _ -> return $ TInt (x + y)
    Abs.Minus _ -> return $ TInt (x - y)
evalExpr (Abs.ERel pos expr1 op expr2) = do
  val1 <- evalExpr expr1
  val2 <- evalExpr expr2
  case op of
    Abs.LTH _ -> return $ TBool (val1 < val2)
    Abs.LE _ -> return $ TBool (val1 <= val2)
    Abs.GTH _ -> return $ TBool (val1 > val2)
    Abs.GE _ -> return $ TBool (val1 >= val2)
    Abs.EQU _ -> return $ TBool (val1 == val2)
    Abs.NE _ -> return $ TBool (val1 /= val2)
evalExpr (Abs.EAnd pos expr1 expr2) = do
  x <- exprToBool pos expr1
  case x of
    False -> return $ TBool False
    True -> do
      y <- exprToBool pos expr2
      return $ TBool (x && y)
evalExpr (Abs.EOr pos expr1 expr2) = do
  x <- exprToBool pos expr1
  case x of
    True -> return $ TBool False
    False -> do
      y <- exprToBool pos expr2
      return $ TBool (x || y)
evalExpr f@(Abs.EApp pos fName fArgs) = do
  VarState (identToLoc, fS) <- ask
  case (Map.lookup fName fS) of
    Nothing -> throwError (ErrM.unexpectedErrMsg pos ++ "could not find the function '" ++ (show fName) ++ "'.")
    Just fData@(Abs.FnDef declPos _ _ declArgs declBlock, s) -> do
      locState <- get
      let locStateSize = Map.size locState
      argIdLocs <- getArgLocs fArgs declArgs
      varS <- local (const s) (insertArgs argIdLocs)
      result <- local (const $ addFToVarState fName fData varS) (runBlock declBlock)
      state <- get
      put (Map.insert 0 Nothing state)
      removeUnusedLocations locStateSize
      case result of
        (Just x) -> return x
        Nothing -> throwError $ ErrM.fDidntRetErrMsg pos declPos

addFToVarState :: Ident -> (Abs.TopDef, VarState) -> VarState -> VarState
addFToVarState fName fData (VarState (idToLoc, fS)) = VarState (idToLoc, Map.insert fName fData fS)

getArgLocs :: [Abs.Expr] -> [Abs.Arg] -> IM [(Ident, Loc)]
getArgLocs [] [] = return []
getArgLocs (expr : exprs) (arg : args) = do
  argApp1 <- getArgLoc expr arg
  argApps <- getArgLocs exprs args
  return (argApp1 : argApps)
  where
    getArgLoc :: Abs.Expr -> Abs.Arg -> IM (Ident, Loc)
    getArgLoc expr (Abs.ValueArg _ _ argName) = do
      locState <- get
      val <- evalExpr expr
      let key = findEmptyKey locState
      put (Map.insert key (Just val) locState)
      return (argName, key)
    getArgLoc (Abs.EVar pos x) (Abs.RefArg p _ argName) = do
      VarState (identToLoc, fS) <- ask
      case (Map.lookup x identToLoc) of
        Just loc -> return (argName, loc)
        _ -> throwError (ErrM.unexpectedErrMsg pos ++ "could not find the location of the variable " ++ (show x) ++ "'.")
    getArgLoc expr (Abs.RefArg p t n) = getArgLoc expr (Abs.ValueArg p t n)
getArgLocs _ _ = throwError $ ErrM.unexpectedErrMsg Nothing


insertArgs :: [(Ident, Loc)] -> IM VarState
insertArgs [] = ask
insertArgs ((argName, loc) : args) = do
  VarState (idLoc, fS) <- ask
  let newIdLoc = Map.insert argName loc idLoc
  local (const $ VarState (newIdLoc, fS)) (insertArgs args)

runStmt :: Abs.Stmt -> IM VarState
runStmt (Abs.Empty _) = ask
runStmt (Abs.Decl pos itemType (item : items)) = do
  varState <- runOneDecl itemType item
  case items of
    [] -> return varState
    xs -> local (\x -> varState) (runStmt (Abs.Decl pos itemType items))
  where
    runOneDecl :: Type -> Abs.Item -> IM VarState
    runOneDecl itemType (Abs.NoInit varPos varName) = do
      varS <- putVariable varName Nothing
      return varS
    runOneDecl itemType (Abs.Init varPos varName expr) = do
      val <- evalExpr expr
      varS <- putVariable varName (Just val)
      return varS
runStmt (Abs.Ass pos varName expr) = do
  val <- evalExpr expr
  updateVariable pos varName (Just val)
  ask
runStmt (Abs.Ret pos expr) = do
  val <- evalExpr expr
  state <- get
  put (Map.insert 0 (Just val) state)
  ask
runStmt (Abs.Cond pos expr block) = do
  val <- evalExpr expr
  case val of
    TBool True -> do
      runBlock block
      ask
    _ -> ask
runStmt (Abs.CondElse pos expr block1 block2) = do
  val <- evalExpr expr
  case val of
    TBool True -> do
      runBlock block1
      ask
    _ -> do
      runBlock block2
      ask
runStmt (Abs.While pos expr block) = do
  val <- evalExpr expr
  case val of
    TBool True -> loop expr block
    _ -> ask
    where
        loop :: Abs.Expr -> Abs.Block -> IM (VarState)
        loop expr block = do
          runBlock block
          val <- evalExpr expr
          case val of
            TBool True -> loop expr block
            _ -> ask
runStmt (Abs.SExp pos expr) = do
  evalExpr expr
  ask
runStmt (Abs.SFnDef _ topDef@(Abs.FnDef _ _ fName _ _)) = do
  vS@(VarState (idToLoc, fS)) <- ask
  return $ VarState (idToLoc, Map.insert fName (topDef, vS) fS)
runStmt _ = throwError (ErrM.unexpectedErrMsg Nothing ++ "invalid statement.")

runBlock :: Abs.Block -> IM (Maybe TValue)
runBlock (Abs.Block _ stmts) = do
  runStmtList stmts
  state <- get
  case (Map.lookup 0 state) of
    (Just x) -> return x
    _ -> return Nothing
  where
    runStmtList :: [Abs.Stmt] -> IM ()
    runStmtList [] = return ()
    runStmtList (stmt : stmts) = do
       varS <- runStmt stmt
       state <- get
       case (Map.lookup 0 state) of
         Just (Just x) -> return ()
         _ -> local (const varS) (runStmtList stmts)
runBlock Abs.PrintBlock = do
  printedVal <- findVariable Nothing printlnArgIdent
  case printedVal of
    Just x -> do
      lift $ lift $ lift $ putStrLn (show x)
      state <- get
      put (Map.insert 0 (Just (TBool True)) state)
      return $ Just (TBool True)
    Nothing -> throwError (ErrM.unexpectedErrMsg Nothing ++ "println argument has no value.")

mainIdent :: Ident
mainIdent = Ident "main"

runProgram :: Abs.Program -> IM (Maybe TValue)
runProgram (Abs.Program pos []) = throwError $ ErrM.mainNotDefErrMsg pos
runProgram (Abs.Program programPos (topDef@(Abs.FnDef pos t id args block) : topDefs))
  | id == mainIdent = do
    case topDef of
      Abs.FnDef pos (Abs.Int _) _ [] _ -> runBlock block
      _ -> throwError $ ErrM.mainInvalidErrMsg pos
  | otherwise = do
    vS <- runStmt (Abs.SFnDef pos topDef)
    val <- local (const vS) (runProgram (Abs.Program programPos topDefs))
    return val


runInterpreter :: Abs.Program -> IO ()
runInterpreter p@(Abs.Program pos _) = do
  let emptyVarState = VarState (Map.empty, Map.empty)
  let varState = VarState (Map.empty, Map.insert printlnName (printlnTopDef, emptyVarState) Map.empty)
  let locState = Map.insert 0 Nothing Map.empty
  retVal <- runExceptT $ runStateT (runReaderT (runProgram p) varState) locState
  case retVal of
    Right (Just x, _) -> putStrLn ("Main returned: " ++ (show x) ++ ".")
    Right (Nothing, _) -> hPutStrLn stderr (ErrM.mainDidntRetErrMsg pos)
    Left err -> hPutStrLn stderr err
