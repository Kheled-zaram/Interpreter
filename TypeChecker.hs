{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module TypeChecker where

import Prelude (($), Either(..), String, (++), Show, show, Maybe(..), Bool(..), (&&), Int, (<=), (==), const)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import AbsIcedAmericano (Type, Ident(..), Expr)
import qualified AbsIcedAmericano as Abs
import ErrorMessage (showPos)
import qualified ErrorMessage as ErrM

type IdentState = Map.Map Ident Type

type IM a = ReaderT IdentState (Either String) a

type Position = (Int, Int)

insertArgsIntoIdentState :: IdentState -> [Abs.Arg] -> IdentState
insertArgsIntoIdentState s [] = s
insertArgsIntoIdentState s (arg : args) = insertArgsIntoIdentState (go s arg) args where
  go s (Abs.ValueArg _ argType argName) = Map.insert argName argType s
  go s (Abs.RefArg _ argType argName) = Map.insert argName argType s

getArgsType :: [Abs.Arg] -> [Type]
getArgsType args = fmap getType args where
  getType (Abs.ValueArg _ t _) = t
  getType (Abs.RefArg _ t _) = t

checkIfInt :: Abs.BNFC'Position -> Abs.Expr -> (Abs.BNFC'Position -> String) -> IM Type
checkIfInt pos expr errMsg = do
  t <- checkExpr expr
  case t of
    Abs.Int pos -> return t
    _ -> throwError $ errMsg pos

checkIfBool :: Abs.BNFC'Position -> Abs.Expr -> (Abs.BNFC'Position -> String) -> IM Type
checkIfBool pos expr errMsg = do
  t <- checkExpr expr
  case t of
    Abs.Bool pos -> return t
    _ -> throwError $ errMsg pos

checkOp :: Show a => a -> Abs.BNFC'Position -> Abs.Expr -> Abs.Expr -> (Abs.BNFC'Position -> Abs.Expr -> (Abs.BNFC'Position -> String) -> IM Type) -> IM Type
checkOp op pos expr1 expr2 opCheck = do
  t <- opCheck pos expr1 (ErrM.illegalOpErrMsg op)
  _ <- opCheck pos expr2 (ErrM.illegalOpErrMsg op)
  return t

checkRelOp :: Abs.BNFC'Position -> Abs.RelOp -> Type -> IM ()
checkRelOp pos op t@(Abs.Fun _ _ _) = throwError $ ErrM.illegalOpErrMsg op pos
checkRelOp pos op t@(Abs.Bool _) = do
  case op of
    Abs.LTH _ -> throwError $ ErrM.illegalOpErrMsg op pos
    Abs.LE _ -> throwError $ ErrM.illegalOpErrMsg op pos
    Abs.GTH _ -> throwError $ ErrM.illegalOpErrMsg op pos
    Abs.GE _ -> throwError $ ErrM.illegalOpErrMsg op pos
    _ -> return ()
checkRelOp _ _ _ = return ()

getPosFromType :: Type -> Abs.BNFC'Position
getPosFromType (Abs.Int pos) = pos
getPosFromType (Abs.Str pos) = pos
getPosFromType (Abs.Bool pos) = pos
getPosFromType (Abs.Fun pos _ _) = pos
getPosFromType (Abs.Print pos) = pos

inTheSameBlock :: Position -> Type -> Maybe Bool
inTheSameBlock blockPos varType = do
  varPos <- getPosFromType varType
  return $ blockPos <= varPos

sameType (Abs.Int _) (Abs.Int _) = True
sameType (Abs.Str _) (Abs.Str _) = True
sameType (Abs.Bool _) (Abs.Bool _) = True
sameType (Abs.Print _) t = printable t where
  printable (Abs.Bool _) = True
  printable (Abs.Int _) = True
  printable (Abs.Str _) = True
  printable _ = False
sameType t p@(Abs.Print _) = sameType p t
sameType (Abs.Fun pos1 fType1 argTypes1) (Abs.Fun pos2 fType2 argTypes2) = (sameType fType1 fType2) && (go argTypes1 argTypes2) where
  go [] [] = True
  go [] _ = False
  go _ [] = False
  go (x : xs) (y : ys) = (sameType x y) && (go xs ys)
sameType _ _ = False

checkExpr :: Abs.Expr -> IM Type
checkExpr (Abs.EVar pos x) = do
  state <- ask
  case (Map.lookup x state) of
    Just y -> return y
    Nothing -> throwError $ ErrM.varNotDefErrMsg pos x
checkExpr (Abs.ELitInt pos x) = return $ Abs.Int pos
checkExpr (Abs.ELitTrue pos) = return $ Abs.Bool pos
checkExpr (Abs.ELitFalse pos) = return $ Abs.Bool pos
checkExpr (Abs.EString pos _) = return $ Abs.Str pos
checkExpr (Abs.Neg pos expr) = checkIfInt pos expr ErrM.illegalNegErrMsg
checkExpr (Abs.Not pos expr) = checkIfBool pos expr ErrM.illegalNotErrMsg
checkExpr (Abs.EMul pos expr1 op expr2) = checkOp op pos expr1 expr2 checkIfInt
checkExpr (Abs.EAdd pos expr1 op expr2) = checkOp op pos expr1 expr2 checkIfInt
checkExpr (Abs.ERel pos expr1 op expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  case (sameType t1 t2) of
    False -> throwError $ ErrM.diffTypesWithOpErrMsg pos op
    True -> do
      checkRelOp pos op t1
      return (Abs.Bool pos)
checkExpr (Abs.EAnd pos expr1 expr2) = checkOp Abs.And pos expr1 expr2 checkIfBool
checkExpr (Abs.EOr pos expr1 expr2) = checkOp Abs.Or pos expr1 expr2 checkIfBool
checkExpr f@(Abs.EApp pos fName fArgs) = do
  state <- ask
  case (Map.lookup fName state) of
    Just (Abs.Fun declaredName declaredType declaredArgs) -> do
      _ <- checkArgs fArgs declaredArgs
      return declaredType
    _ -> throwError $ ErrM.fNotDefErrMsg pos fName
  where
    checkArgs :: [Expr] -> [Type] -> IM ()
    checkArgs [] [] = return ()
    checkArgs [] _ = throwError $ ErrM.fInvalidArgTypeErrMsg pos fName
    checkArgs _ [] = throwError $ ErrM.fInvalidArgTypeErrMsg pos fName
    checkArgs (expr : exprs) (t : ts) = do
      exprType <- checkExpr expr
      case (sameType exprType t) of
        True -> checkArgs exprs ts
        False -> throwError $ ErrM.fInvalidArgTypeErrMsg pos fName

checkStmt :: Type -> Position -> Abs.Stmt -> IM IdentState
checkStmt _ _ (Abs.Empty _) = ask
checkStmt _ blockPos (Abs.Decl pos itemType []) = throwError $ ErrM.emptyDeclErrMsg pos
checkStmt retType blockPos (Abs.Decl pos itemType (item : items)) = do
  s <- check pos itemType item
  case items of
    [] -> return s
    xs -> local (const s) (checkStmt retType blockPos (Abs.Decl pos itemType items))
  where
    check :: Abs.BNFC'Position -> Type -> Abs.Item -> IM IdentState
    check pos itemType (Abs.NoInit varPos varName) = do
      state <- ask
      case Map.lookup varName state of
        Just varType -> replaceVar (inTheSameBlock blockPos varType) varName where
          replaceVar :: Maybe Bool -> Ident -> IM IdentState
          replaceVar (Just True) varName = throwError $ ErrM.multipleDeclErrMsg pos varName
          replaceVar (Just False) varName = return (Map.insert varName varType state)
          replaceVar Nothing varName = throwError $ ErrM.noPositionErrMsg varName
        Nothing -> return (Map.insert varName itemType state)
    check pos itemType (Abs.Init varPos varName expr) = do
      state <- check pos itemType (Abs.NoInit varPos varName)
      exprType <- checkExpr expr
      case sameType itemType exprType of
        False -> throwError $ ErrM.assignTypeErrMsg itemType pos varName
        True -> return state
checkStmt _ blockPos (Abs.Ass pos varName expr) = do
  state <- ask
  case (Map.lookup varName state) of
    Just varType -> do
      exprType <- checkExpr expr
      case (sameType exprType varType) of
        True -> ask
        False -> throwError $ ErrM.assignTypeErrMsg varType pos varName
    Nothing -> throwError $ ErrM.varNotDefErrMsg pos varName
checkStmt retType blockPos (Abs.Ret pos expr) = do
  state <- ask
  exprType <- checkExpr expr
  case (sameType retType exprType) of
    True -> ask
    False -> throwError $ ErrM.retTypeErrMsg pos retType
checkStmt retType blockPos (Abs.Cond pos expr block) = checkStmt retType blockPos (Abs.CondElse pos expr block (Abs.Block pos [(Abs.Empty pos)]))
checkStmt retType blockPos (Abs.CondElse pos expr block1 block2) = do
  state <- ask
  checkIfBool pos expr (ErrM.invalidTypeExprErrMsg (Abs.Bool pos))
  case pos of
    Just p -> do
      checkBlock retType p block1
      checkBlock retType p block2
      ask
    Nothing -> throwError $ ErrM.noPositionErrMsg "if else block"
checkStmt retType blockPos (Abs.While pos expr block) = checkStmt retType blockPos (Abs.Cond pos expr block)
checkStmt _ _ (Abs.SExp pos expr) = do
  checkExpr expr
  ask
checkStmt _ _ (Abs.SFnDef _ topDef) = do
  state <- ask
  (fName, fType) <- checkTopDef topDef
  return (Map.insert fName fType state)

checkBlock :: Type -> Position -> Abs.Block -> IM ()
checkBlock _ _ (Abs.Block _ []) = return ()
checkBlock retType pos (Abs.Block _ stmts) = do
  go pos stmts
  return ()
  where
    go :: Position -> [Abs.Stmt] -> IM IdentState
    go pos [] = ask
    go pos (stmt : stmts) = do
       s <- checkStmt retType pos stmt
       local (const s) (go pos stmts)

checkTopDef :: Abs.TopDef -> IM (Ident, Type)
checkTopDef (Abs.FnDef pos fType fName args fBlock) = do
  state <- ask
  let sArgs = insertArgsIntoIdentState state args
  let argTypes = fmap getType args
  let t = Abs.Fun pos fType argTypes
  let s = Map.insert fName t sArgs
  case pos of
    Just p -> do
      local (const s) (checkBlock fType p fBlock)
      return (fName, t)
    Nothing -> throwError $ ErrM.noPositionErrMsg fName
  where
    getType :: Abs.Arg -> Type
    getType (Abs.ValueArg _ t _) = t
    getType (Abs.RefArg _ t _) = t

checkProgram' :: Abs.Program -> IM ()
checkProgram' (Abs.Program _ []) = return ()
checkProgram' (Abs.Program pos (topDef : topDefs)) = do
  state <- ask
  (fName, fType) <- checkTopDef topDef
  local (Map.insert fName fType) (checkProgram (Abs.Program pos topDefs))

checkProgram :: Abs.Program -> IM ()
checkProgram p = do
  state <- ask
  local addPrintln (checkProgram' p) where
    addPrintln = Map.insert printlnName printlnType

printlnType :: Abs.Type
printlnType = (Abs.Fun (Just (-1, 0)) (Abs.Bool (Just (-1, 1))) [(Abs.Print (Just (-1, 2)))])

printlnName :: Ident
printlnName = (Ident "println")

printlnArgIdent :: Ident
printlnArgIdent = Ident "printlnArg"

printlnTopDef :: Abs.TopDef
printlnTopDef = Abs.FnDef (Just (-1, 0)) printlnType printlnName [arg] Abs.PrintBlock where
  arg = Abs.ValueArg (Just (-1, 1)) (Abs.Print (Just (-1, 1))) printlnArgIdent