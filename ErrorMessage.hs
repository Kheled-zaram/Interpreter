module ErrorMessage where

import Prelude (String, (++), Show, show, Maybe(..))
import qualified AbsIcedAmericano as Abs

showPos :: Abs.BNFC'Position -> String
showPos (Just x) = (show x) ++ " "
showPos Nothing = ""

-- TypeChecker error messages:
generalTypeErrMsg :: Abs.BNFC'Position -> String
generalTypeErrMsg pos = (showPos pos) ++ "TypeError: "

illegalNegErrMsg :: Abs.BNFC'Position -> String
illegalNegErrMsg pos = generalTypeErrMsg pos ++ "can only negate expressions of type 'int'."

illegalNotErrMsg :: Abs.BNFC'Position -> String
illegalNotErrMsg pos = generalTypeErrMsg pos ++ "can only apply 'not' to expression of type 'boolean'."

diffTypesWithOpErrMsg :: Show a => Abs.BNFC'Position -> a -> String
diffTypesWithOpErrMsg pos op = generalTypeErrMsg pos ++ "cannot apply operator '" ++ (show op) ++ "' to expressions of different types."

illegalOpErrMsg :: Show a => a -> Abs.BNFC'Position -> String
illegalOpErrMsg op pos = generalTypeErrMsg pos ++ "operator '" ++ (show op) ++ "' cannot be aplied to this expression."

varNotDefErrMsg :: Abs.BNFC'Position -> Abs.Ident -> String
varNotDefErrMsg pos x = generalTypeErrMsg pos ++ "variable not defined: '" ++ (show x) ++ "'."

fNotDefErrMsg :: Abs.BNFC'Position -> Abs.Ident -> String
fNotDefErrMsg pos x = generalTypeErrMsg pos ++ "function not defined: '" ++ (show x) ++ "'."

fInvalidArgTypeErrMsg :: Abs.BNFC'Position -> Abs.Ident -> String
fInvalidArgTypeErrMsg pos x = generalTypeErrMsg pos ++ "invalid arguments in function call: '" ++ (show x) ++ "'."

assignTypeErrMsg :: Abs.Type -> Abs.BNFC'Position -> Abs.Ident -> String
assignTypeErrMsg t pos x = generalTypeErrMsg pos ++ "expression assigned to '" ++ (show x) ++ "' must be of the type: '" ++ (show t) ++ "'."

retTypeErrMsg :: Abs.BNFC'Position -> Abs.Type -> String
retTypeErrMsg pos retType = generalTypeErrMsg pos ++ "the returned type must be '" ++ (show retType) ++ "'."

invalidTypeExprErrMsg :: Abs.Type -> Abs.BNFC'Position -> String
invalidTypeExprErrMsg t pos = generalTypeErrMsg pos ++ "the expression must be of the type '" ++ (show t) ++ "'."

multipleDeclErrMsg :: Abs.BNFC'Position -> Abs.Ident -> String
multipleDeclErrMsg pos x = generalTypeErrMsg pos ++ "multiple declarations of '" ++ (show x) ++ "'."

emptyDeclErrMsg :: Abs.BNFC'Position -> String
emptyDeclErrMsg pos = generalTypeErrMsg pos ++ "empty declaration."

noPositionErrMsg :: Show a => a -> String
noPositionErrMsg x = "No position for: " ++ (show x)

-- Execution error messages:
generalErrMsg :: Abs.BNFC'Position -> String
generalErrMsg pos = (showPos pos) ++ "Execution Error: "

varNotInitErrMsg :: Abs.BNFC'Position -> String
varNotInitErrMsg pos = (generalErrMsg pos) ++ "variable has not been initialized."

divByZeroErrMsg :: Abs.BNFC'Position -> String
divByZeroErrMsg pos = (generalErrMsg pos) ++ "division or modulo by zero."

fDidntRetErrMsg :: Abs.BNFC'Position -> Abs.BNFC'Position -> String
fDidntRetErrMsg pos declPos = (generalErrMsg pos) ++ "function declared in " ++ (showPos declPos) ++ " did not return any value."

mainInvalidErrMsg :: Abs.BNFC'Position -> String
mainInvalidErrMsg pos = (generalErrMsg pos) ++ "invalid declaration of 'main'."

mainNotDefErrMsg :: Abs.BNFC'Position -> String
mainNotDefErrMsg pos = (generalErrMsg pos) ++ "function 'main' has not been declared."

mainDidntRetErrMsg :: Abs.BNFC'Position -> String
mainDidntRetErrMsg pos = (generalErrMsg pos) ++ "'main' did not return any value."

-- Unexpected error message is only for debugging reasons.
-- It should never occur while running Interpreter (these errors should be caught later by TypeChecker).
unexpectedErrMsg :: Abs.BNFC'Position -> String
unexpectedErrMsg pos = (showPos pos) ++ "Unexpected Execution Error: "
