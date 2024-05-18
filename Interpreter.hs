module Interpreter where

import qualified Bnfc.Abs as Abs
import Common (throwErrorPos)
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadIO (liftIO), runExceptT)
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (runReaderT), asks)
import Control.Monad.State (StateT, evalStateT, gets, modify)
import qualified Data.Map as Map

type Err = Either String

type Loc = Int

type Env = Map.Map Abs.Ident Loc

type Stt = Map.Map Loc Value

data Value = TEmpty | TInt Integer | TStr String | TBool Bool | TFun Abs.Type Env [Abs.Arg] Abs.Block

type InterpreterMonad a = ExceptT String (ReaderT Env (StateT Stt IO)) a

interpreter :: Abs.Program -> IO (Err ())
interpreter p = evalStateT (runReaderT (runExceptT (evalProgram p)) Map.empty) Map.empty

putNewVal :: Abs.Ident -> Value -> InterpreterMonad Env
putNewVal i v = do
  l <- gets Map.size
  modify (Map.insert l v)
  asks (Map.insert i l)

getVal :: Abs.Ident -> InterpreterMonad Value
getVal i = do
  l <- asks (Map.lookup i)
  case l of
    Nothing -> throwError "no location for variable"
    Just l' -> do
      v <- gets (Map.lookup l')
      case v of
        Nothing -> throwError "no value for location"
        Just v' -> return v'

changeVal :: Abs.Ident -> Value -> InterpreterMonad ()
changeVal i v = do
  l <- asks (Map.lookup i)
  case l of
    Nothing -> throwError "no location for variable"
    Just l' -> modify (Map.insert l' v)

evalProgram :: Abs.Program -> InterpreterMonad ()
evalProgram (Abs.Program _ ss) = evalStmts ss >> return ()

evalStmts :: [Abs.Stmt] -> InterpreterMonad Value
evalStmts (Abs.FnDef _ t i as b : ss) = do
  oe <- ask
  ne <- putNewVal i (TFun t oe as b)
  local (const ne) (evalStmts ss)
evalStmts (Abs.Empty _ : ss) = evalStmts ss
evalStmts (Abs.BStmt _ b : ss) = do
  v <- evalBlock b
  case v of
    TEmpty -> evalStmts ss
    _ -> return v
evalStmts (Abs.Decl _ _ i e : ss) = do
  v <- evalExpr e
  ne <- putNewVal i v
  local (const ne) (evalStmts ss)
evalStmts (Abs.Ass _ i e : ss) = do
  v <- evalExpr e
  changeVal i v
  evalStmts ss
evalStmts (Abs.Incr p i : ss) = evalStmts (Abs.Ass p i (Abs.EAdd p (Abs.EVar p i) (Abs.Plus p) (Abs.ELitInt p 1)) : ss)
evalStmts (Abs.Decr p i : ss) = evalStmts (Abs.Ass p i (Abs.EAdd p (Abs.EVar p i) (Abs.Minus p) (Abs.ELitInt p 1)) : ss)
evalStmts (Abs.Ret _ e : _) = do
  evalExpr e
evalStmts (Abs.Cond _ e s : ss) = do
  v <- evalExpr e
  if getBool v
    then evalStmts (s : ss)
    else evalStmts ss
evalStmts (Abs.CondElse _ e s1 s2 : ss) = do
  v <- evalExpr e
  if getBool v
    then evalStmts (s1 : ss)
    else evalStmts (s2 : ss)
evalStmts (Abs.While p e s : ss) = do
  v <- evalExpr e
  if getBool v
    then evalStmts (s : Abs.While p e s : ss)
    else evalStmts ss
evalStmts (Abs.SExp _ e : ss) = evalExpr e >> evalStmts ss
evalStmts (Abs.Print _ e : ss) = do
  v <- evalExpr e
  _ <- printVal v
  evalStmts ss
evalStmts [] = return TEmpty

evalBlock :: Abs.Block -> InterpreterMonad Value
evalBlock (Abs.Block _ ss) = evalStmts ss

evalExpr :: Abs.Expr -> InterpreterMonad Value
evalExpr (Abs.EVar _ i) = getVal i
evalExpr (Abs.ELitInt _ d) = return (TInt d)
evalExpr (Abs.ELitTrue _) = return (TBool True)
evalExpr (Abs.ELitFalse _) = return (TBool False)
evalExpr (Abs.EString _ s) = return (TStr s)
evalExpr (Abs.Neg _ e) = do
  v <- evalExpr e
  return (TInt (-(getInt v)))
evalExpr (Abs.Not _ e) = do
  v <- evalExpr e
  return (TBool (not (getBool v)))
evalExpr (Abs.EMul _ e1 o e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  v3 <- appMulOp o (getInt v1) (getInt v2)
  return (TInt v3)
evalExpr (Abs.EAdd _ e1 o e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  v3 <- appAddOp o (getInt v1) (getInt v2)
  return (TInt v3)
evalExpr (Abs.ERel _ e1 o e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  v3 <- appRelOp o (getInt v1) (getInt v2)
  return (TBool v3)
evalExpr (Abs.EAnd _ e1 e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  return (TBool (getBool v1 && getBool v2))
evalExpr (Abs.EOr _ e1 e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  return (TBool (getBool v1 || getBool v2))
evalExpr (Abs.EApp _ i es) = do
  v1 <- getVal i
  let (t, e1, as, b) = getFun v1
  e2 <- putNewVal i (TFun t e1 as b)
  e3 <- addArgs es as e2
  v2 <- local (const e3) (evalBlock b)
  return (getRetVal v2 t)

addArgs :: [Abs.Expr] -> [Abs.Arg] -> Env -> InterpreterMonad Env
addArgs (Abs.EVar _ i1 : es) (Abs.RefArg _ _ i2 : as) en = do
  l <- asks (Map.lookup i1)
  case l of
    Nothing -> throwError "no location for variable"
    Just l' -> do
      addArgs es as (Map.insert i2 l' en)
addArgs (e : es) (a : as) en1 = do
  v <- evalExpr e
  let i = getArgId a
  en2 <- local (const en1) (putNewVal i v)
  addArgs es as en2
addArgs _ _ e = return e

getArgId :: Abs.Arg -> Abs.Ident
getArgId (Abs.ValArg _ _ i) = i
getArgId (Abs.RefArg _ _ i) = i

appMulOp :: Abs.MulOp -> Integer -> Integer -> InterpreterMonad Integer
appMulOp (Abs.Times _) v1 v2 = return (v1 * v2)
appMulOp (Abs.Div p) _ 0 = throwErrorPos p "division by zero"
appMulOp (Abs.Div _) v1 v2 = return (v1 `div` v2)
appMulOp (Abs.Mod p) _ 0 = throwErrorPos p "division by zero"
appMulOp (Abs.Mod _) v1 v2 = return (v1 `mod` v2)

appAddOp :: Abs.AddOp -> Integer -> Integer -> InterpreterMonad Integer
appAddOp (Abs.Plus _) v1 v2 = return (v1 + v2)
appAddOp (Abs.Minus _) v1 v2 = return (v1 - v2)

appRelOp :: Abs.RelOp -> Integer -> Integer -> InterpreterMonad Bool
appRelOp (Abs.LTH _) v1 v2 = return (v1 < v2)
appRelOp (Abs.LE _) v1 v2 = return (v1 <= v2)
appRelOp (Abs.GTH _) v1 v2 = return (v1 > v2)
appRelOp (Abs.GE _) v1 v2 = return (v1 >= v2)
appRelOp (Abs.EQU _) v1 v2 = return (v1 == v2)
appRelOp (Abs.NE _) v1 v2 = return (v1 /= v2)

getInt :: Value -> Integer
getInt (TInt i) = i
getInt TEmpty = 0
getInt _ = undefined

getBool :: Value -> Bool
getBool (TBool b) = b
getBool TEmpty = False
getBool _ = undefined

getFun :: Value -> (Abs.Type, Env, [Abs.Arg], Abs.Block)
getFun (TFun t e as b) = (t, e, as, b)
getFun _ = undefined

getRetVal :: Value -> Abs.Type -> Value
getRetVal TEmpty (Abs.Int _) = TInt 0
getRetVal TEmpty (Abs.Str _) = TStr ""
getRetVal TEmpty (Abs.Bool _) = TBool False
getRetVal v _ = v

printVal :: Value -> InterpreterMonad ()
printVal TEmpty = return ()
printVal (TInt i) = liftIO (print i)
printVal (TStr s) = liftIO (putStrLn s)
printVal (TBool b) = liftIO (print b)
printVal (TFun {}) = return ()