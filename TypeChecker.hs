{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module TypeChecker where
import Bnfc.Abs (Program, Ident (Ident), Type, Stmt, Type' (Int, Fun, Bool, Str), Program' (Program), Stmt' (FnDef, Empty, BStmt, Decl), Arg, Arg' (ValArg, RefArg), Block, Block' (Block), Expr, Expr' (EVar, ELitInt, ELitTrue, ELitFalse, EString, Neg, Not, EApp))
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask, local))
import Data.Map (Map, empty, insert, member, union)
import Control.Monad.Except (MonadError(throwError))
import qualified Data.Map as Map

type Err = Either String
type TypeMap = Map Ident Type
type CheckerReader a = ReaderT (TypeMap, Type) Err a

insertReader :: Ident -> Type -> (TypeMap, Type) -> (TypeMap, Type)
insertReader i ty (m',t) = (insert i ty m', t)

startType :: Type
startType = Int (Just (0,0))

typeChecker :: Program -> Err ()
typeChecker p = runReaderT (checkProgram p) (empty, startType)

argType :: Arg -> Type
argType (ValArg _ t _) = t
argType (RefArg _ t _) = t

checkProgram :: Program -> CheckerReader ()
checkProgram (Program _ ss) = do
    m <- local (\(_,t) -> (empty,t)) (preCheckStmts ss)
    local (\(m',t) -> (m `union` m', t)) (checkStmts ss)

checkBlock :: Block -> CheckerReader ()
checkBlock (Block _ ss) = do
    m <- local (\(_,t) -> (empty,t)) (preCheckStmts ss)
    local (\(m',t) -> (m `union` m', t)) (checkStmts ss)

checkStmts :: [Stmt] -> CheckerReader ()
checkStmts (FnDef _ _ _ _ b : ss) = checkBlock b >> checkStmts ss
checkStmts (Empty _ : ss) = checkStmts ss
checkStmts (BStmt _ b : ss) = checkBlock b >> checkStmts ss
checkStmts (Decl _ t i e : ss) = do
    t' <- checkExpr e
    matchType t t'
    local (insertReader i t) (checkStmts ss)
checkStmts (_:ss) = checkStmts ss
checkStmts [] = return ()

eqType :: Type -> Type -> Bool
eqType (Int _) (Int _) = True
eqType (Str _) (Str _) = True
eqType (Bool _) (Bool _) = True
eqType _ _ = False

matchType :: Type -> Type -> CheckerReader ()
matchType t1 t2 = if eqType t1 t2 then return () else throwError "type does not match"

safeLookup :: Ident -> CheckerReader Type
safeLookup i = do
    (m, _) <- ask
    case Map.lookup i m of
        Nothing -> throwError "variable not defined"
        Just x -> return x
checkExpr :: Expr -> CheckerReader Type
checkExpr (EVar p i) = safeLookup i
checkExpr (ELitInt p _) = return (Int p)
checkExpr (ELitTrue p) = return (Bool p)
checkExpr (ELitFalse p) = return (Bool p)
checkExpr (EApp p i es) = do
    f <- safeLookup i 
    case f of
        Fun p t ts -> return t
        _ -> throwError "called not a fun"
checkExpr (EString p _) = return (Str p)
checkExpr (Neg p e) = checkExpr e >>= matchType (Int p) >> return (Int p)
checkExpr (Not p e) = checkExpr e >>= matchType (Bool p) >> return (Bool p)


fixType :: [Stmt] -> [Stmt]
fixType (FnDef p t i as b : ss) = FnDef p (Fun p t (map argType as)) i as b : fixType ss
fixType (s:ss) = s : fixType ss
fixType [] = []

preCheckStmts :: [Stmt] -> CheckerReader TypeMap
preCheckStmts sts = preCheckStmts' (fixType sts) where
    preCheckStmts' [] = do
        (m, _) <- ask
        return m
    preCheckStmts' (FnDef _ ty i _ _:ss) = do
        (m, _) <- ask
        if member i m then
            let (Ident n) = i in throwError ("double definition of function " ++ n)
        else
            local (insertReader i ty) (preCheckStmts ss)
    preCheckStmts' (_:ss)  = preCheckStmts ss

