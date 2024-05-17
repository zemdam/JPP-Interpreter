-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Use first" #-}
{-# LANGUAGE FlexibleContexts #-}
module TypeChecker where
import qualified Bnfc.Abs as Abs
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask, local), zipWithM_)
import Control.Monad.Except (MonadError(throwError))
import qualified Data.Map as Map

type Err = Either String
type TypeMap = Map.Map Abs.Ident Abs.Type
type CheckerReader a = ReaderT (TypeMap, Abs.Type) Err a

insertReader :: Abs.Ident -> Abs.Type -> (TypeMap, Abs.Type) -> (TypeMap, Abs.Type)
insertReader i ty (m',t) = (Map.insert i ty m', t)

goInFunReader :: Abs.Ident -> Abs.Type -> TypeMap -> (TypeMap, Abs.Type) -> (TypeMap, Abs.Type)
goInFunReader i ty m (m',_) = (Map.union m (Map.insert i ty m'), ty)

throwErrorPos :: MonadError String m => Abs.BNFC'Position -> String -> m a
throwErrorPos Nothing e = throwError e
throwErrorPos (Just (l,_)) e = throwError (e ++ " at line " ++ show l)

startType :: Abs.Type
startType = Abs.Int (Just (0,0))

typeChecker :: Abs.Program -> Err ()
typeChecker p = runReaderT (checkProgram p) (Map.empty, startType)

checkProgram :: Abs.Program -> CheckerReader ()
checkProgram (Abs.Program _ ss) = checkStmts ss

checkStmts :: [Abs.Stmt] -> CheckerReader ()
checkStmts s = checkStmts' (fixType s) where
    checkStmts' (Abs.FnDef _ t i as b : ss) = do
        m <- checkArgs as
        local (goInFunReader i t m) (checkBlock b)
        local (insertReader i t) (checkStmts' ss)
    checkStmts' (Abs.Empty _ : ss) = checkStmts' ss
    checkStmts' (Abs.BStmt _ b : ss) = checkBlock b >> checkStmts' ss
    checkStmts' (Abs.Decl p t i e : ss) = do
        t' <- checkExpr e
        _ <- matchType p t t'
        local (insertReader i t) (checkStmts' ss)
    checkStmts' (Abs.Ass p i e : ss) = do
        t1 <- safeLookup p i
        t2 <- checkExpr e
        _ <- matchType p t1 t2
        checkStmts' ss
    checkStmts' (Abs.Incr p i : ss) = do
        t <- safeLookup p i
        _ <- matchType p t (Abs.Int p)
        checkStmts' ss
    checkStmts' (Abs.Decr p i : ss) = do
        t <- safeLookup p i
        _ <- matchType p t (Abs.Int p)
        checkStmts' ss
    checkStmts' (Abs.Ret p e : ss) = do
        (_,t1) <- ask
        t2 <- checkExpr e
        _ <- matchType p t1 t2
        checkStmts' ss
    checkStmts' (Abs.Cond p e s' : ss) = do
        t <- checkExpr e
        _ <- matchType p t (Abs.Bool p)
        condStmt s'
        checkStmts' ss
    checkStmts' (Abs.CondElse p e s1 s2 : ss) = do
        t <- checkExpr e
        _ <- matchType p t (Abs.Bool p)
        condStmt s1
        condStmt s2
        checkStmts' ss
    checkStmts' (Abs.While p e s' : ss) = do
        t <- checkExpr e
        _ <- matchType p t (Abs.Bool p)
        condStmt s'
        checkStmts' ss
    checkStmts' (Abs.SExp _ e : ss) = checkExpr e >> checkStmts' ss
    checkStmts' (Abs.Print _ e : ss) = checkExpr e >> checkStmts' ss
    checkStmts' [] = return ()

condStmt :: Abs.Stmt -> CheckerReader ()
condStmt (Abs.FnDef p _ _ _ _) = throwErrorPos p "conditional definition"
condStmt (Abs.Decl p _ _ _) = throwErrorPos p "conditional declaration"  
condStmt s = checkStmts [s]

checkBlock :: Abs.Block -> CheckerReader ()
checkBlock (Abs.Block _ ss) = checkStmts ss

checkArgs :: [Abs.Arg] -> CheckerReader TypeMap
checkArgs (Abs.ValArg p t i : as) = do
    (m,_) <- ask
    if Map.member i m then
        throwErrorPos p "duplicate parametr name"
    else
        local (insertReader i t) (checkArgs as)
checkArgs (Abs.RefArg p t i : as) = do
    (m,_) <- ask
    if Map.member i m then
        throwErrorPos p "duplicate parametr name"
    else
        local (insertReader i t) (checkArgs as)
checkArgs [] = do
    (m,_) <- ask
    return m

matchType :: Abs.BNFC'Position -> Abs.Type -> Abs.Type -> CheckerReader Abs.Type
matchType p t1 t2 = if eqType t1 t2 then return t1 else throwErrorPos p "type does not match"

eqType :: Abs.Type -> Abs.Type -> Bool
eqType (Abs.Int _) (Abs.Int _) = True
eqType (Abs.Str _) (Abs.Str _) = True
eqType (Abs.Bool _) (Abs.Bool _) = True
eqType (Abs.Fun _ (Abs.Int _) _) (Abs.Int _) = True
eqType (Abs.Fun _ (Abs.Str _) _) (Abs.Str _) = True
eqType (Abs.Fun _ (Abs.Bool _) _) (Abs.Bool _) = True
eqType (Abs.Int _) (Abs.Fun _ (Abs.Int _) _) = True
eqType (Abs.Str _) (Abs.Fun _ (Abs.Str _) _)  = True
eqType (Abs.Bool _) (Abs.Fun _ (Abs.Bool _) _) = True
eqType _ _ = False

checkExpr :: Abs.Expr -> CheckerReader Abs.Type
checkExpr (Abs.EVar p i) = safeLookup p i
checkExpr (Abs.ELitInt p _) = return (Abs.Int p)
checkExpr (Abs.ELitTrue p) = return (Abs.Bool p)
checkExpr (Abs.ELitFalse p) = return (Abs.Bool p)
checkExpr (Abs.EApp p i es) = do
    f <- safeLookup p i
    case f of
        Abs.Fun _ t ts ->
            if length es /= length ts then
                throwErrorPos p "called fun with wrong number of arguments"
            else do
                ts' <- mapM checkExpr es
                zipWithM_ (matchType p) ts ts'
                return t
        _ -> throwErrorPos p "called not a fun"
checkExpr (Abs.EString p _) = return (Abs.Str p)
checkExpr (Abs.Neg p e) = checkExpr e >>= matchType p (Abs.Int p)
checkExpr (Abs.Not p e) = checkExpr e >>= matchType p (Abs.Bool p)
checkExpr (Abs.EMul p e1 _ e2) = matchOp p (Abs.Int p) e1 e2
checkExpr (Abs.EAdd p e1 _ e2) = matchOp p (Abs.Int p) e1 e2
checkExpr (Abs.ERel p e1 _ e2) = matchOp p (Abs.Bool p) e1 e2
checkExpr (Abs.EAnd p e1 e2) = matchOp p (Abs.Bool p) e1 e2
checkExpr (Abs.EOr p e1 e2) = matchOp p (Abs.Bool p) e1 e2

matchOp :: Abs.BNFC'Position -> Abs.Type -> Abs.Expr -> Abs.Expr -> CheckerReader Abs.Type
matchOp p t e1 e2 = do
    t1 <- checkExpr e1
    _ <- matchType p t t1
    t2 <- checkExpr e2
    matchType p t t2

safeLookup :: Abs.BNFC'Position -> Abs.Ident -> CheckerReader Abs.Type
safeLookup p i = do
    (m, _) <- ask
    case Map.lookup i m of
        Nothing -> throwErrorPos p "variable not defined"
        Just x -> return x

fixType :: [Abs.Stmt] -> [Abs.Stmt]
fixType (Abs.FnDef p t i as b : ss) = Abs.FnDef p (Abs.Fun p t (map argType as)) i as b : fixType ss
fixType (s:ss) = s : fixType ss
fixType [] = []

argType :: Abs.Arg -> Abs.Type
argType (Abs.ValArg _ t _) = t
argType (Abs.RefArg _ t _) = t

