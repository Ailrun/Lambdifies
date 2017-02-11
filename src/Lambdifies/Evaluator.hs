module Lambdifies.Evaluator where

import Lambdifies.Parser
import Lambdifies.Type


initialEval :: Expr -> IO Expr
initialEval = eval eEnv

eval :: Env -> Expr -> IO Expr
eval env ExprS =
  return ExprS
eval env ExprK =
  return ExprK
eval env en@(ExprNative NativePut) =
  return en
eval env (ExprNative NativeGet) =
  do
    expr <- getLine >>= parseExpr
    return expr
eval env ea@(ExprApp ExprK _) =
  return ea
eval env ea@(ExprApp ExprS _) =
  return ea
eval env (ExprApp (ExprNative NativePut) epar) =
  do
    putStrLn . show $ epar
    eval env epar
    -- putStrLn (show epar)
    -- return epar
eval env (ExprApp (ExprApp ExprK epar1) epar2) =
  do
    -- epar1' <- eval env epar1
    -- eval env epar1'
    eval env epar1
eval env ea@(ExprApp (ExprApp ExprS _) _) =
  return ea
eval env (ExprApp (ExprApp (ExprApp ExprS epar1) epar2) epar3) =
  do
    -- epar1' <- eval env epar1
    -- epar2' <- eval env epar2
    -- epar3' <- eval env epar3
    -- eval env (ExprApp (ExprApp epar1' epar3') (ExprApp epar2' epar3'))
    eval env (ExprApp (ExprApp epar1 epar3) (ExprApp epar2 epar3))
eval env (ExprApp efun epar) =
  do
    -- putStrLn ("Debug :" ++ show efun ++ ", " ++ show epar)
    efun' <- eval env efun
    eval env (ExprApp efun' epar)
eval env (ExprId id) =
  do
    expr <- findExpr id env
    eval env expr

findExpr :: Id -> Env -> IO Expr
findExpr id (Env es) =
  let
    res = filter (\(eid, eexpr) -> eid == id) es
  in
    case res of
      [] ->
        error $ "Can't find id: " ++ show id
      ((_, expr):_) ->
        return expr
