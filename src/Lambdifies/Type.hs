module Lambdifies.Type where


type Id = Int

data Native
  = NativePut
  | NativeGet

instance Show Native where
  showsPrec d NativePut = showChar 'p'
  showsPrec d NativeGet = showChar 'g'

data Expr
  = ExprS
  | ExprK
  | ExprNative Native
  | ExprApp Expr Expr
  | ExprId Id

instance Show Expr where
  showsPrec d ExprK =
    showChar 'K'
  showsPrec d ExprS =
    showChar 'S'
  showsPrec d (ExprNative n) =
    showChar 'N' . showsPrec d n
  showsPrec d (ExprApp efun epar) =
    showParen (d >= app_prec) $
    showsPrec 0 efun . showsPrec app_prec epar
    where
      app_prec = 10
  showsPrec d (ExprId i) =
    showChar '?' . showsPrec d i

newtype Env =
  Env [(Id, Expr)]

eEnv :: Env
eEnv = Env []
