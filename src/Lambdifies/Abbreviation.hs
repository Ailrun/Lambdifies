module Lambdifies.Abbreviation where

import Lambdifies.Type


s :: Expr
s = ExprS
k :: Expr
k = ExprK
p :: Expr
p = ExprNative NativePut
g :: Expr
g = ExprNative NativeGet
a :: Expr -> Expr -> Expr
a = ExprApp
i :: Expr
i = a (a s k) k
b :: Expr
b = a (a s (a k s)) k
u :: Expr
u = a (a s (a k (a s i))) (a (a s i) i)
y :: Expr
y = a (a s (a k (a (a s (a k (a (a s (a s i)) (a k (a (a s i) i))))) s))) k
