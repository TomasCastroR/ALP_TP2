module Untyped where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Common

----------------------------------------------
-- Seccón 2  
-- Ejercicio 2: Conversión a términos localmente sin nombres
----------------------------------------------

conversion :: LamTerm -> Term
conversion lamt = conversion' lamt []
                    where
                        conversion' (LVar v) binds = case elemIndex v binds of
                                                            Just n -> Bound n
                                                            _ -> Free (Global v)
                        conversion' (App t1 t2) binds = (conversion' t1 binds) :@: (conversion' t2 binds)
                        conversion' (Abs v t) binds = Lam (conversion' t (v:binds))

-------------------------------
-- Sección 3
-------------------------------

vapp :: Value -> Value -> Value
vapp = undefined

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' _          _         = undefined


--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote = undefined






