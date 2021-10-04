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
vapp (VLam fun) v = fun v
vapp (VNeutral n) v = VNeutral (NApp n v)

eval :: NameEnv Value -> Term -> Value
eval e t = eval' t (e, [])

eval' :: Term -> (NameEnv Value, [Value]) -> Value
eval' (Bound ii) (_, lEnv) = lEnv !! ii
eval' (Free var) (entorno, _) = search var entorno 
                                    where search v [] = VNeutral (NFree v)
                                          search v ((name,val):xs) = if v == name then val else search v xs
eval' (t1 :@: t2) (entorno, lEnv) = vapp (eval' t1 (entorno, lEnv)) (eval' t2 (entorno, lEnv))
eval' (Lam t) (entorno, lEnv) = VLam (\val -> eval' t (entorno, val:lEnv))

--------------------------------
-- Sección 4 - Mostrando Valores
--------------------------------

quote :: Value -> Term
quote = undefined






