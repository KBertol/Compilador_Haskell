module Semantico where

import AST

type TabelaGlobal = [(Id, ([Tipo], Tipo))]
type TabelaLocal = [(Id, Tipo)]

data Result a = Result (Bool, String, a) deriving Show

instance Functor Result where
  fmap f (Result (b, s, a)) = Result (b, s, f a)

instance Applicative Result where
  pure a = Result (False, "", a)
  Result (b1, s1, f) <*> Result (b2, s2, x) = Result (b1 || b2, s1 <> s2, f x)   

instance Monad Result where 
  Result (b, s, a) >>= f = let Result (b', s', a') = f a in Result (b || b', s++s', a')

erro :: String -> Result ()
erro s = Result (True, "Erro: " ++ s ++ "\n", ())
 
advertencia :: String -> Result ()
advertencia s = Result (False, "Advertencia: " ++ s ++ "\n", ())

getTipo :: Var -> Tipo 
getTipo (_ :#: (t, _)) = t

ctx :: String -> String -> String
ctx local msg = "[" ++ local ++ "] " ++ msg

construirGlobal :: [Funcao] -> Result TabelaGlobal
construirGlobal funcoes = auxiliar [] funcoes
   where
      auxiliar tg [] = return tg
      auxiliar tg ((nome :->: (params, ret)):resto) =
         case lookup nome tg of
            Just _ -> do
               erro $ ctx "global" $ "função '" ++ nome ++ "' declarada mais de uma vez"
               auxiliar tg resto 
            Nothing -> 
               auxiliar ((nome, (map getTipo params, ret)) : tg) resto
   
construirLocal :: String -> [Var] -> Result TabelaLocal
construirLocal local vars = auxiliar [] vars
   where
      auxiliar tl [] = return tl
      auxiliar tl ((nome :#: (tipo, _)):resto) =
         case lookup nome tl of
            Just _ -> do
               erro $ ctx local $ "variável '" ++ nome ++ "' declarada mais de uma vez"
               auxiliar tl resto
            Nothing -> 
               auxiliar ((nome, tipo) : tl) resto
 
analisarM :: Programa -> Result Programa
analisarM (Prog funcoes corpos vars bloco) = do
   tg      <- construirGlobal funcoes
   corpos' <- mapM (analisarFuncao tg funcoes) corpos
   tl      <- construirLocal "bloco principal" vars
   bloco'  <- mapM (analisarCmd tg tl TVoid "bloco principal") bloco
   return $ Prog funcoes corpos' vars bloco'

analisarFuncao :: TabelaGlobal -> [Funcao] -> (Id, [Var], Bloco) -> Result (Id, [Var], Bloco)
analisarFuncao tg funcoes (nome, vars, cmds) = do
   tl    <- construirLocal ("funcao '" ++ nome ++ "'") vars
   let ret = getTipoRetorno nome funcoes
   cmds' <- mapM (analisarCmd tg tl ret ("funcao '" ++ nome ++ "'")) cmds
   return (nome, vars, cmds')
   
getTipoRetorno :: Id -> [Funcao] -> Tipo
getTipoRetorno nome [] = error $ "interno: função '" ++ nome ++ "' não encontrada"
getTipoRetorno nome ((n :->: (_, ret)):fs)
   | nome == n = ret
   | otherwise = getTipoRetorno nome fs
   
analisarCmd :: TabelaGlobal -> TabelaLocal -> Tipo -> String -> Comando -> Result Comando

analisarCmd tg tl _ local (Atrib nome expr) =
   case lookup nome tl of
     Nothing   -> do
       erro $ ctx local $ "variável '" ++ nome ++ "' não declarada"
       (_, expr') <- analisarExpr tg tl local expr
       return (Atrib nome expr')
     Just tVar -> do
       (tExpr, expr') <- analisarExpr tg tl local expr
       case (tVar, tExpr) of
         (TDouble, TInt)    -> return $ Atrib nome (IntDouble expr')
         (TInt,    TDouble) -> do
           advertencia $ ctx local $ "coercao de Double para Int na variavel '" ++ nome ++ "'"
           return $ Atrib nome (DoubleInt expr')
         (t1, t2) | t1 == t2 -> return $ Atrib nome expr'
         _                    -> do
           erro $ ctx local $ "tipos incompatíveis na atribuição de '" ++ nome ++ "'"
           return $ Atrib nome expr'
 
analisarCmd tg tl ret local (If cond bt bf) = do
   cond' <- analisarExprL tg tl local cond
   bt'   <- mapM (analisarCmd tg tl ret local) bt
   bf'   <- mapM (analisarCmd tg tl ret local) bf
   return $ If cond' bt' bf'
 
analisarCmd tg tl ret local (While cond bloco) = do
   cond'  <- analisarExprL tg tl local cond
   bloco' <- mapM (analisarCmd tg tl ret local) bloco
   return $ While cond' bloco'
 
analisarCmd tg tl _ local (Imp expr) = do
   (_, expr') <- analisarExpr tg tl local expr
   return $ Imp expr'
 
analisarCmd tg tl _ local (Leitura nome) =
   case lookup nome tl of
     Nothing -> do
       erro $ ctx local $ "variável '" ++ nome ++ "' não declarada"
       return $ Leitura nome
     Just _  -> return $ Leitura nome
 
analisarCmd tg tl ret local (Ret Nothing) = return $ Ret Nothing
 
analisarCmd tg tl ret local (Ret (Just expr)) = do
   (tExpr, expr') <- analisarExpr tg tl local expr
   case (ret, tExpr) of
     (TVoid,   _)       -> do
       erro $ ctx local "função void não pode retornar valor"
       return $ Ret (Just expr')
     (TDouble, TInt)    -> return $ Ret (Just (IntDouble expr'))
     (TInt,    TDouble) -> do
       advertencia $ ctx local "coercao de Double para Int no retorno"
       return $ Ret (Just (DoubleInt expr'))
     (t1, t2) | t1 == t2 -> return $ Ret (Just expr')
     _                    -> do
       erro $ ctx local "tipo de retorno incompatível"
       return $ Ret (Just expr')
 
analisarCmd tg tl _ local (Proc nome args) = do
   resultado <- analisarExpr tg tl local (Chamada nome args)
   case resultado of
     (_, Chamada _ args') -> return $ Proc nome args'
     _                    -> return $ Proc nome args

   
analisarExpr :: TabelaGlobal -> TabelaLocal -> String -> Expr -> Result (Tipo, Expr)
 
analisarExpr _  _  _     (Const (CInt n))    = return (TInt,    Const (CInt n))
analisarExpr _  _  _     (Const (CDouble d)) = return (TDouble, Const (CDouble d))
analisarExpr _  _  _     (Lit s)             = return (TString, Lit s)

analisarExpr _  tl local (IdVar nome) =
   case lookup nome tl of
     Just t  -> return (t, IdVar nome)
     Nothing -> do erro $ ctx local $ "variável '" ++ nome ++ "' não declarada"
                   return (TInt, IdVar nome)

analisarExpr tg tl local (Neg expr) = do
   (t, expr') <- analisarExpr tg tl local expr
   case t of
        TInt -> return (TInt,    Neg expr')
        TDouble -> return (TDouble, Neg expr')
        _ -> do erro $ ctx local "operador unário '-' aplicado a tipo inválido"
                return (TInt, Neg expr')

analisarExpr tg tl local (Add e1 e2) = analisarBinario tg tl local Add e1 e2
analisarExpr tg tl local (Sub e1 e2) = analisarBinario tg tl local Sub e1 e2
analisarExpr tg tl local (Mul e1 e2) = analisarBinario tg tl local Mul e1 e2
analisarExpr tg tl local (Div e1 e2) = analisarBinario tg tl local Div e1 e2

analisarExpr tg tl local (Chamada nome args) =
   case lookup nome tg of
     Nothing -> do erro $ ctx local $ "função '" ++ nome ++ "' não declarada"
                   return (TInt, Chamada nome args)
     Just (params, ret)  ->
       if length args /= length params
         then do erro $ ctx local $ "número errado de parâmetros em '" ++ nome ++ "'"
                 return (ret, Chamada nome args)
       else do args' <- converterListaParams tg tl local nome params args
               return (ret, Chamada nome args')

analisarExpr tg tl local (IntDouble e) = do
   (_, e') <- analisarExpr tg tl local e
   return (TDouble, IntDouble e')

analisarExpr tg tl local (DoubleInt e) = do
   (_, e') <- analisarExpr tg tl local e
   return (TInt, DoubleInt e')
 
   
analisarBinario :: TabelaGlobal -> TabelaLocal -> String -> (Expr -> Expr -> Expr) -> Expr -> Expr -> Result (Tipo, Expr)
analisarBinario tg tl local op e1 e2 = do
   (t1, e1') <- analisarExpr tg tl local e1
   (t2, e2') <- analisarExpr tg tl local e2
   case (t1, t2) of
     (TInt,    TInt)    -> return (TInt,    op e1' e2')
     (TDouble, TDouble) -> return (TDouble, op e1' e2')
     (TInt,    TDouble) -> return (TDouble, op (IntDouble e1') e2')
     (TDouble, TInt)    -> return (TDouble, op e1' (IntDouble e2'))
     _                  -> do
       erro $ ctx local "tipos incompatíveis em expressão aritmética"
       return (TInt, op e1' e2')


converterParam :: TabelaGlobal -> TabelaLocal -> String -> String -> Tipo -> Expr -> Result Expr
converterParam tg tl local nomeFuncao esperado arg = do 
   (tArg, arg') <- analisarExpr tg tl local arg
   case (esperado, tArg) of
     (TDouble, TInt)    -> return $ IntDouble arg'
     (TInt,    TDouble) -> do
       advertencia $ ctx local $ "coercao de Double para Int no parametro da funcao '" ++ nomeFuncao ++ "'"
       return $ DoubleInt arg'
     (t1, t2) | t1 == t2 -> return arg'
     _                    -> do
       erro $ ctx local $ "tipo incompatível em parâmetro de '" ++ nomeFuncao ++ "'"
       return arg'
       
converterListaParams :: TabelaGlobal -> TabelaLocal -> String -> String -> [Tipo] -> [Expr] -> Result [Expr]
converterListaParams _ _ _ _ [] []         = return []
converterListaParams tg tl local nf (t:ts) (e:es) = do
   e'  <- converterParam tg tl local nf t e
   es' <- converterListaParams tg tl local nf ts es
   return (e' : es')
converterListaParams _ _ _ _ _ _           = return []

analisarExprL :: TabelaGlobal -> TabelaLocal -> String -> ExprL -> Result ExprL
analisarExprL tg tl local (And e1 e2) = do
   e1' <- analisarExprL tg tl local e1
   e2' <- analisarExprL tg tl local e2
   return $ And e1' e2'
analisarExprL tg tl local (Or e1 e2) = do
   e1' <- analisarExprL tg tl local e1
   e2' <- analisarExprL tg tl local e2
   return $ Or e1' e2'
analisarExprL tg tl local (Not e) = do
   e' <- analisarExprL tg tl local e
   return $ Not e'
analisarExprL tg tl local (Rel exprR) = do
   exprR' <- analisarExprR tg tl local exprR
   return $ Rel exprR'

analisarExprR :: TabelaGlobal -> TabelaLocal -> String -> ExprR -> Result ExprR
analisarExprR tg tl local exprR = do
   let (op, e1, e2) = extraiRel exprR
   (t1, e1') <- analisarExpr tg tl local e1
   (t2, e2') <- analisarExpr tg tl local e2
   case (t1, t2) of
     (TString, TString) -> return $ op e1' e2'
     (TString, _)       -> do
       erro $ ctx local "tipos incompatíveis em expressão relacional"
       return $ op e1' e2'
     (_, TString)       -> do
       erro $ ctx local "tipos incompatíveis em expressão relacional"
       return $ op e1' e2'
     (TInt,    TInt)    -> return $ op e1' e2'
     (TDouble, TDouble) -> return $ op e1' e2'
     (TInt,    TDouble) -> return $ op (IntDouble e1') e2'
     (TDouble, TInt)    -> return $ op e1' (IntDouble e2')
     _                  -> do
       erro $ ctx local "tipos incompatíveis em expressão relacional"
       return $ op e1' e2'
   where
     extraiRel (Req  a b) = (Req, a, b)
     extraiRel (Rdif a b) = (Rdif, a, b)
     extraiRel (Rlt  a b) = (Rlt, a, b)
     extraiRel (Rgt  a b) = (Rgt, a, b)
     extraiRel (Rle  a b) = (Rle, a, b)
     extraiRel (Rge  a b) = (Rge, a, b)

