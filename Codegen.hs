module Codegen where

import AST
import Control.Monad.State

-- ─── Estado: contador de labels ──────────────────────────────────────────────

type Gen a = State Int a

novoLabel :: Gen String
novoLabel = do
  n <- get
  put (n+1)
  return ("L" ++ show n)

-- ─── Tabelas ─────────────────────────────────────────────────────────────────

type TabVars    = [(Id, (Int, Tipo))]
type TabFuncoes = [(Id, ([Tipo], Tipo))]

getTipo :: Var -> Tipo
getTipo (_ :#: (t, _)) = t

tamanhoTipo :: Tipo -> Int
tamanhoTipo TDouble = 2
tamanhoTipo _       = 1

construirTabVars :: Int -> [Var] -> TabVars
construirTabVars slotInicial vars = auxiliar slotInicial vars
  where
    auxiliar _ [] = []
    auxiliar slot ((nome :#: (tipo, _)):resto) =
      (nome, (slot, tipo)) : auxiliar (slot + tamanhoTipo tipo) resto

tamanhoTab :: [Var] -> Int
tamanhoTab = sum . map (tamanhoTipo . getTipo)

construirTabFuncoes :: [Funcao] -> TabFuncoes
construirTabFuncoes = map converter
   where 
     converter(nome :->: (params, ret)) = (nome, (map getTipo params, ret))

buscarFuncao :: Id -> [Funcao] -> Funcao
buscarFuncao nome (f@(n :->: _):fs)
  | nome == n = f
  | otherwise = buscarFuncao nome fs
buscarFuncao nome [] = error $ "função '" ++ nome ++ "' não encontrada"

-- ─── Tipos JVM ───────────────────────────────────────────────────────────────

tipoJVM :: Tipo -> String
tipoJVM TInt    = "I"
tipoJVM TDouble = "D"
tipoJVM TString = "Ljava/lang/String;"
tipoJVM TVoid   = "V"

montarAssinatura :: [Tipo] -> Tipo -> String
montarAssinatura params ret = "(" ++ concatMap tipoJVM params ++ ")" ++ tipoJVM ret

-- ─── Cabeçalhos ──────────────────────────────────────────────────────────────

genCab :: String -> Gen String
genCab nome = return (
  ".class public " ++ nome ++
  "\n.super java/lang/Object\n\n" ++
  ".method public <init>()V\n" ++
  "\taload_0\n" ++
  "\tinvokenonvirtual java/lang/Object/<init>()V\n" ++
  "\treturn\n" ++
  ".end method\n\n")

genMainCab :: Int -> Int -> Gen String
genMainCab s l = return (
  ".method public static main([Ljava/lang/String;)V" ++
  "\n\t.limit stack "  ++ show s ++
  "\n\t.limit locals " ++ show l ++ "\n\n")

-- ─── Instruções de load/store/operações ──────────────────────────────────────

genInt :: Int -> String
genInt i
  | i >= -1 && i <= 5          = "\ticonst_" ++ show i ++ "\n"
  | i >= -128 && i <= 127      = "\tbipush "  ++ show i ++ "\n"
  | i >= -32768 && i <= 32767  = "\tsipush "  ++ show i ++ "\n"
  | otherwise                  = "\tldc "     ++ show i ++ "\n"

genDouble :: Double -> String
genDouble d
  | d == 0.0  = "\tdconst_0\n"
  | d == 1.0  = "\tdconst_1\n"
  | otherwise = "\tldc2_w " ++ show d ++ "\n"

genLoad :: Tipo -> Int -> String
genLoad TInt    slot = "\t" ++ varInstr "iload" slot ++ "\n"
genLoad TDouble slot = "\t" ++ varInstr "dload" slot ++ "\n"
genLoad TString slot = "\t" ++ varInstr "aload" slot ++ "\n"
genLoad TVoid   _    = error "não é possível carregar tipo void"

genStore :: Tipo -> Int -> String
genStore TInt    slot = "\t" ++ varInstr "istore" slot ++ "\n"
genStore TDouble slot = "\t" ++ varInstr "dstore" slot ++ "\n"
genStore TString slot = "\t" ++ varInstr "astore" slot ++ "\n"
genStore TVoid   _    = error "não é possível armazenar tipo void"

-- Gera a forma curta (_0.._3) ou genérica de instruções de variável local
varInstr :: String -> Int -> String
varInstr base n
  | n <= 3    = base ++ "_" ++ show n
  | otherwise = base ++ " "  ++ show n

genOp :: Tipo -> String -> String
genOp TInt    op = "\ti" ++ op ++ "\n"
genOp TDouble op = "\td" ++ op ++ "\n"
genOp _       _  = error "operação inválida para esse tipo"

genNeg :: Tipo -> String
genNeg TInt    = "\tineg\n"
genNeg TDouble = "\tdneg\n"
genNeg _       = error "negação inválida para esse tipo"

genReturn :: Tipo -> String
genReturn TInt    = "\tireturn\n"
genReturn TDouble = "\tdreturn\n"
genReturn TString = "\tareturn\n"
genReturn TVoid   = "\treturn\n"

-- ─── Expressões aritméticas ──────────────────────────────────────────────────

genExpr :: String -> TabVars -> Id -> TabFuncoes -> Expr -> Gen (Tipo, String)

genExpr _ _ _ _ (Const (CInt i))    = return (TInt,    genInt i)
genExpr _ _ _ _ (Const (CDouble d)) = return (TDouble, genDouble d)
genExpr _ _ _ _ (Lit s)             = return (TString, "\tldc \"" ++ s ++ "\"\n")

genExpr _ tab _ _ (IdVar nome) =
  case lookup nome tab of
    Just (slot, tipo) -> return (tipo, genLoad tipo slot)
    Nothing           -> error $ "variável '" ++ nome ++ "' não encontrada na tabela"

genExpr c tab fun tabF (Neg e) = do
  (t, e') <- genExpr c tab fun tabF e
  return (t, e' ++ genNeg t)

genExpr c tab fun tabF (Add e1 e2) = genBin c tab fun tabF "add" e1 e2
genExpr c tab fun tabF (Sub e1 e2) = genBin c tab fun tabF "sub" e1 e2
genExpr c tab fun tabF (Mul e1 e2) = genBin c tab fun tabF "mul" e1 e2
genExpr c tab fun tabF (Div e1 e2) = genBin c tab fun tabF "div" e1 e2

genExpr c tab fun tabF (IntDouble e) = do
  (_, e') <- genExpr c tab fun tabF e
  return (TDouble, e' ++ "\ti2d\n")

genExpr c tab fun tabF (DoubleInt e) = do
  (_, e') <- genExpr c tab fun tabF e
  return (TInt, e' ++ "\td2i\n")

genExpr c tab fun tabF (Chamada nome args) = do
  argsCode <- mapM (genExpr c tab fun tabF) args
  let codigo = concatMap snd argsCode
  case lookup nome tabF of
    Nothing -> error $ "função '" ++ nome ++ "' não encontrada"
    Just (params, ret) ->
      return (ret, codigo ++ "\tinvokestatic " ++ c ++ "/" ++ nome
                          ++ montarAssinatura params ret ++ "\n")

-- Auxiliar para operações binárias aritméticas
genBin :: String -> TabVars -> Id -> TabFuncoes -> String -> Expr -> Expr -> Gen (Tipo, String)
genBin c tab fun tabF op e1 e2 = do
  (t1, e1') <- genExpr c tab fun tabF e1
  (_,  e2') <- genExpr c tab fun tabF e2
  return (t1, e1' ++ e2' ++ genOp t1 op)

-- ─── Expressões relacionais (genExprR) ───────────────────────────────────────

-- v: label para onde pular se a condição for VERDADEIRA
-- f: label para onde pular se a condição for FALSA
genExprR :: String -> TabVars -> Id -> TabFuncoes -> String -> String -> ExprR -> Gen String
genExprR c tab fun tabF v f (Req  e1 e2) = genComparacao c tab fun tabF v f e1 e2 "eq"
genExprR c tab fun tabF v f (Rdif e1 e2) = genComparacao c tab fun tabF v f e1 e2 "ne"
genExprR c tab fun tabF v f (Rlt  e1 e2) = genComparacao c tab fun tabF v f e1 e2 "lt"
genExprR c tab fun tabF v f (Rgt  e1 e2) = genComparacao c tab fun tabF v f e1 e2 "gt"
genExprR c tab fun tabF v f (Rle  e1 e2) = genComparacao c tab fun tabF v f e1 e2 "le"
genExprR c tab fun tabF v f (Rge  e1 e2) = genComparacao c tab fun tabF v f e1 e2 "ge"

genComparacao :: String -> TabVars -> Id -> TabFuncoes -> String -> String
              -> Expr -> Expr -> String -> Gen String
genComparacao c tab fun tabF v f e1 e2 op = do
  (t1, e1') <- genExpr c tab fun tabF e1
  (t2, e2') <- genExpr c tab fun tabF e2
  return (e1' ++ e2' ++ genRel t1 t2 v op ++ "\tgoto " ++ f ++ "\n")

-- Para int usa if_icmp<op> direto; para double usa dcmpg + if<op>
genRel :: Tipo -> Tipo -> String -> String -> String
genRel TInt TInt v op = "\tif_icmp" ++ op ++ " " ++ v ++ "\n"
genRel _    _    v op = "\tdcmpg\n\tif" ++ op ++ " " ++ v ++ "\n"

-- ─── Expressões lógicas (genExprL) ───────────────────────────────────────────

genExprL :: String -> TabVars -> Id -> TabFuncoes -> String -> String -> ExprL -> Gen String

genExprL c tab fun tabF v f (And e1 e2) = do
  l1  <- novoLabel
  e1' <- genExprL c tab fun tabF l1 f e1
  e2' <- genExprL c tab fun tabF v  f e2
  return (e1' ++ l1 ++ ":\n" ++ e2')

genExprL c tab fun tabF v f (Or e1 e2) = do
  l1  <- novoLabel
  e1' <- genExprL c tab fun tabF v l1 e1
  e2' <- genExprL c tab fun tabF v f  e2
  return (e1' ++ l1 ++ ":\n" ++ e2')

genExprL c tab fun tabF v f (Not e) = genExprL c tab fun tabF f v e

genExprL c tab fun tabF v f (Rel exprR) = genExprR c tab fun tabF v f exprR

-- ─── Comandos (genCmd) ────────────────────────────────────────────────────────

genCmd :: String -> TabVars -> Id -> TabFuncoes -> Comando -> Gen String

genCmd c tab fun tabF (Atrib nome expr) = do
  (t, expr') <- genExpr c tab fun tabF expr
  case lookup nome tab of
    Just (slot, _) -> return (expr' ++ genStore t slot)
    Nothing        -> error $ "variável '" ++ nome ++ "' não encontrada"

genCmd c tab fun tabF (If cond bt []) = do
  lv <- novoLabel
  lf <- novoLabel
  cond' <- genExprL c tab fun tabF lv lf cond
  bt'   <- genBloco c tab fun tabF bt
  return (cond' ++ lv ++ ":\n" ++ bt' ++ lf ++ ":\n")

genCmd c tab fun tabF (If cond bt bf) = do
  lv   <- novoLabel
  lf   <- novoLabel
  lfim <- novoLabel
  cond' <- genExprL c tab fun tabF lv lf cond
  bt'   <- genBloco c tab fun tabF bt
  bf'   <- genBloco c tab fun tabF bf
  return (cond' ++ lv ++ ":\n" ++ bt' ++ "\tgoto " ++ lfim ++ "\n" ++
          lf ++ ":\n" ++ bf' ++ lfim ++ ":\n")

genCmd c tab fun tabF (While cond bloco) = do
  li <- novoLabel
  lv <- novoLabel
  lf <- novoLabel
  cond' <- genExprL c tab fun tabF lv lf cond
  bloco' <- genBloco c tab fun tabF bloco
  return (li ++ ":\n" ++ cond' ++ lv ++ ":\n" ++ bloco' ++ "\tgoto " ++ li ++ "\n" ++ lf ++ ":\n")

genCmd c tab fun tabF (Imp expr) = do
  (t, expr') <- genExpr c tab fun tabF expr
  return (
    "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n" ++
    expr' ++
    "\tinvokevirtual java/io/PrintStream/println(" ++ tipoJVM t ++ ")V\n")

genCmd c tab fun tabF (Leitura nome) =
  case lookup nome tab of
    Nothing -> error $ "variável '" ++ nome ++ "' não encontrada"
    Just (slot, tipo) -> return (
      "\tnew java/util/Scanner\n" ++
      "\tdup\n" ++
      "\tgetstatic java/lang/System/in Ljava/io/InputStream;\n" ++
      "\tinvokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V\n" ++
      "\t" ++ metodoNext tipo ++ "\n" ++
      genStore tipo slot)

genCmd c tab fun tabF (Ret Nothing) = return "\treturn\n"

genCmd c tab fun tabF (Ret (Just expr)) = do
  (t, expr') <- genExpr c tab fun tabF expr
  return (expr' ++ genReturn t)

genCmd c tab fun tabF (Proc nome args) = do
  (t, codigo) <- genExpr c tab fun tabF (Chamada nome args)
  case t of
    TVoid   -> return codigo
    TDouble -> return (codigo ++ "\tpop2\n")
    _       -> return (codigo ++ "\tpop\n")

metodoNext :: Tipo -> String
metodoNext TInt    = "invokevirtual java/util/Scanner/nextInt()I"
metodoNext TDouble = "invokevirtual java/util/Scanner/nextDouble()D"
metodoNext TString = "invokevirtual java/util/Scanner/next()Ljava/lang/String;"
metodoNext TVoid   = error "não é possível ler tipo void"

-- ─── Bloco ───────────────────────────────────────────────────────────────────

genBloco :: String -> TabVars -> Id -> TabFuncoes -> Bloco -> Gen String
genBloco c tab fun tabF cmds = do
  codigos <- mapM (genCmd c tab fun tabF) cmds
  return (concat codigos)

-- ─── Funções ─────────────────────────────────────────────────────────────────

limitePilhaPadrao :: Int
limitePilhaPadrao = 50

genFuncao :: String -> TabFuncoes -> [Funcao] -> (Id, [Var], Bloco) -> Gen String
genFuncao c tabF funcoes (nome, vars, cmds) = do
  let (_ :->: (params, ret)) = buscarFuncao nome funcoes
  let tab          = construirTabVars 0 vars
  let limiteLocais = tamanhoTab vars
  corpo <- genBloco c tab nome tabF cmds
  return (
    ".method public static " ++ nome ++ montarAssinatura (map getTipo params) ret ++ "\n" ++
    "\t.limit stack "  ++ show limitePilhaPadrao ++ "\n" ++
    "\t.limit locals " ++ show limiteLocais      ++ "\n\n" ++
    corpo ++
    "\n.end method\n\n")

-- ─── Programa ────────────────────────────────────────────────────────────────

genProg :: String -> Programa -> Gen String
genProg nome (Prog funcoes corpos vars bloco) = do
  cab <- genCab nome
  let tabF = construirTabFuncoes funcoes
  funcs <- mapM (genFuncao nome tabF funcoes) corpos
  let tabPrincipal = construirTabVars 1 vars
  let limiteLocais = tamanhoTab vars + 1
  mainCab   <- genMainCab limitePilhaPadrao limiteLocais
  corpoMain <- genBloco nome tabPrincipal "main" tabF bloco
  return (cab ++ concat funcs ++ mainCab ++ corpoMain ++ ".end method\n")

-- ─── Ponto de entrada ─────────────────────────────────────────────────────────

gerar :: String -> Programa -> String
gerar nome p = fst $ runState (genProg nome p) 0
