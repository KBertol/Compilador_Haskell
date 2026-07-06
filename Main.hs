module Main where

import Lex (alexScanTokens)
import Parser (parsePrograma)
import Semantico (analisarM, Result(..))
import Codegen (gerar)
import System.Environment (getArgs)
import System.Process (callCommand)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            conteudo <- readFile file
            executar conteudo
        [] -> do
            conteudo <- getContents
            executar conteudo
        _ -> putStrLn "Uso: ./compiler [arquivo] ou ./compiler < arquivo"

executar :: String -> IO ()
executar input = do
    putStrLn "\n[1/4] Analise Lexica (Tokens):"
    let tokens = alexScanTokens input
    putStrLn "Tokens gerados com sucesso."

    putStrLn "\n[2/4] Analise Sintatica (AST):"
    let ast = parsePrograma tokens
    putStrLn "AST construida com sucesso."

    putStrLn "\n[3/4] Analise Semantica (Tipos e Coercoes):"
    let Result (temErro, msgs, ast') = analisarM ast
    if null msgs
       then putStrLn "Nenhum aviso ou erro encontrado."
       else putStr msgs
    if temErro
       then putStrLn "\n[ERRO] Analise Semantica concluida com erros!"
       else putStrLn "\n[OK] Analise Semantica concluida com sucesso!"

    if temErro
       then putStrLn "\nGeracao de codigo cancelada devido a erros semanticos."
       else do
         let jasmin = gerar "teste" ast'
         writeFile "teste.j" jasmin
         putStrLn "\nArquivo teste.j gerado com sucesso."
         putStrLn "\n[4/4] Montando bytecode com Jasmin..."
         callCommand "java -jar jasmin-2.4/jasmin.jar teste.j"
         putStrLn "[OK] Arquivo teste.class gerado."
         putStrLn "\nExecutando programa..."
         putStrLn "----------------------------"
         callCommand "java teste"

formatarMensagens :: String -> String
formatarMensagens msgs = unlines $ map formatarLinha (lines msgs)
  where
    formatarLinha linha = linha

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _          = True
isPrefixOf _  []         = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
