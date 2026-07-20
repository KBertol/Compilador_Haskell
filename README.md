# Compilador em Haskell (Alex + Happy + JVM Bytecode)

Compilador multi-etapas para uma linguagem imperativa simples, escrito
em Haskell, com análise léxica e sintática geradas por **Alex** e
**Happy**, análise semântica com um monad de erros próprio e geração
de bytecode para a **JVM** via **Jasmin**.

Trabalho da disciplina de **Compiladores**.

## Pipeline

O compilador processa o código-fonte em 4 etapas:

```
[1/4] Análise Léxica    -> Lex.x   (Alex)   -> tokens
[2/4] Análise Sintática -> Parser.y (Happy) -> AST
[3/4] Análise Semântica -> Semantico.hs      -> AST anotada / erros
[4/4] Geração de Código -> Codegen.hs        -> bytecode Jasmin (.j) -> .class -> execução
```

Se a análise semântica encontrar erros, a geração de código é
cancelada e as mensagens são reportadas; caso contrário, o compilador
monta o `.class` com o Jasmin e executa o programa automaticamente.

## A linguagem

Linguagem imperativa com tipagem estática, definida em `AST.hs`:

- **Tipos**: `int`, `double`, `string`, `void`
- **Expressões**: aritméticas (`+ - * /`), relacionais
  (`< > <= >= == /=`), lógicas (`&& || !`)
- **Comandos**: atribuição, `if`/`else`, `while`, `read`, `print`,
  `return`, chamada de função/procedimento
- **Funções**: declaração com parâmetros e tipo de retorno,
  verificadas na análise semântica

## Análise semântica

A análise semântica é construída sobre um monad `Result` próprio, que
acumula erros e advertências junto com a AST resultante — sem
depender de bibliotecas externas de tratamento de erro:

```haskell
data Result a = Result (Bool, String, a)  -- (temErro, mensagens, valor)
```

Verifica, entre outras coisas: funções e variáveis declaradas mais de
uma vez, uso de identificadores não declarados, compatibilidade de
tipos em atribuições e chamadas, e coerções implícitas entre `int` e
`double`.

## Geração de código

`Codegen.hs` percorre a AST anotada e emite instruções Jasmin
(assembly para a JVM), com tabela de variáveis locais e geração de
labels para estruturas de controle. O arquivo `.j` gerado é montado
para bytecode `.class` pelo [Jasmin](https://jasmin.sourceforge.net/)
(incluído em `jasmin-2.4/`) e executado com `java`.

## Como executar

### Pré-requisitos
- GHC + `alex` e `happy` (ou usar os `.hs` já gerados a partir de
  `Lex.x`/`Parser.y`, incluídos no repositório)
- JDK (`java`, `javac`) para rodar o Jasmin e o `.class` gerado

### Compilar o compilador

```bash
ghc -o compiler Main.hs Lex.hs Parser.hs Token.hs Semantico.hs Codegen.hs
```

### Rodar

```bash
./compiler programa.txt      # compila e executa o arquivo
./compiler < programa.txt    # ou via stdin
```

O compilador imprime o progresso de cada etapa, gera `teste.j`,
monta `teste.class` com o Jasmin e executa o programa.

## Estrutura do projeto

```
├── Token.hs        -> definição dos tokens
├── Lex.x            -> especificação léxica (Alex)
├── Parser.y          -> gramática e construção da AST (Happy)
├── AST.hs            -> tipos da árvore sintática abstrata
├── Semantico.hs       -> análise semântica (monad Result)
├── Codegen.hs         -> geração de bytecode Jasmin
├── Main.hs            -> pipeline principal
└── jasmin-2.4/         -> montador Jasmin (biblioteca de terceiros)
```

## Tecnologias

- Haskell (GHC)
- Alex (análise léxica) e Happy (análise sintática)
- Jasmin (montagem de bytecode JVM)

## Autoria

Karla Bertol
