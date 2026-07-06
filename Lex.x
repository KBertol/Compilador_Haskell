{
module Lex where
import Token
}

%wrapper "basic"

$digit  = [0-9]
$letter = [A-Za-z]

@id     = $letter ($letter | $digit | _)*
@int    = $digit+
@double = $digit+ \. $digit+
@string = \" ([^\"\n\\]| \\ .)* \"

tokens :-

  $white+        ;
  "//" .*        ;

-- Tipos e palavras reservadas
  "int"          { \s -> TINT }
  "double"       { \s -> TDOUBLE }
  "string"       { \s -> TSTRING }
  "void"         { \s -> TVOID }
  "if"           { \s -> IF }
  "else"         { \s -> ELSE }
  "while"        { \s -> WHILE }
  "print"        { \s -> PRINT }
  "return"       { \s -> RETURN }
  "read"         { \s -> READ }

-- Literais
  @double        { \s -> LDOUBLE (read s) }
  @int           { \s -> LINT    (read s) }
  @string        { \s -> LSTRING (init (tail s)) }

-- Identificador
  @id            { \s -> ID s }

-- Operadores relacionais 
  "<="           { \s -> LE  }
  ">="           { \s -> GE  }
  "=="           { \s -> EQ' }
  "/="           { \s -> NEQ }
  "<"            { \s -> LT' }
  ">"            { \s -> GT' }

-- Operadores lógicos
  "&&"           { \s -> AND }
  "||"           { \s -> OR  }
  "!"            { \s -> NOT }

-- Operadores aritméticos
  "+"            { \s -> ADD }
  "-"            { \s -> SUB }
  "*"            { \s -> MUL }
  "/"            { \s -> DIV }

-- Pontuação e atribuição
  "="            { \s -> ASSIGN }
  ";"            { \s -> SEMI   }
  ","            { \s -> COMMA  }
  "("            { \s -> LPAR   }
  ")"            { \s -> RPAR   }
  "{"            { \s -> LBRACE }
  "}"            { \s -> RBRACE }

{
testLex :: IO ()
testLex = do s <- getContents
             print (alexScanTokens s)
}
