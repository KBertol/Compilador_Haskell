module Token where

data Token
  = TINT | TDOUBLE | TSTRING | TVOID
  | IF | ELSE | WHILE | PRINT | RETURN | READ
  | LINT Int | LDOUBLE Double | LSTRING String
  | ID String
  | ADD | SUB | MUL | DIV
  | LT' | GT' | LE | GE | EQ' | NEQ
  | AND | OR | NOT
  | ASSIGN | SEMI | COMMA
  | LPAR | RPAR | LBRACE | RBRACE
  deriving (Eq, Show)
