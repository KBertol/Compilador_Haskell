{
module Parser where

import Token
import AST
import qualified Lex as L
}

%name parsePrograma
%tokentype { Token }
%error { parseError }

%token
  int          { TINT       }
  double       { TDOUBLE    }
  string       { TSTRING    }
  void         { TVOID      }
  if           { IF         }
  else         { ELSE       }
  while        { WHILE      }
  print        { PRINT      }
  return       { RETURN     }
  read         { READ       }
  litInt       { LINT    $$ }
  litDouble    { LDOUBLE $$ }
  litString    { LSTRING $$ }
  id           { ID      $$ }
  '+'          { ADD }
  '-'          { SUB }
  '*'          { MUL }
  '/'          { DIV }
  '<'          { LT' }
  '>'          { GT' }
  '<='         { LE  }
  '>='         { GE  }
  '=='         { EQ' }
  '/='         { NEQ }
  '&&'         { AND }
  '||'         { OR  }
  '!'          { NOT }
  '='          { ASSIGN }
  ';'          { SEMI   }
  ','          { COMMA  }
  '('          { LPAR   }
  ')'          { RPAR   }
  '{'          { LBRACE }
  '}'          { RBRACE }

-- Precedências (menor para maior)
%left '||'
%left '&&'
%nonassoc '!'
%nonassoc '==' '/=' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'
%nonassoc NEG 

%%

Programa : ListaFuncoes BlocoPrincipal
             { Prog (fst (unzip $1)) (snd (unzip $1)) (fst $2) (snd $2) }
         | BlocoPrincipal
             { Prog [] [] (fst $1) (snd $1) }

ListaFuncoes : ListaFuncoes Funcao  { $1 ++ [$2] }
             | Funcao               { [$1] }

Funcao : TipoRet id '(' ParamFormais ')' BlocoPrincipal
           { ($2 :->: ($4, $1), ($2, $4 ++ fst $6, snd $6)) }
       | TipoRet id '(' ')' BlocoPrincipal
           { ($2 :->: ([], $1), ($2, fst $5, snd $5)) }

TipoRet : Tipo  { $1    }
        | void  { TVoid }
        
             
Tipo : int      { TInt }
     | double   { TDouble }
     | string   { TString }

ParamFormais : ParamFormais ',' ParamFormal  { $1 ++ [$3] }
             | ParamFormal                   { [$1] }

ParamFormal : Tipo id  { $2 :#: ($1, 0) }

BlocoPrincipal : '{' Declaracoes ListaComandos '}'  { ($2, $3) }
               | '{' ListaComandos '}'              { ([], $2) }
               | '{' '}'                            { ([], []) }

Declaracoes : Declaracoes Declaracao  { $1 ++ $2 }
            | Declaracao              { $1 }

Declaracao : Tipo ListaId ';'  { map (\i -> i :#: ($1, 0)) $2 }

ListaId : ListaId ',' id  { $1 ++ [$3] }
        | id               { [$1] }
        
Bloco : '{' ListaComandos '}'  { $2 }
      | '{' '}'                { [] }

ListaComandos : ListaComandos Comando  { $1 ++ [$2] }
              | Comando                { [$1] }
              
Comando : if '(' ExprL ')' Bloco else Bloco { If $3 $5 $7 }
        | if '(' ExprL ')' Bloco                     { If $3 $5 [] }
        | while '(' ExprL ')' Bloco                  { While $3 $5 }
        | id '=' Expr ';'                                     { Atrib $1 $3 }
        | print '(' Expr ')' ';'                              { Imp $3 }
        | read '(' id ')' ';'                                 { Leitura $3 }
        | return Expr ';'                                     { Ret (Just $2) }
        | return ';'                                          { Ret Nothing }
        | id '(' ListaExpr ')' ';'                            { Proc $1 $3 }
        | id '(' ')' ';'                                      { Proc $1 [] }

Expr : Expr '+' Expr                    { Add $1 $3 }
     | Expr '-' Expr                    { Sub $1 $3 }
     | Expr '*' Expr                    { Mul $1 $3 }
     | Expr '/' Expr                    { Div $1 $3 }
     | '-' Expr %prec NEG               { Neg $2 }
     | '(' Expr ')'                     { $2 }
     | litInt                           { Const (CInt $1) }
     | litDouble                        { Const (CDouble $1) }
     | litString                        { Lit $1 }            
     | id                               { IdVar $1 }
     | id '(' ListaExpr ')'             { Chamada $1 $3 }
     | id '(' ')'                       { Chamada $1 [] }

ListaExpr : ListaExpr ',' Expr          { $1 ++ [$3] }
          | Expr                        { [$1] }

ExprR : Expr '==' Expr                  { Req $1 $3 }
      | Expr '/=' Expr                  { Rdif $1 $3 }
      | Expr '<' Expr                   { Rlt $1 $3 }
      | Expr '>' Expr                   { Rgt $1 $3 }
      | Expr '<=' Expr                  { Rle $1 $3 }
      | Expr '>=' Expr                  { Rge $1 $3 }

ExprL : ExprL '&&' ExprL                { And $1 $3 }
      | ExprL '||' ExprL                { Or $1 $3 }
      | '!' ExprL                       { Not $2 }
      | ExprR                           { Rel $1 }
      | '(' ExprL ')'                   { $2 }

{
parseError :: [Token] -> a
parseError toks = error $ "Erro sintático: " ++ show toks

main :: IO ()
main = do s <- getContents
          let ast = parsePrograma (L.alexScanTokens s)
          print ast
}
