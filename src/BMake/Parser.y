{
{-# LANGUAGE OverloadedStrings #-}
module BMake.Parser where

import BMake.Base
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified Data.DList as DList
import Data.Either
import           Data.DList (DList)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)

}

%name happyParser
%tokentype { Token }

%monad { Parser } { thenP } { returnP }
%lexer { lexer } { Token _ TokenEOF }

%token
        include         { Token _ TokenInclude         }
        local           { Token _ TokenLocal           }
        ifeq            { Token _ TokenIfEq            }
        ifneq           { Token _ TokenIfNEq           }
        else            { Token _ TokenElse            }
        endif           { Token _ TokenEndif           }

        OTHER           { Token _ (TokenOther $$)      }
        SPACES          { Token _ (TokenWhitespace $$) }

        "="             { Token _ TokenEqual           }
        "?="            { Token _ TokenEqualMaybe      }
        ":"             { Token _ TokenColon           }
        "|"             { Token _ TokenPipe            }
        "("             { Token _ TokenParenOpen       }
        ")"             { Token _ TokenParenClose      }
        "{"             { Token _ TokenCurlyOpen       }
        "}"             { Token _ TokenCurlyClose      }
        ","             { Token _ TokenComma           }
        "%"             { Token _ TokenPercent         }
        "*"             { Token _ TokenAsterik         }
        "$"             { Token _ TokenDollar          }
        DC              { Token _ (TokenDollarChar $$) }
        TAB             { Token _ TokenNewLineAndTab   }
        NEWLINE         { Token _ TokenNewLine         }

%%

-- Regular types

Root  :: {Makefile}
       : StatementsDList  { Makefile (DList.toList $1) }

StatementsDList :: {DList Statement}
      : StatementsDList Statement       { case $2 of { Just x -> $1 `DList.snoc` x ; Nothing -> $1 } }
      | StatementsDList NEWLINE         { $1 }
      | Statement                       { maybe DList.empty DList.singleton $1 }
      |                                 { DList.empty }

-- Maybe whitespace
MW    :: {DList ByteString}
      : SPACES { DList.singleton $1 }
      |        { DList.empty }

Statement :: {Maybe Statement}
      : local MW "{" StatementsDList local MW "}" { Just $ Local (DList.toList $4) }
      | OTHER MW "=" MW TgtExprListE   { Just $ Assign $1 AssignNormal $5 }
      | OTHER MW "?=" MW TgtExprListE  { Just $ Assign $1 AssignConditional $5 }
      | ExprList MW ":" MW TgtExprListInputE TgtScriptE
                                       { let regularInputs = $5
                                          in Just $ Target $1 regularInputs [] $6 }
      | ExprList MW ":" MW TgtExprListInputE "|" TgtExprListE TgtScriptE
                                       { let regularInputs = $5
                                          in Just $ Target $1 regularInputs $7 $8 }
      | include MW OTHER               { Just $ Include $3 }
      | SPACES                         { Nothing }
      | ifeq IfStmt                    { Just $ ($2) $ IfCmp IfEquals }
      | ifneq IfStmt                   { Just $ ($2) $ IfCmp IfNotEquals }

TgtScriptE :: {[[Expr]]}
      :                                { [] }
      | TAB TgtScript                  { {-TgtScript-}DList.toList $2 }

TgtScript :: {DList [Expr]}
      : TgtScript TAB TgtExprListE    { $1 `DList.snoc` $3 }
      | TgtExprListE                  { DList.singleton $1 }

IfStmt -- TODO: Is this it? :: { [Expr] -> [Expr] -> [Statement] -> [Statement] -> Statement }
      : MW "(" NoCommaExprListE "," NoCommaExprListE ")" NEWLINE StatementsDList else StatementsDList endif
                                      { \x -> x $3 $5 (DList.toList $8) (DList.toList $10) }
      | MW "(" NoCommaExprListE "," NoCommaExprListE ")" NEWLINE StatementsDList endif
                                      { \x -> x $3 $5 (DList.toList $8) [] }

ExprListE :: {[Expr]}
      :                               { [] }
      | ExprList                      { $1 }

ExprList :: {[Expr]}
      : ExprDList                     { {-ExprList-}DList.toList $1 }

ExprDList :: {DList Expr}
      : ExprDList MW Expr          { ($1 `DList.snoc` Spaces) `DList.snoc` $3 }
      | ExprDList Expr             { $1 `DList.snoc` $2 }
      | Expr                       { DList.singleton $1 }

Expr :: {Expr}
      : OTHER                         { Str $1 }
      | DC                            { parseDCToken $1 }
      | "$" "{" OTHER "}"             { VarSimple $3 }
      | "{"                           { OpenBrace }
      | "}"                           { CloseBrace }
      | SPACES                        { Spaces }
      | ","                           { Comma }
      | "|"                           { Str "|" }
      | "%"                           { Str "%" }
      | "*"                           { Str "*" }
      | "$"                           { Str "$" }


NoCommaExprListE :: {[Expr]}
      :                                          { [] }
      | NoCommaExprList                          { $1 }

NoCommaExprList :: {[Expr]}
      : NoCommaExprDList                         { {-ExprList-}DList.toList $1 }

NoCommaExprDList :: {DList Expr}
      : NoCommaExprDList MW NoCommaExpr          { ($1 `DList.snoc` Spaces) `DList.snoc` $3 }
      | NoCommaExprDList NoCommaExpr             { $1 `DList.snoc` $2 }
      | NoCommaExpr                              { DList.singleton $1 }

NoCommaExpr :: {Expr}
      : OTHER                         { Str $1 }
      | DC                            { parseDCToken $1 }
      | "$" "{" OTHER "}"             { VarSimple $3 }
      | "{"                           { OpenBrace }
      | "}"                           { CloseBrace }
      | SPACES                        { Spaces }
      | "|"                           { Str "|" }
      | "%"                           { Str "%" }
      | "*"                           { Str "*" }
      | "$"                           { Str "$" }

-- "TgtExpr" is a simple string as expr

-- TODO: Check if base case of 1 is simpler than 2 rules?
TgtExprListE :: {[Expr]}
      :                               { [] }
      | TgtExprList                   { {-TgtExprListE-}DList.toList $1 }

TgtExprList :: {DList Expr}
      : TgtExprList TgtExpr        { $1 `DList.snoc` $2 }
      | TgtExpr                    { DList.singleton $1 }

TgtExpr :: {Expr}
      : OTHER                         { Str $1 }
      | SPACES                        { Spaces }
      | DC                            { parseDCToken $1 }
      | "$" "{" OTHER "}"             { VarSimple $3 }
      | "$"                           { Str "$" }
      | "{"                           { OpenBrace }
      | "}"                           { CloseBrace }
      | else                          { Str "else" }
      | ifeq                          { Str "ifeq" }
      | ifneq                         { Str "ifneq" }
      | include                       { Str "include" }
      | local                         { Str "local" }
      | endif                         { Str "endif" }
      | "="                           { Str "=" }
      | ","                           { Comma }
      | "?="                          { Str "?="}
      | "|"                           { Str "|" }
      | ":"                           { Str ":" }
      | "%"                           { Str "%" }
      | "*"                           { Str "*" }
      | "("                           { Str "(" }
      | ")"                           { Str ")" }

TgtExprListInputE :: {[Expr]}
      :                               { [] }
      | TgtExprListInput              { DList.toList $1 }

TgtExprListInput :: {DList (Expr)}
      : TgtExprListInput TgtInputExpr { $1 `DList.snoc` ($2) }
      | TgtInputExpr                  { DList.singleton ($1) }

TgtInputExpr :: {Expr}
      : OTHER                         { Str $1 }
      | SPACES                        { Spaces }
      | DC                            { parseDCToken $1 }
      | "$" "{" OTHER "}"             { VarSimple $3 }
      | "$"                           { Str "$" }
      | "{"                           { OpenBrace }
      | "}"                           { CloseBrace }
      | else                          { Str "else" }
      | ifeq                          { Str "ifeq" }
      | ifneq                         { Str "ifneq" }
      | include                       { Str "include" }
      | local                         { Str "local" }
      | endif                         { Str "endif" }
      | "="                           { Str "=" }
      | ","                           { Comma }
      | "?="                          { Str "?="}
      | ":"                           { Str ":" }
      | "%"                           { Str "%" }
      | "*"                           { Str "*" }
      | "("                           { Str "(" }
      | ")"                           { Str ")" }
