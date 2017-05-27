module ProcParser where

import Text.Megaparsec hiding (token)
import Text.Megaparsec.String
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as Lexer
import Prelude hiding (Num)
import Control.Monad (void)


--------------------------------------------------------------------------------
-- Basic Parsing Functions
--------------------------------------------------------------------------------

-- Parses spaces, and C-style comments
-- More information at:
-- https://hackage.haskell.org/package/megaparsec-5.2.0/docs/Text-Megaparsec-Lexer.html
whitespace :: Parser ()
whitespace = Lexer.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = Lexer.skipLineComment "//"
        blockCmnt = Lexer.skipBlockComment "/*" "*/"


-- A wrapper that has the effect of coinsuming whitespace after every lexeme
lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whitespace

-- Takes a string as input, and parses it.
-- Used, for example, to parse `(`, etc
token :: String -> Parser String
token = Lexer.symbol whitespace

-- Parses content between normal parenthesis brackets
-- Used, for exmaple, for unpacking Arethmentic exressions enclosed in brackets
parentheses :: Parser a -> Parser a
parentheses = between (token "(") (token ")")


-- A type alias `Num` to represent Numerals, or an unsigned integer
type Num = Integer

-- A parser that parses a Numeral
num :: Parser Num
num = lexeme Lexer.integer

--------------------------------------------------------------------------------
-- Identifiers
--
-- `identifier`s are used to parse strings that will be used to identify
--  something (Variable and Procedure names)
--
--      identifier ::= (letter Charicter) (alpha numeric char)*
--------------------------------------------------------------------------------



-- Parsers identifiers, failing if the identifier is a reserved word.
-- `try` is used so that the parser will backtrack if it fails (reads a keyword)
identifier :: Parser String
identifier = (lexeme.try) (str >>= check)  where
    str        = (:) <$> (letterChar) <*> many (alphaNumChar)
    check word = if word `elem` ["true",                                        -- The Boolean `True`
                                 "false",                                       -- The Boolean `False`
                                 "var",                                         -- Used for Variable declarations
                                 "skip",                                        -- Used as a "Do Nothing"
                                 "if",                                          -- Part of the `if then else` conditional
                                 "then",                                        -- Part of the `if then else` conditional
                                 "else",                                        -- Part of the `if then else` conditional
                                 "while",                                       -- Denotes the start of a `while` loop
                                 "do",                                          -- Denotes the coming declaration of the inside of a while loop
                                 "begin",                                       -- Declares ther start of a procdure declaration
                                 "end",                                         -- Declares ther start of a procdure declaration
                                 "call"]                                        -- Calls a procedure

                    then fail $ "'" ++ show word ++ "' is a keyword, and cannot be an identifier"
                    else return word



--------------------------------------------------------------------------------
-- Variables and Process Names
--
-- The distinction between the string used to describe a variable and that of a
-- Procedure name is non-existant. The `identifier` could have easily been used
-- for both, however seperating their parsers allows for the code to be more
-- readable
--
--      var  ::= identifier
--      proc ::= identifier
--------------------------------------------------------------------------------

type Var   = String
type Pname = String

var :: Parser Var
var = identifier

pName :: Parser Pname
pName = identifier

--------------------------------------------------------------------------------
-- Arithmetic Expressions:
--
-- Defines the parser for arthimetic expressions
--
--      aexp ::= num
--             | var
--             | aexp '+' aexp
--             | aexp '*' aexp
--             | aexp '-' aexp
--------------------------------------------------------------------------------

-- Defines the Aexp datatype
data Aexp = N    Num
          | V    Var
          | Mult Aexp Aexp
          | Add  Aexp Aexp
          | Sub  Aexp Aexp
          deriving (Eq, Show)

-- The Parser that is used as the `term` in the `makeExprParser` function
aExpTerm :: Parser Aexp
aExpTerm = parentheses aexp
       <|> N <$> num
       <|> V <$> var

-- Used as the operator table by the `makeExprParser` function.
-- Describes the associativitys and presidence of the arthmetic operations.
--
-- For example, in this case:
--      Multiplicaton is of the highest presidence,
--      Addition and Substraction and have a lower, but equal level of
--      presidence
aexpOperatorTable :: [[Operator Parser Aexp]]
aexpOperatorTable = [[InfixL (Mult <$ token "*")],
                     [InfixL (Add <$ token "+"), InfixL (Sub <$ token "-")]]

-- The parser for Arthmetic Expressions
-- More info on `makeExprParser`:
-- `makeExprParser term table` builds an expression parser for the terms `term`
-- with operators from `table`, taking the associativity and precedence
-- specified in table into account.
--      https://hackage.haskell.org/package/megaparsec-5.3.0/docs/Text-Megaparsec-Expr.html
aexp :: Parser Aexp
aexp =  makeExprParser aExpTerm aexpOperatorTable

--------------------------------------------------------------------------------
-- Boolean Expressions:
--      bexp ::= 'true'
--             | 'false'
--             | aexp '=' aexp
--             | aexp '<=' aexp
--             | '!' bexp
--             | bexp '&' bexp
--------------------------------------------------------------------------------

-- Defines the Bexp datatype
data Bexp = TRUE
          | FALSE
          | Neg Bexp
          | And Bexp Bexp
          | Le  Aexp Aexp
          | Eq  Aexp Aexp
          deriving (Eq, Show)

bExprTerm :: Parser Bexp
bExprTerm = try (parentheses bexp)
    <|> TRUE  <$ token "true"
    <|> FALSE <$ token "false"
    <|> try (Le <$> aexp <* token "<=" <*> aexp)
    <|> try (Eq <$> aexp <* token "=" <*> aexp)

bExpOperatorTable :: [[Operator Parser Bexp]]
bExpOperatorTable  = [[Prefix (Neg <$ token "!")],
                      [InfixL (And <$ token "&")]]

                      
-- The parser for Boolean Expressions
-- More info on `makeExprParser`:
-- `makeExprParser term table` builds an expression parser for the terms `term`
-- with operators from `table`, taking the associativity and precedence
-- specified in table into account.
--      https://hackage.haskell.org/package/megaparsec-5.3.0/docs/Text-Megaparsec-Expr.html
bexp :: Parser Bexp
bexp =  makeExprParser bExprTerm bExpOperatorTable

--------------------------------------------------------------------------------
-- Variable Declarations:
--      Dv ::= 'var' var ':=' aexp ';' Dv | epsilon
--------------------------------------------------------------------------------

-- A type alias for a list of Variable declarations
type DecV  = [(Var, Aexp)]

-- A Parser for the DecV datatype
-- Uses `many` to parse a list of (Var, Aexp)
decV :: Parser DecV
decV = many varAExprTuple

-- Used by the `decV` Parser in orderto parse a (Var, Aexp)
varAExprTuple :: Parser (Var, Aexp)
varAExprTuple = do
  token "var"                                                                   -- Consumes the "var" keyword
  variableName <- var                                                           -- Gets the variable name
  token ":="                                                                    -- Consumes the ":=" token
  arthExpr <- aexp                                                              -- Gets the arthmetic expression
  token ";"                                                                     -- Consumes the semi-colon
  return (variableName, arthExpr)                                               -- Returns the (variableName, arhtExpr)  tuple

--------------------------------------------------------------------------------
-- Procedure Declarations:
--      Dp ::= 'proc' proc 'is' Stm ';' Dp | ε
--------------------------------------------------------------------------------

-- A type alias for a list of Procedure Declarations
type DecP  = [(Pname, Stm)]


-- A Parser for the DecV datatype
-- Uses `many` to parse a list of (Pname, Stm)
decP :: Parser DecP
decP = many pNameStmTuple

pNameStmTuple :: Parser (Pname, Stm)
pNameStmTuple = do
  token "proc"
  name <- pName
  token "is"
  s <- stm'
  token ";"
  return (name, s)

--------------------------------------------------------------------------------
-- Statements:
--      Stm ::= 'skip'
--            | var ':=' aexp
--            | Stm ';' Stm
--            | 'if' bexp 'then' Stm 'else' Stm
--            | 'while' bexp 'do' Stm
--            | 'begin' Dv Dp Stm 'end'
--            | 'call' proc
--------------------------------------------------------------------------------

data Stm = Skip
         | Ass    Var Aexp        -- Var `x` := 2
         | Comp   Stm Stm         -- stm1; stm2
         | If     Bexp Stm Stm    -- If `bool` then `stm1` else `stm2`
         | While  Bexp Stm        -- while `bool` do `stm`
         | Block  DecV DecP Stm   -- TODO
         | Call   Pname           -- call `pName`
         deriving (Eq, Show)

-- TODO Document
stm' :: Parser Stm
stm' = try (parentheses stm)                                                    -- `try` is used so that input isnt consumed if the parser fails
   <|> try (Ass <$> var <* token ":=" <*> aexp)                                 -- `try` is used so that input isnt consumed if the parser fails
   <|> Skip  <$ token "skip"
   <|> If    <$ token "if"    <*> bexp <*  token "then" <*> stm <* token "else" <*> stm
   <|> While <$ token "while" <*> bexp <*  token "do"   <*> stm
   <|> Block <$ token "begin" <*> decV   <*> decP    <*> stm <* token "end"
   <|> Call  <$ token "call"  <*> pName

-- An adaption of `Prelude.foldl` that avoids certain stack overflows due to
-- the lazy reduction of GHC. In this version redexes are reduced as soon as
-- are available, rather than when needed.
-- More info:   https://wiki.haskell.org/Foldr_Foldl_Foldl%27
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z []     = z
foldl' f z (x:xs) = let z' = z `f` x
                    in seq z' $ foldl' f z' xs

--
stmOperatorTable :: [[Operator Parser Stm]]
stmOperatorTable = [[InfixR (Comp <$ token ";")]]

-- Parses a statement composition. Recall that a staetment can be two statements
-- seperated by a symbol
-- From hackage.haskell.org:
--      "`sepBy1 p sep` parses one or more occurrences of p, separated by sep.
--       Returns a list of values returned by p"
stmComp :: Parser Stm
stmComp = makeExprParser stm' stmOperatorTable
-- stmComp = do
--    (headStatement:rest) <- sepBy1 stm' (token ";")
--    return $ foldl' Comp headStatement rest

-- TODO Document
stm :: Parser Stm
stm = whitespace *> stmComp
