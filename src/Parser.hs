module Parser where

import AST 

import Text.Parsec hiding (runP, parse)

import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language 
import Control.Monad (guard)
import Data.Char (isLower, isUpper)

type P = Parsec String ()

-----------------------
-- Lexer definition
-----------------------

lexer :: Tok.TokenParser t
lexer = Tok.makeTokenParser languageDefintion

languageDefintion :: LanguageDef u
languageDefintion = emptyDef
  { commentStart    = "/*"
  , commentEnd      = "*/"
  , commentLine     = "//"

  , reservedNames   = ["define", "Model", "Transitions", "Labels",
                       "Formula", "export", "isValid", "F", "T", "A", "E", "U", "o"]
  , reservedOpNames = ["=", "::", "|=", "=>", "<=", "&&", "||", "!", "[]", "<>", "->", ","]
  }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

parens :: P a -> P a
parens = Tok.parens lexer

braces :: P a -> P a
braces = Tok.braces lexer

angles :: P a -> P a
angles = Tok.angles lexer

brackets :: P a -> P a
brackets = Tok.brackets lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

commaSep :: P a -> P [a]
commaSep = Tok.commaSep lexer

-----------------------
-- Parsers definitions
-----------------------
varIdent :: P VarIdent 
varIdent = try $ do
  ident@(c:_) <- identifier
  guard (isUpper c)
  return ident

atomIdent :: P AtomIdent 
atomIdent = try $ do 
  ident@(c:_) <- identifier
  guard (isLower c)
  return ident

nodeIdent :: P NodeIdent 
nodeIdent = try $ do
  ident@(c:_) <- identifier
  guard (c == '_')
  return ident



typeParser :: P Type
typeParser = try (reserved "Model"       >> return Model)       <|>
             try (reserved "Labels"      >> return Labels)      <|>
             try (reserved "Transitions" >> return Transitions) <|>
             try (reserved "Formula"     >> return Formula) 



expr :: P Expr
expr = try labelsExpr     <|>
       try modelExpr       <|>
       try transitionsExpr <|>
       formulaExpr

labelsExpr :: P Expr 
labelsExpr = LabelDef <$> braces (many labelsExprAux)
  where labelsExprAux = do node <- nodeIdent
                           reservedOp "<=" 
                           label <- braces $ commaSep atomIdent
                           return (node, label)

modelExpr :: P Expr
modelExpr = angles modelExprAux
  where modelExprAux = do transExpr <- expr
                          reservedOp ","
                          labelExpr <- expr
                          return $ ModelDef transExpr labelExpr

transitionsExpr :: P Expr
transitionsExpr = TransDef <$> braces (many transitionsExprAux)
  where transitionsExprAux = do (node, isInitial) <- parseNode
                                reservedOp "=>"
                                neighboors <- braces $ commaSep nodeIdent
                                return (node, isInitial, neighboors) 

        parseNode = try (do node <- parens nodeIdent ; return (node, True)) <|>
                        (do node <- nodeIdent        ; return (node, False))

formulaExpr :: P Expr
formulaExpr = chainr1 impliesTerm (reservedOp "->" >> return (BinaryOp Implies))

impliesTerm :: P Expr
impliesTerm = chainl1 orTerm (reservedOp "||" >> return (BinaryOp Or))

orTerm :: P Expr
orTerm = chainl1 andTerm (reservedOp "&&" >> return (BinaryOp And))

andTerm :: P Expr
andTerm = try unaryQuantifier <|> binaryQuantifier

unaryQuantifier :: P Expr
unaryQuantifier = try (reserved "A" >> reserved   "o"  >> UQuantifier AC <$> unaryQuantifier) <|>
                  try (reserved "E" >> reserved   "o"  >> UQuantifier EC <$> unaryQuantifier) <|>
                  try (reserved "A" >> reservedOp "<>" >> UQuantifier AR <$> unaryQuantifier) <|>
                  try (reserved "E" >> reservedOp "<>" >> UQuantifier ER <$> unaryQuantifier) <|>
                  try (reserved "A" >> reservedOp "[]" >> UQuantifier AS <$> unaryQuantifier) <|>
                  try (reserved "E" >> reservedOp "[]" >> UQuantifier ES <$> unaryQuantifier) <|>
                      quantifierTerm

binaryQuantifier :: P Expr
binaryQuantifier = try  (reserved "A" >> (uncurry $ BQuantifier AU) <$> brackets binaryAux) <|>
                        (reserved "E" >> (uncurry $ BQuantifier AU) <$> brackets binaryAux)

  where binaryAux = do  leftFormula <- formulaExpr
                        reserved "U"
                        rightFormula <- formulaExpr
                        return (leftFormula, rightFormula)

quantifierTerm :: P Expr
quantifierTerm = try (reservedOp "!" >> Not <$> quantifierTerm) <|>
                 atomicTerm

atomicTerm :: P Expr 
atomicTerm =  try (parens formulaExpr)       <|>
              try (reserved "T" >> return T) <|>
              try (reserved "F" >> return F) <|>
              try (Var <$> varIdent)         <|>
                  (Atom <$> atomIdent)     



sentence :: P Sentence
sentence = try defSentence    <|> 
           try exportSentence <|>
           try modelsSentence <|>
           isValidSentence 

defSentence :: P Sentence
defSentence = do reserved "define"
                 var <- varIdent
                 reservedOp "::"
                 varType <- typeParser
                 reservedOp "="
                 varExpr <- expr
                 return $ Def var varType varExpr 

exportSentence :: P Sentence 
exportSentence = reserved "export" >> (Export <$> expr)

modelsSentence :: P Sentence 
modelsSentence = do model <- expr
                    reservedOp "|="
                    formula <- expr
                    return $ Models model formula

isValidSentence :: P Sentence
isValidSentence = reserved "isValid" >> (IsValid <$> expr)



---------------------------------------
-- General parsers
---------------------------------------
program :: P Program
program = many sentence

runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

parseExpr :: String -> Expr
parseExpr s = case runP expr s "" of 
                Right t -> t
                Left e -> error ("no parse: " ++ show s)

parseSentence :: String -> Sentence
parseSentence s = case runP sentence s "" of
                    Right t -> t
                    Left e -> error ("no parse: " ++ show s)

parseProgram :: String -> Program
parseProgram s = case runP program s "" of
                    Right t -> t
                    Left e -> error ("no parse: " ++ show s)

