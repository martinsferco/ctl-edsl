module Parser where

import AST 
import Common

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

  , reservedNames   = ["define", "Model", "Nodes", "Labels",
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
typeParser = try (reserved "Model"       >> return ModelTy)       <|>
             try (reserved "Labels"      >> return LabelsTy)      <|>
             try (reserved "Nodes" >> return NodesTy) <|>
             try (reserved "Formula"     >> return FormulaTy) 



expr :: P Expr
expr = try labelsExpr <|>
       try modelExpr  <|>
       try nodesExpr  <|>
       try varExpr    <|>
       formulaExpr

labelsExpr :: P Expr 
labelsExpr = LabelsExpr <$> braces (many labelsExprAux)
  where labelsExprAux = do node <- nodeIdent
                           reservedOp "<=" 
                           label <- braces $ commaSep atomIdent
                           return (node, label)

modelExpr :: P Expr
modelExpr = angles modelExprAux
  where modelExprAux = do transExpr <- expr
                          reservedOp ","
                          labelsExpr <- expr
                          return $ ModelExpr transExpr labelsExpr

nodesExpr :: P Expr
nodesExpr = NodesExpr <$> braces (many nodesExprAux)
  where nodesExprAux = do  (node, isInitial) <- parseNode
                           reservedOp "=>"
                           neighboors <- braces $ commaSep nodeIdent
                           return (node, isInitial, neighboors) 

        parseNode = try (do node <- parens nodeIdent ; return (node, True)) <|>
                        (do node <- nodeIdent        ; return (node, False))


varExpr :: P Expr 
varExpr = VarExpr <$> varIdent

formulaExpr :: P Expr
formulaExpr = FormulaExpr <$> formulaExpr'
  
formulaExpr' :: P SFormula
formulaExpr' = chainr1 impliesTerm (reservedOp "->" >> return (SBinaryOp Implies))

impliesTerm :: P SFormula
impliesTerm = chainl1 orTerm (reservedOp "||" >> return (SBinaryOp Or))

orTerm :: P SFormula
orTerm = chainl1 andTerm (reservedOp "&&" >> return (SBinaryOp And))

andTerm :: P SFormula
andTerm = try unaryQuantifier <|> binaryQuantifier

unaryQuantifier :: P SFormula
unaryQuantifier = try (reserved "A" >> reserved   "o"  >> SUQuantifier AC <$> unaryQuantifier) <|>
                  try (reserved "E" >> reserved   "o"  >> SUQuantifier EC <$> unaryQuantifier) <|>
                  try (reserved "A" >> reservedOp "<>" >> SUQuantifier AR <$> unaryQuantifier) <|>
                  try (reserved "E" >> reservedOp "<>" >> SUQuantifier ER <$> unaryQuantifier) <|>
                  try (reserved "A" >> reservedOp "[]" >> SUQuantifier AS <$> unaryQuantifier) <|>
                  try (reserved "E" >> reservedOp "[]" >> SUQuantifier ES <$> unaryQuantifier) <|>
                      quantifierTerm

binaryQuantifier :: P SFormula
binaryQuantifier = try  (reserved "A" >> (uncurry $ SBQuantifier AU) <$> brackets binaryAux) <|>
                        (reserved "E" >> (uncurry $ SBQuantifier AU) <$> brackets binaryAux)

  where binaryAux = do  leftFormula <- formulaExpr'
                        reserved "U"
                        rightFormula <- formulaExpr'
                        return (leftFormula, rightFormula)

quantifierTerm :: P SFormula
quantifierTerm = try (reservedOp "!" >> SNot <$> quantifierTerm) <|>
                 atomicTerm

atomicTerm :: P SFormula 
atomicTerm =  try (parens formulaExpr')       <|>
              try (reserved "T" >> return ST) <|>
              try (reserved "F" >> return SF) <|>
              try (SVar <$> varIdent)         <|>
                  (SAtom <$> atomIdent)     



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

