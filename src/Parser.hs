module Parser ( P, runP, program, sentence ) where

import Common ( Pos (..), VarIdent, AtomIdent, NodeIdent )
import Lang   ( Sentence (..), Program, SFormula (..), Expr (..), Type (..),
                UQuantifier (..), BQuantifier (..), BinaryOp (..) )

import qualified Text.Parsec.Token as Tok
import           Text.Parsec hiding (runP, parse)
import           Text.ParserCombinators.Parsec.Language 

import Data.Char     (isLower, isUpper)
import Control.Monad (guard)

type P = Parsec String ()

--------------------------------------------------------------------------------
-- Lexer definition
--------------------------------------------------------------------------------

lexer :: Tok.TokenParser t
lexer = Tok.makeTokenParser languageDefintion

languageDefintion :: LanguageDef u
languageDefintion = emptyDef
  { commentStart    = "/*"
  , commentEnd      = "*/"
  , commentLine     = "//"

  , identStart      = letter <|> char '_'
  , identLetter     = alphaNum <|> char '_'

  , reservedNames   = ["define", "Model", "Nodes", "Labels",
                       "Formula", "export", "F", "T", "A", "E", "U", "as"]
  , reservedOpNames = ["=", "::", "|=", "=>", "<=", "&&", "||", "!", "[]", "()",
                       "<>", "->", ",", "⊤", "⊥", "∀", "∃", "○", "◇", "□", "⊨",
                       "∧", "∨", "→", "¬"]
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

--------------------------------------------------------------------------------
-- Parsers definitions
--------------------------------------------------------------------------------
getPos :: P Pos
getPos = do pos <- getPosition
            return $ Pos (sourceLine pos)

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

forallQuantifier :: P ()
forallQuantifier = try (reserved "A") <|> reserved "∀"

existsQuantifier :: P ()
existsQuantifier = try (reserved "E") <|> reserved "∃"

bottom :: P ()
bottom = try (reserved "F") <|> reservedOp "⊥"

top :: P ()
top = try (reserved "T") <|> reservedOp "⊤"

circle :: P ()
circle = try (reserved "()") <|> reservedOp "○"

rombus :: P ()
rombus = try (reserved "<>") <|> reservedOp "◇"

square :: P ()
square = try (reserved "[]") <|> reservedOp "□"

modelsFormula :: P ()
modelsFormula = try (reserved "|=") <|> reservedOp "⊨"

andOp :: P ()
andOp = try (reserved "&&") <|> reservedOp "∧"

orOp:: P ()
orOp= try (reserved "||") <|> reservedOp "∨"

implies :: P ()
implies = try (reserved "->") <|> reservedOp "→"

negation :: P ()
negation = try (reserved "!") <|> reservedOp "!"

expr :: P Expr
expr = try modelExpr  <|>
       try labelsExpr <|>
       try nodesExpr  <|>
       try varExpr    <|>
       formulaExpr

labelsExpr :: P Expr 
labelsExpr = LabelsExpr <$> getPos <*> braces (many labelsExprAux)
  where labelsExprAux = do node <- nodeIdent
                           reservedOp "<=" 
                           nodeLabel <- braces $ commaSep atomIdent
                           return (node, nodeLabel)

modelExpr :: P Expr
modelExpr = angles modelExprAux
  where modelExprAux = do pos <- getPos
                          transExpr <- expr
                          reservedOp ","
                          nodeLabelsExpr <- expr
                          return $ ModelExpr pos transExpr nodeLabelsExpr

nodesExpr :: P Expr
nodesExpr = NodesExpr <$> getPos <*> braces (many1 nodesExprAux)
  where nodesExprAux = do  (node, isInitial) <- parseNode
                           reservedOp "=>"
                           neighboors <- braces $ commaSep nodeIdent
                           return (node, isInitial, neighboors) 

        parseNode = try (do node <- parens nodeIdent ; return (node, True)) <|>
                        (do node <- nodeIdent        ; return (node, False))


varExpr :: P Expr 
varExpr = VarExpr <$> getPos <*> varIdent

formulaExpr :: P Expr
formulaExpr = FormulaExpr <$> getPos <*> formulaExpr'
  
formulaExpr' :: P SFormula
formulaExpr' = chainr1 orTerm (implies >> return (SBinaryOp Implies))

orTerm :: P SFormula
orTerm = chainl1 andTerm (orOp >> return (SBinaryOp Or))

andTerm :: P SFormula
andTerm = chainl1 notTerm (andOp >> return (SBinaryOp And))

notTerm :: P SFormula
notTerm = try (negation >> SNot <$> notTerm) <|> quantifierTerm

quantifierTerm :: P SFormula
quantifierTerm = try unaryQuantifier <|> try binaryQuantifier <|> atomicTerm

unaryQuantifier :: P SFormula
unaryQuantifier = 
  try (forallQuantifier >> circle >> SUQuantifier AC <$> notTerm) <|> 
  try (existsQuantifier >> circle >> SUQuantifier EC <$> notTerm) <|> 
  try (forallQuantifier >> rombus >> SUQuantifier AR <$> notTerm) <|> 
  try (existsQuantifier >> rombus >> SUQuantifier ER <$> notTerm) <|> 
  try (forallQuantifier >> square >> SUQuantifier AS <$> notTerm) <|> 
  try (existsQuantifier >> square >> SUQuantifier ES <$> notTerm)     

binaryQuantifier :: P SFormula
binaryQuantifier = 
  try (forallQuantifier >> (uncurry $ SBQuantifier AU) <$> brackets binaryAux) <|>
      (existsQuantifier >> (uncurry $ SBQuantifier EU) <$> brackets binaryAux)
  where 
    binaryAux = do  
      leftFormula <- formulaExpr'  
      reserved "U"
      rightFormula <- formulaExpr'
      return (leftFormula, rightFormula)

atomicTerm :: P SFormula 
atomicTerm = 
  try (parens formulaExpr') <|> 
  try (top >> return ST)    <|>
  try (bottom >> return SF) <|>
  try (SVar <$> varIdent)   <|>
      (SAtom <$> atomIdent)


sentence :: P Sentence
sentence = try defSentence     <|> 
           try exportSentence  <|>
           try modelsSentence  <|>
           try isValidSentence <|>
           isSatisSentence 

defSentence :: P Sentence
defSentence = do  pos <- getPos
                  reserved "define"
                  var <- varIdent
                  reservedOp "::"
                  varType <- typeParser
                  reservedOp "="
                  definitionExpr <- expr
                  return $ Def pos var varType definitionExpr

exportSentence :: P Sentence 
exportSentence = do pos <- getPos
                    reserved "export" 
                    exportedModel <- expr
                    reserved "as"
                    fileName <- identifier
                    return $ Export pos exportedModel fileName

modelsSentence :: P Sentence 
modelsSentence = do pos <- getPos
                    modelE <- expr
                    modelsFormula
                    formulaE <- expr
                    return $ Models pos modelE formulaE

isValidSentence :: P Sentence
isValidSentence = do  pos <- getPos
                      modelE <- expr
                      reservedOp ","
                      node <- nodeIdent
                      modelsFormula
                      formulaE <- expr
                      return $ IsValid pos modelE node formulaE
 
isSatisSentence :: P Sentence
isSatisSentence = do  pos <- getPos
                      modelsFormula
                      form <- expr
                      reserved "as"
                      fileName <- identifier
                      return $ IsSatis pos form fileName

program :: P Program
program = many sentence

runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s