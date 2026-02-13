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
                       "^", "∨", "→", "¬"]
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
typeParser = (reserved "Model"   >> return ModelTy)   <|>
             (reserved "Labels"  >> return LabelsTy)  <|>
             (reserved "Nodes"   >> return NodesTy)   <|>
             (reserved "Formula" >> return FormulaTy) 

forallQuantifier :: P ()
forallQuantifier = reserved "A" <|> reservedOp "∀"

existsQuantifier :: P ()
existsQuantifier = reserved "E" <|> reservedOp "∃"

bottom :: P ()
bottom = reserved "F" <|> reservedOp "⊥"

top :: P ()
top = reserved "T" <|> reservedOp "⊤"

circle :: P ()
circle = reservedOp "()" <|> reservedOp "○"

rombus :: P ()
rombus = reservedOp "<>" <|> reservedOp "◇"

square :: P ()
square = reservedOp "[]" <|> reservedOp "□"

modelsFormula :: P ()
modelsFormula = reservedOp "|=" <|> reservedOp "⊨"

andOp :: P ()
andOp = reservedOp "&&" <|> reservedOp "^"

orOp:: P ()
orOp= reservedOp "||" <|> reservedOp "∨"

implies :: P ()
implies = reservedOp "->" <|> reservedOp "→"

negation :: P ()
negation = reservedOp "!" <|> reservedOp "¬"

expr :: P Expr
expr = modelExpr        <|>
       try labelsExpr   <|>
       nodesExpr        <|>
       formulaOrVarExpr

formulaOrVarExpr :: P Expr
formulaOrVarExpr = do
  p <- getPos
  form <- formulaExpr
  return $ case form of
    SVar var -> VarExpr p var
    _        -> FormulaExpr p form       

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


formulaExpr :: P SFormula
formulaExpr = chainr1 orTerm (implies >> return (SBinaryOp Implies))

orTerm :: P SFormula
orTerm = chainl1 andTerm (orOp >> return (SBinaryOp Or))

andTerm :: P SFormula
andTerm = chainl1 notTerm (andOp >> return (SBinaryOp And))

notTerm :: P SFormula
notTerm = (negation >> SNot <$> notTerm) <|> quantifierTerm

quantifierTerm :: P SFormula
quantifierTerm = unaryQuantifier <|> binaryQuantifier <|> atomicTerm

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
  try (existsQuantifier >> (uncurry $ SBQuantifier EU) <$> brackets binaryAux)
  where 
    binaryAux = do  
      leftFormula <- formulaExpr  
      reserved "U"
      rightFormula <- formulaExpr
      return (leftFormula, rightFormula)

atomicTerm :: P SFormula 
atomicTerm =  (parens formulaExpr)  <|> 
              (top >> return ST)    <|>
              (bottom >> return SF) <|>
              (SVar <$> varIdent)   <|>
              (SAtom <$> atomIdent)


sentence :: P Sentence
sentence = defSentence         <|> 
           exportSentence      <|>
           try modelsSentence  <|>
           isValidSentence     <|>
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