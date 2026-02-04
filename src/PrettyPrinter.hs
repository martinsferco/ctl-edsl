module PrettyPrinter where

import Lang
import Common
import Prettyprinter
import Prettyprinter.Render.Terminal
import Data.Text (unpack)


keywordColor, typeColor, opColor, quantifierColor , atomColor, varColor, nodeColor, arrowColor, punctuationColor :: Doc AnsiStyle -> Doc AnsiStyle
keywordColor     = annotate (color Green <> bold)
typeColor        = annotate (color Blue <> bold)
opColor          = annotate (color Yellow <> bold)
quantifierColor  = annotate (color Magenta <> bold)
atomColor        = annotate (color Cyan)
nodeColor        = annotate (color Red)
varColor         = annotate (color White <> bold)
arrowColor       = annotate (color Yellow)
punctuationColor = annotate (color White)

------------------------------------------------------------
-- Expressions
------------------------------------------------------------

exprToDoc :: Expr -> Doc AnsiStyle
exprToDoc (FormulaExpr _ f) = sformulaToDoc 0 f
exprToDoc (VarExpr _ v) = varColor (pretty v)
exprToDoc (NodesExpr _ ns) = nodesExprToDoc False ns
exprToDoc (LabelsExpr _ ls) = labelsExprToDoc False ls
exprToDoc (ModelExpr _ n l) = 
    angles $ exprToDocInline n <> punctuationColor comma <+> exprToDocInline l

exprToDocInline :: Expr -> Doc AnsiStyle
exprToDocInline (NodesExpr _ ns) = nodesExprToDoc True ns
exprToDocInline (LabelsExpr _ ls) = labelsExprToDoc True ls
exprToDocInline e = exprToDoc e


nodesExprToDoc :: Bool -> [InfoNode] -> Doc AnsiStyle
nodesExprToDoc inline ns 
  | inline    = braces (hsep $ punctuate (punctuationColor comma) (map nodeLine ns))
  | otherwise = braces $ nest 2 (line <> vsep (map nodeLine ns) <> line)
  where
    nodeLine (n, isInit, neighs) = nodeDoc <+> arrowColor (pretty "=>") <+> neighsDoc
    nodeDoc = if isInit 
              then parens (nodeColor (pretty n)) 
              else nodeColor (pretty n)
    neighsDoc = braces $ hsep $ punctuate (punctuationColor comma) (map (nodeColor . pretty) neighs)



labelsExprToDoc :: Bool -> [Label] -> Doc AnsiStyle
labelsExprToDoc inline ls 
  | inline    = braces (hsep $ punctuate (punctuationColor comma) (map labelLine ls))
  | otherwise = braces $ nest 2 (line <> vsep (map labelLine ls) <> line)
  where
    labelLine (n, atoms) = 
        nodeColor (pretty n) <+> arrowColor (pretty "<=") <+> 
        braces (hsep $ punctuate (punctuationColor comma) (map (atomColor . pretty) atoms))


sformulaToDoc :: Int -> SFormula -> Doc AnsiStyle
sformulaToDoc _ SF = atomColor (pretty "⊥")
sformulaToDoc _ ST = atomColor (pretty "⊤")
sformulaToDoc _ (SAtom a) = atomColor (pretty a)
sformulaToDoc _ (SVar v) = varColor (pretty v)

sformulaToDoc prec (SNot p) = 
    parensIf (prec > 4) $ 
    opColor (pretty "¬") <> sformulaToDoc 4 p

sformulaToDoc prec (SBinaryOp op p q) = 
    parensIf (prec > opPrec) $ 
    sformulaToDoc (opPrec + 1) p <+> binaryOpToDoc op <+> sformulaToDoc (opPrec + 1) q
  where
    opPrec = case op of
        Implies -> 1
        Or      -> 2
        And     -> 3

sformulaToDoc _ (SUQuantifier uq p) = 
    uquantifierToDoc uq <+> sformulaToDoc 5 p

sformulaToDoc _ (SBQuantifier bq p q) = 
    sbquantifierToDoc bq p q

parensIf :: Bool -> Doc AnsiStyle -> Doc AnsiStyle
parensIf True = parens
parensIf False = id



uquantifierToDoc :: UQuantifier -> Doc AnsiStyle
uquantifierToDoc AC = quantifierColor (pretty "A◯")
uquantifierToDoc EC = quantifierColor (pretty "E◯")
uquantifierToDoc AR = quantifierColor (pretty "A◇")
uquantifierToDoc ER = quantifierColor (pretty "E◇")
uquantifierToDoc AS = quantifierColor (pretty "A◻")
uquantifierToDoc ES = quantifierColor (pretty "E◻")

sbquantifierToDoc :: BQuantifier -> SFormula -> SFormula -> Doc AnsiStyle
sbquantifierToDoc q p r = 
    quantifierColor (pretty (case q of AU -> "A"; EU -> "E")) <>
    brackets (sformulaToDoc 0 p <+> quantifierColor (pretty "U") <+> sformulaToDoc 0 r)

binaryOpToDoc :: BinaryOp -> Doc AnsiStyle
binaryOpToDoc And     = opColor (pretty "∧")
binaryOpToDoc Or      = opColor (pretty "∨")
binaryOpToDoc Implies = opColor (pretty "→")


typeToDoc :: Type -> Doc AnsiStyle
typeToDoc ModelTy   = typeColor (pretty "Model")
typeToDoc LabelsTy  = typeColor (pretty "Labels")
typeToDoc NodesTy   = typeColor (pretty "Nodes")
typeToDoc FormulaTy = typeColor (pretty "Formula")

sentenceToDoc :: Sentence -> Doc AnsiStyle
sentenceToDoc (Def _ v t e) = 
    sep [ keywordColor (pretty "define")
        , varColor (pretty v)
        , punctuationColor (pretty "::")
        , typeToDoc t
        , opColor (pretty "=")
        ] <+> exprToDoc e

sentenceToDoc (Export _ m f) = 
    sep [ keywordColor (pretty "export")
        , exprToDoc m
        , keywordColor (pretty "as")
        , pretty f
        ]

sentenceToDoc (IsSatis _ f) = 
    sep [ opColor (pretty "⊨"), exprToDoc f ]

sentenceToDoc (Models _ m f) = 
    sep [ exprToDoc m, opColor (pretty "⊨"), exprToDoc f ]

sentenceToDoc (IsValid _ m n f) = 
    sep [ exprToDoc m
        , punctuationColor comma
        , nodeColor (pretty n)
        , opColor (pretty "⊨")
        , exprToDoc f
        ]


render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

ppSentence :: Sentence -> String
ppSentence = render . sentenceToDoc