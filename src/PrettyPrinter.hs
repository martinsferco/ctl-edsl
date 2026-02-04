module PrettyPrinter where

import Lang
import Common

import Prettyprinter
import Data.Text ( unpack )
import Prettyprinter.Render.Terminal


------------------------------------------------------------
-- Colors
------------------------------------------------------------

typeOpColor :: Doc AnsiStyle -> Doc AnsiStyle
typeOpColor = annotate (colorDull Blue)

opColor :: Doc AnsiStyle -> Doc AnsiStyle
opColor = annotate (colorDull Blue)

quantifierColor :: Doc AnsiStyle -> Doc AnsiStyle
quantifierColor = annotate (colorDull Blue)

atomColor :: Doc AnsiStyle -> Doc AnsiStyle
atomColor = annotate (colorDull Blue)

varColor :: Doc AnsiStyle -> Doc AnsiStyle
varColor = annotate (colorDull Blue)

nodeColor :: Doc AnsiStyle -> Doc AnsiStyle
nodeColor = annotate (colorDull Blue)

keywordColor :: Doc AnsiStyle -> Doc AnsiStyle
keywordColor = annotate (colorDull Green)

------------------------------------------------------------
-- Expressions
------------------------------------------------------------

exprToDoc :: Expr -> Doc AnsiStyle
exprToDoc (FormulaExpr _ sformula)   = sformulaToDoc sformula
exprToDoc (ModelExpr _ nodes labels) =
  angles $
    nest 2 $
      line <>
      vsep [ exprToDoc nodes <> comma
           , exprToDoc labels
           ] <> line
exprToDoc (LabelsExpr _ labels)      = labelsExprToDoc False labels
exprToDoc (NodesExpr _ infoNodes)    = nodesExprToDoc False infoNodes
exprToDoc (VarExpr _ var)            = varColor (pretty var)

------------------------------------------------------------
-- Nodes
------------------------------------------------------------

nodesExprToDoc :: Bool -> [InfoNode] -> Doc AnsiStyle
nodesExprToDoc _ infoNodes =
  braces $
    nest 2 $
      line <>
      vsep (map nodeLine infoNodes) <>
      line
  where
    nodeLine (node, isInitial, neighs) =
      let nodeDoc =
            if isInitial
              then parens (nodeColor (pretty node))
              else nodeColor (pretty node)
          neighsDoc =
            braces $
              hsep $
                punctuate comma (map (nodeColor . pretty) neighs)
      in nodeDoc <+> opColor (pretty "=>") <+> neighsDoc

------------------------------------------------------------
-- Labels
------------------------------------------------------------

labelsExprToDoc :: Bool -> [Label] -> Doc AnsiStyle
labelsExprToDoc _ labels =
  braces $
    nest 2 $
      line <>
      vsep (map labelLine labels) <>
      line
  where
    labelLine (node, atoms) =
      let atomsDoc =
            braces $
              hsep $
                punctuate comma (map (atomColor . pretty) atoms)
      in nodeColor (pretty node)
         <+> opColor (pretty "<=")
         <+> atomsDoc

------------------------------------------------------------
-- Formulas
------------------------------------------------------------

formulaToDoc :: Formula -> Doc AnsiStyle
formulaToDoc F                        = atomColor (pretty "⊥")
formulaToDoc T                        = atomColor (pretty "⊤")
formulaToDoc (Atom atom)              = atomColor (pretty atom)
formulaToDoc (Not p)                  = opColor (pretty "¬") <+> parens (formulaToDoc p)
formulaToDoc (BinaryOp op p q)        =
  parens (formulaToDoc p <+> binaryOpToDoc op <+> parens (formulaToDoc q))
formulaToDoc (UQuantifier uquant p)   =
  uquantifierToDoc uquant <+> parens (formulaToDoc p)
formulaToDoc (BQuantifier bquant p q) =
  bquantifierToDoc bquant p q

sformulaToDoc :: SFormula -> Doc AnsiStyle
sformulaToDoc SF                          = atomColor (pretty "⊥")
sformulaToDoc ST                          = atomColor (pretty "⊤")
sformulaToDoc (SAtom atom)                = atomColor (pretty atom)
sformulaToDoc (SNot sp)                   = opColor (pretty "¬") <+> parens (sformulaToDoc sp)
sformulaToDoc (SBinaryOp op sp sq)        =
  parens (sformulaToDoc sp <+> binaryOpToDoc op <+> parens (sformulaToDoc sq))
sformulaToDoc (SUQuantifier uquant sp)    =
  uquantifierToDoc uquant <+> parens (sformulaToDoc sp)
sformulaToDoc (SBQuantifier bquant sp sq) =
  sbquantifierToDoc bquant sp sq
sformulaToDoc (SVar var)                  = varColor (pretty var)

------------------------------------------------------------
-- Quantifiers and operators
------------------------------------------------------------

uquantifierToDoc :: UQuantifier -> Doc AnsiStyle
uquantifierToDoc AC = quantifierColor (pretty "A()")
uquantifierToDoc EC = quantifierColor (pretty "E()")
uquantifierToDoc AR = quantifierColor (pretty "A<>")
uquantifierToDoc ER = quantifierColor (pretty "E<>")
uquantifierToDoc AS = quantifierColor (pretty "A[]")
uquantifierToDoc ES = quantifierColor (pretty "E[]")

sbquantifierToDoc :: BQuantifier -> SFormula -> SFormula -> Doc AnsiStyle
sbquantifierToDoc AU sp sq =
  quantifierColor (pretty "A") <> untilToDoc (sformulaToDoc sp) (sformulaToDoc sq)
sbquantifierToDoc EU sp sq =
  quantifierColor (pretty "E") <> untilToDoc (sformulaToDoc sp) (sformulaToDoc sq)

bquantifierToDoc :: BQuantifier -> Formula -> Formula -> Doc AnsiStyle
bquantifierToDoc AU p q =
  quantifierColor (pretty "A") <> untilToDoc (formulaToDoc p) (formulaToDoc q)
bquantifierToDoc EU p q =
  quantifierColor (pretty "E") <> untilToDoc (formulaToDoc p) (formulaToDoc q)

untilToDoc :: Doc AnsiStyle -> Doc AnsiStyle -> Doc AnsiStyle
untilToDoc d1 d2 =
  brackets (d1 <+> quantifierColor (pretty "U") <+> d2)

binaryOpToDoc :: BinaryOp -> Doc AnsiStyle
binaryOpToDoc And      = opColor (pretty "&&")
binaryOpToDoc Or       = opColor (pretty "||")
binaryOpToDoc Implies  = opColor (pretty "->")

------------------------------------------------------------
-- Types and sentences
------------------------------------------------------------

typeToDoc :: Type -> Doc AnsiStyle
typeToDoc ModelTy   = typeOpColor (pretty "Model")
typeToDoc LabelsTy  = typeOpColor (pretty "Labels")
typeToDoc NodesTy   = typeOpColor (pretty "Nodes")
typeToDoc FormulaTy = typeOpColor (pretty "Formula")

sentenceToDoc :: Sentence -> Doc AnsiStyle
sentenceToDoc (Def _ var ty e) =
  sep
    [ keywordColor (pretty "define")
    , varColor (pretty var)
    , pretty "::"
    , typeToDoc ty
    , opColor (pretty "=")
    ]
  <+> nest 2 (exprToDoc e)

sentenceToDoc (Export _ model file) =
  sep
    [ keywordColor (pretty "export")
    , exprToDoc model
    , keywordColor (pretty "as")
    , pretty file
    ]

sentenceToDoc (IsSatis _ formula) =
  sep
    [ opColor (pretty "|=")
    , exprToDoc formula
    ]

sentenceToDoc (Models _ model formula) =
  sep
    [ exprToDoc model
    , opColor (pretty "|=")
    , exprToDoc formula
    ]

sentenceToDoc (IsValid _ model node formula) =
  sep
    [ exprToDoc model
    , pretty ","
    , nodeColor (pretty node)
    , opColor (pretty "|=")
    , exprToDoc formula
    ]

------------------------------------------------------------
-- Rendering helpers
------------------------------------------------------------

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

ppType :: Type -> String
ppType = render . typeToDoc

ppSentence :: Sentence -> String
ppSentence = render . sentenceToDoc
