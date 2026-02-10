module PrettyPrinter 
    ( ppSentence
    , ppFormula
    , ppValue
    , ppEvalResult
    , ppType ) where

import Model.TSystem ( TSystem(..), Graph(..) )
import EvalResult    ( EvalResult (..), CheckNodeType (..) )
import Common        ( NodeIdent, Nodes, InfoNode, InfoNodes (..), LabelingFunction, Label )
import Lang          ( Sentence (..), Formula (..), Value (..), Type (..), BinaryOp (..), BQuantifier (..),
                       UQuantifier (..), SFormula (..), Expr (..) )

import Prettyprinter.Render.Terminal
import Prettyprinter
import Data.Text (unpack)

import qualified Data.Set as Set
import qualified Data.Map as Map

keywordColor, successColor, failColor :: Doc AnsiStyle -> Doc AnsiStyle
keywordColor = annotate (color Cyan)
successColor = annotate (color Green <> bold)
failColor    = annotate (color Red <> bold)

title :: String -> Doc AnsiStyle
title s = annotate bold (pretty s)

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

exprToDoc :: Expr -> Doc AnsiStyle
exprToDoc (FormulaExpr _ f) = sformulaToDoc 0 f
exprToDoc (VarExpr _ v)     = pretty v
exprToDoc (NodesExpr _ ns)  = nodesExprToDoc False ns
exprToDoc (LabelsExpr _ ls) = labelsExprToDoc False ls
exprToDoc (ModelExpr _ n l) = angles $ exprToDocInline n <> comma <+> exprToDocInline l

exprToDocInline :: Expr -> Doc AnsiStyle
exprToDocInline (NodesExpr _ ns) = nodesExprToDoc True ns
exprToDocInline (LabelsExpr _ ls) = labelsExprToDoc True ls
exprToDocInline e = exprToDoc e

nodesExprToDoc :: Bool -> [InfoNode] -> Doc AnsiStyle
nodesExprToDoc inline ns 
  | inline    = braces (hsep $ punctuate comma (map nodeLine ns))
  | otherwise = braces $ nest 2 (line <> vsep (map nodeLine ns) <> line)
  where
    nodeLine (n, isInit, neighs) = nodeDoc <+> pretty "=>" <+> neighsDoc
        where
            nodeDoc = if isInit 
                      then parens (pretty n) 
                      else pretty n
            neighsDoc = braces $ hsep $ punctuate comma (map pretty neighs)

labelsExprToDoc :: Bool -> [Label] -> Doc AnsiStyle
labelsExprToDoc inline ls 
  | inline    = braces (hsep $ punctuate comma (map labelLine ls))
  | otherwise = braces $ nest 2 (line <> vsep (map labelLine ls) <> line)
  where
    labelLine (n, atoms) = 
        pretty n <+> pretty "<=" <+> 
        braces (hsep $ punctuate comma (map pretty atoms))

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

valueToDoc :: Value -> Doc AnsiStyle
valueToDoc (Formula f)    = formulaToDoc 0 f
valueToDoc (Model ts)     = modelToDoc ts
valueToDoc (Labels lf)    = labelsExprToDoc False (labelingToLabels lf)
valueToDoc (Nodes infoNs) = nodesExprToDoc False (infoNodesToList infoNs)

modelToDoc :: TSystem -> Doc AnsiStyle
modelToDoc (TSystem g lf) = angles $  nodesExprToDoc False (graphToInfoNodes g) <> comma
                                  <+> labelsExprToDoc False (labelingToLabels lf)

labelingToLabels :: LabelingFunction -> [Label]
labelingToLabels lf = [ (n, Set.toList atoms)| (n, atoms) <- Map.toList lf ]

graphToInfoNodes :: Graph -> [InfoNode]
graphToInfoNodes (Graph _ i t) = [ (n, Set.member n i, Set.toList neighs) | (n, neighs) <- Map.toList t ]

infoNodesToList :: InfoNodes -> [InfoNode]
infoNodesToList (InfoNodes i m) =[(n, Set.member n i, Set.toList neighs) | (n, neighs) <- Map.toList m ]

--------------------------------------------------------------------------------
-- Formulas and Superfitial Formulas
--------------------------------------------------------------------------------
formulaToDoc :: Int -> Formula -> Doc AnsiStyle
formulaToDoc _ F = pretty "⊥"
formulaToDoc _ T = pretty "⊤"
formulaToDoc _ (Atom a) = pretty a

formulaToDoc prec (Not p) = 
    parensIf (prec > 4) $ 
    pretty "¬" <> formulaToDoc 4 p

formulaToDoc prec (BinaryOp op p q) = 
    parensIf (prec > opPrec) $ 
    formulaToDoc (opPrec + 1) p <+> binaryOpToDoc op <+> formulaToDoc (opPrec + 1) q
  where
    opPrec = case op of
        Implies -> 1
        Or      -> 2
        And     -> 3

formulaToDoc _ (UQuantifier uq p) = 
    uquantifierToDoc uq <+> formulaToDoc 5 p

formulaToDoc _ (BQuantifier bq p q) = 
    bquantifierToDoc bq p q

sformulaToDoc :: Int -> SFormula -> Doc AnsiStyle
sformulaToDoc _ SF = pretty "⊥"
sformulaToDoc _ ST = pretty "⊤"
sformulaToDoc _ (SAtom a) = pretty a
sformulaToDoc _ (SVar v) = pretty v

sformulaToDoc prec (SNot p) = 
    parensIf (prec > 4) $ 
    pretty "¬" <> sformulaToDoc 4 p

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
uquantifierToDoc AC = pretty "∀○"
uquantifierToDoc EC = pretty "∃○"
uquantifierToDoc AR = pretty "∀◇"
uquantifierToDoc ER = pretty "∃◇"
uquantifierToDoc AS = pretty "∀□"
uquantifierToDoc ES = pretty "∃□"

sbquantifierToDoc :: BQuantifier -> SFormula -> SFormula -> Doc AnsiStyle
sbquantifierToDoc q p r = 
    pretty (case q of AU -> "∀"; EU -> "∃") <>
    brackets (sformulaToDoc 0 p <+> pretty "U" <+> sformulaToDoc 0 r)

bquantifierToDoc :: BQuantifier -> Formula -> Formula -> Doc AnsiStyle
bquantifierToDoc q p r = 
    pretty (case q of AU -> "∀"; EU -> "∃") <>
    brackets (formulaToDoc 0 p <+> pretty "U" <+> formulaToDoc 0 r)

binaryOpToDoc :: BinaryOp -> Doc AnsiStyle
binaryOpToDoc And     = pretty "∧"
binaryOpToDoc Or      = pretty "∨"
binaryOpToDoc Implies = pretty "→"

--------------------------------------------------------------------------------
-- Types 
--------------------------------------------------------------------------------
typeToDoc :: Type -> Doc AnsiStyle
typeToDoc ModelTy   = keywordColor (pretty "Model")
typeToDoc LabelsTy  = keywordColor (pretty "Labels")
typeToDoc NodesTy   = keywordColor (pretty "Nodes")
typeToDoc FormulaTy = keywordColor (pretty "Formula")

--------------------------------------------------------------------------------
-- Sentences
--------------------------------------------------------------------------------

sentenceToDoc :: Sentence -> Doc AnsiStyle
sentenceToDoc (Def _ v t e)      = sep [ keywordColor (pretty "define"), pretty v, pretty "::", typeToDoc t, pretty "="] <+> exprToDoc e
sentenceToDoc (Export _ m f)     = sep [ keywordColor (pretty "export"), exprToDoc m, keywordColor (pretty "as"), pretty f ]
sentenceToDoc (IsSatis _ f file) = sep [ keywordColor (pretty "⊨"), exprToDoc f, keywordColor (pretty "as"), pretty file ]
sentenceToDoc (Models _ m f)     = sep [ exprToDoc m, keywordColor (pretty "⊨"), exprToDoc f ]
sentenceToDoc (IsValid _ m n f)  = sep [ exprToDoc m, comma, pretty n, keywordColor (pretty "⊨"), exprToDoc f]


--------------------------------------------------------------------------------
-- Evaluation Results
--------------------------------------------------------------------------------

evalResultToDoc :: EvalResult -> Doc AnsiStyle
evalResultToDoc result = vsep
    [ emptyDoc
    , topBorder
    , emptyDoc
    , formulaSection result
    , emptyDoc
    , nodesSection result
    , emptyDoc
    , verificationSection result
    , emptyDoc
    , statusSection result
    , emptyDoc
    , pathSection result
    , emptyDoc
    , bottomBorder ]

topBorder :: Doc AnsiStyle
topBorder = vsep
    [ pretty "┏" <> pretty (replicate 58 '━') <>                              pretty "┓"
    , pretty "┃  CTL MODEL CHECKING RESULT"   <> pretty (replicate 31 ' ') <> pretty "┃"
    , pretty "┗" <> pretty (replicate 58 '━') <>                              pretty "┛"
    ]

bottomBorder :: Doc AnsiStyle
bottomBorder = pretty "┗" <> pretty (replicate 58 '━') <> pretty "┛"

formulaSection :: EvalResult -> Doc AnsiStyle
formulaSection result =  (title " Formula:    ") <> formulaToDoc 0 (evalFormula result)

statusSection :: EvalResult -> Doc AnsiStyle
statusSection result = 
    (title " Status:     ") <> statusDoc
  where
    statusDoc = if holds result
                then successColor (pretty "The formula holds!")
                else failColor    (pretty "The formula does not hold.")

nodesSection :: EvalResult -> Doc AnsiStyle
nodesSection result = vsep
    [ title " Sat Nodes", pretty "  " <> nodeSetToDoc (satNodes result), emptyDoc, checkTypeDoc ]
  where
    checkTypeDoc = case checkType result of
        CheckInitials initNodes -> vsep [ title " Initial Nodes", pretty "  " <> nodeSetToDoc initNodes ]
        CheckNode node          -> vsep [ title " Checked Node", pretty "  " <> pretty node ]

verificationSection :: EvalResult -> Doc AnsiStyle
verificationSection result = vsep
    [ title " Verification"
    , case checkType result of
        CheckInitials initials -> verificationInitials initials
        CheckNode node -> verificationNode node
    ]
  where
    sat = satNodes result
    
    verificationInitials :: Nodes -> Doc AnsiStyle
    verificationInitials initials
        | holds result = pretty "  " <> successColor (pretty "OK") <+> nodeSetToDoc initials <+> pretty "⊆" <+> nodeSetToDoc sat
        | otherwise = 
            let violating = Set.difference initials sat
            in vsep
                [ pretty "  " <> failColor (pretty "FAIL") <+> nodeSetToDoc initials <+> pretty "⊄" <+> nodeSetToDoc sat
                , emptyDoc
                , pretty "  Violating nodes: " <> nodeSetToDoc violating
                ]
    
    verificationNode :: NodeIdent -> Doc AnsiStyle
    verificationNode node
        | holds result = pretty "  " <> successColor (pretty "OK") <+> pretty node <+> pretty "∈" <+> nodeSetToDoc sat
        | otherwise    = pretty "  " <> failColor (pretty "FAIL") <+> pretty node <+> pretty "∉" <+> nodeSetToDoc sat
            

pathSection :: EvalResult -> Doc AnsiStyle
pathSection result
    | null (examplePath result) = emptyDoc
    | holds result = witnessPath (examplePath result)
    | otherwise = counterExamplePath (examplePath result)

witnessPath :: [(NodeIdent, String)] -> Doc AnsiStyle
witnessPath path = vsep
    [ successColor (pretty " Witness Path") <+> pretty (pathOrigin path)
    , emptyDoc
    , indent 2 (vsep (pathSteps path)) ]

counterExamplePath :: [(NodeIdent, String)] -> Doc AnsiStyle
counterExamplePath path = vsep
    [ failColor (pretty " Counter-Example Path") <+> pretty (pathOrigin path)
    , emptyDoc
    , indent 2 (vsep (pathSteps path)) ]

pathOrigin :: [(NodeIdent, String)] -> String
pathOrigin []        = ""
pathOrigin ((n,_):_) = "(from " ++ n ++ ")"

pathSteps :: [(NodeIdent, String)] -> [Doc AnsiStyle]
pathSteps path = concat $ zipWith formatStep [0..] path
  where
    formatStep :: Int -> (NodeIdent, String) -> [Doc AnsiStyle]
    formatStep idx (node, desc)
        | idx == length path - 1 = 
            [ pretty "[" <> pretty (show idx) <> pretty "] " <> 
              pretty node <+> 
              pretty "  |  " <+> 
              pretty desc
            ]
        | otherwise = 
            [ pretty "[" <> pretty (show idx) <> pretty "] " <> 
              pretty node <+> 
              pretty "  |  " <+> 
              pretty desc
            , pretty "     ↓"
            ]

nodeSetToDoc :: Nodes -> Doc AnsiStyle
nodeSetToDoc ns 
    | Set.null ns = pretty "∅" <+> pretty "(no nodes)"
    | Set.size ns > 10 = 
        braces $ hsep $ punctuate comma 
            (map pretty (take 10 $ Set.toList ns) ++ [pretty "..."])
    | otherwise = 
        braces $ hsep $ punctuate comma 
            (map pretty (Set.toList ns))


--------------------------------------------------------------------------------
-- Pretty Printers
--------------------------------------------------------------------------------

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

ppEvalResult :: EvalResult -> String
ppEvalResult = render . evalResultToDoc

ppSentence :: Sentence -> String
ppSentence = render . sentenceToDoc

ppFormula :: Formula -> String
ppFormula = render . (formulaToDoc 0)

ppValue :: Value -> String
ppValue = render . valueToDoc

ppType :: Type -> String
ppType = render . typeToDoc