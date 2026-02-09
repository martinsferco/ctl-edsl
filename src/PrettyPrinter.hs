module PrettyPrinter where

import Lang
import Common
import EvalResult
import Prettyprinter
import Prettyprinter.Render.Terminal
import Data.Text (unpack)

import qualified Data.Set as Set

-- Colores más discretos - solo para resaltar elementos clave
keywordColor, successColor, failColor :: Doc AnsiStyle -> Doc AnsiStyle
keywordColor = annotate (color Cyan)
successColor = annotate (color Green <> bold)
failColor    = annotate (color Red <> bold)

------------------------------------------------------------
-- Expressions
------------------------------------------------------------

exprToDoc :: Expr -> Doc AnsiStyle
exprToDoc (FormulaExpr _ f) = sformulaToDoc 0 f
exprToDoc (VarExpr _ v) = pretty v
exprToDoc (NodesExpr _ ns) = nodesExprToDoc False ns
exprToDoc (LabelsExpr _ ls) = labelsExprToDoc False ls
exprToDoc (ModelExpr _ n l) = 
    angles $ exprToDocInline n <> comma <+> exprToDocInline l

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

-- Helper para binary quantifiers
bquantifierToDoc :: BQuantifier -> Formula -> Formula -> Doc AnsiStyle
bquantifierToDoc q p r = 
    pretty (case q of AU -> "∀"; EU -> "∃") <>
    brackets (formulaToDoc 0 p <+> pretty "U" <+> formulaToDoc 0 r)

binaryOpToDoc :: BinaryOp -> Doc AnsiStyle
binaryOpToDoc And     = pretty "∧"
binaryOpToDoc Or      = pretty "∨"
binaryOpToDoc Implies = pretty "→"


typeToDoc :: Type -> Doc AnsiStyle
typeToDoc ModelTy   = pretty "Model"
typeToDoc LabelsTy  = pretty "Labels"
typeToDoc NodesTy   = pretty "Nodes"
typeToDoc FormulaTy = pretty "Formula"

sentenceToDoc :: Sentence -> Doc AnsiStyle
sentenceToDoc (Def _ v t e) = 
    sep [ keywordColor (pretty "define")
        , pretty v
        , pretty "::"
        , typeToDoc t
        , pretty "="
        ] <+> exprToDoc e

sentenceToDoc (Export _ m f) = 
    sep [ keywordColor (pretty "export")
        , exprToDoc m
        , keywordColor (pretty "as")
        , pretty f
        ]

sentenceToDoc (IsSatis _ f) = 
    sep [ keywordColor (pretty "⊨"), exprToDoc f ]

sentenceToDoc (Models _ m f) = 
    sep [ exprToDoc m, keywordColor (pretty "⊨"), exprToDoc f ]

sentenceToDoc (IsValid _ m n f) = 
    sep [ exprToDoc m
        , comma
        , pretty n
        , keywordColor (pretty "⊨")
        , exprToDoc f
        ]


render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

ppSentence :: Sentence -> String
ppSentence = render . sentenceToDoc

ppFormula :: Formula -> String
ppFormula = render . (formulaToDoc 0)


------------------------------------------------------------
-- Evaluation Results
------------------------------------------------------------

-- Pretty printer principal para resultados de evaluación
evalResultToDoc :: EvalResult -> Doc AnsiStyle
evalResultToDoc result = vsep
    [ emptyDoc
    , topBorder
    , emptyDoc
    , formulaSection result
    , emptyDoc
    , statusSection result
    , emptyDoc
    , nodesSection result
    , emptyDoc
    , verificationSection result
    , emptyDoc
    , pathSection result
    , emptyDoc
    , bottomBorder
    ]

topBorder :: Doc AnsiStyle
topBorder = vsep
    [ pretty "┏" <> pretty (replicate 58 '━') <> pretty "┓"
    , pretty "┃  CTL MODEL CHECKING RESULT"   <> pretty (replicate 31 ' ') <> pretty "┃"
    , pretty "┗" <> pretty (replicate 58 '━') <> pretty "┛"
    ]

bottomBorder :: Doc AnsiStyle
bottomBorder = pretty "┗" <> pretty (replicate 58 '━') <> pretty "┛"

formulaSection :: EvalResult -> Doc AnsiStyle
formulaSection result =  pretty "Formula:    " <> formulaToDoc 0 (evalFormula result)

statusSection :: EvalResult -> Doc AnsiStyle
statusSection result = 
    pretty "Status:     " <> statusDoc
  where
    statusDoc = if holds result
                then successColor (pretty "The formula holds!")
                else failColor (pretty "The formula does not hold.")

nodesSection :: EvalResult -> Doc AnsiStyle
nodesSection result = vsep
    [ pretty "Sat Nodes", pretty "  " <> ppNodeSet (satNodes result), emptyDoc, checkTypeDoc ]
  where
    checkTypeDoc = case checkType result of
        CheckInitials nodes -> vsep [ pretty "Initial Nodes", pretty "  " <> ppNodeSet nodes ]
        CheckNode node -> vsep [ pretty "Checked Node", pretty "  " <> pretty node ]


verificationSection :: EvalResult -> Doc AnsiStyle
verificationSection result = vsep
    [ pretty "Verification"
    , case checkType result of
        CheckInitials inits -> verificationInitials inits
        CheckNode node -> verificationNode node
    ]
  where
    sat = satNodes result
    
    verificationInitials :: Nodes -> Doc AnsiStyle
    verificationInitials inits
        | holds result = vsep
            [ pretty "  " <> successColor (pretty "OK") <+> ppNodeSet inits <+> pretty "⊆" <+> ppNodeSet sat
            , emptyDoc
            , pretty "  The formula is valid with respect to all the initial nodes of the model."
            ]
        | otherwise = 
            let violating = Set.difference inits sat
                total = Set.size inits
                failing = Set.size violating
            in vsep
                [ pretty "  " <> failColor (pretty "FAIL") <+> ppNodeSet inits <+> pretty "⊄" <+> ppNodeSet sat
                , emptyDoc
                , pretty "  Violating nodes: " <> ppNodeSet violating
                , emptyDoc
                , pretty "  The formula is not valid in " <> pretty (show failing) <+> 
                  pretty "out of" <+> pretty (show total) <+> pretty "initial nodes."
                , pretty "  The formula is not valid in some of the initial nodes of the model."
                ]
    
    verificationNode :: NodeIdent -> Doc AnsiStyle
    verificationNode node
        | holds result = vsep
            [ pretty "  " <> successColor (pretty "OK") <+> pretty node <+> pretty "∈" <+> ppNodeSet sat
            , emptyDoc
            , pretty "  Node" <+> pretty node <+> pretty "satisfies the formula."
            , pretty "  The property is valid with respect with the node."
            ]
        | otherwise = vsep
            [ pretty "  " <> failColor (pretty "FAIL") <+> pretty node <+> pretty "∉" <+> ppNodeSet sat
            , emptyDoc
            , pretty "  The property is not valid with respect with the node."
            ]

pathSection :: EvalResult -> Doc AnsiStyle
pathSection result
    | null (examplePath result) = emptyDoc
    | holds result = witnessPath (examplePath result)
    | otherwise = counterExamplePath (examplePath result)


witnessPath :: [(NodeIdent, String)] -> Doc AnsiStyle
witnessPath path = vsep
    [ successColor (pretty "Witness Path") <+> pretty (if null path then "" else "(from " ++ fst (head path) ++ ")")
    , emptyDoc
    , indent 2 (vsep (pathSteps True path))
    , emptyDoc
    , indent 2 (pretty "The path demonstrates the formula holds:")
    , indent 2 (pretty "•" <+> pretty (snd $ last path))
    ]

counterExamplePath :: [(NodeIdent, String)] -> Doc AnsiStyle
counterExamplePath path = vsep
    [ failColor (pretty "Counter-Example Path") <+> pretty (if null path then "" else "(from " ++ fst (head path) ++ ")")
    , emptyDoc
    , indent 2 (vsep (pathSteps False path))
    , emptyDoc
    , indent 2 (pretty "The path demonstrates why the formula fails:")
    , indent 2 (vsep (map (\(_, desc) -> pretty "•" <+> pretty desc) path))
    ]

pathSteps :: Bool -> [(NodeIdent, String)] -> [Doc AnsiStyle]
pathSteps isWitness path = concat $ zipWith formatStep [0..] path
  where
    formatStep :: Int -> (NodeIdent, String) -> [Doc AnsiStyle]
    formatStep idx (node, desc)
        | idx == 0 = 
            [ pretty "[" <> pretty (show idx) <> pretty "] " <> 
              pretty node <+> 
              pretty (take 20 desc) <> 
              pretty (replicate (max 0 (30 - length (take 20 desc))) ' ') <> 
              pretty "<- starts here" <+> 
              (if isInitial then pretty "(initial)" else emptyDoc)
            , pretty "     |"
            ]
        | idx == length path - 1 && isCycle (node, desc) =
            [ pretty "[" <> pretty (show idx) <> pretty "] " <> 
              pretty node <+> 
              pretty (take 20 desc) <> 
              pretty (replicate (max 0 (30 - length (take 20 desc))) ' ') <> 
              pretty "--+"
            , pretty "      ^" <> 
              pretty (replicate 14 ' ') <> 
              pretty "|" <> 
              failColor (pretty "   <- " <> pretty (extractCycleDesc desc))
            , pretty "      +" <> 
              pretty (replicate 14 '-') <> 
              pretty "+"
            ]
        | otherwise = 
            [ pretty "[" <> pretty (show idx) <> pretty "] " <> 
              pretty node <+> 
              pretty (take 20 desc) <> 
              pretty (replicate (max 0 (30 - length (take 20 desc))) ' ') <> 
              pretty "<- " <> pretty (extractStepDesc desc)
            , pretty "     |"
            ]
      where
        isInitial = "initial" `elem` words desc || "starts" `elem` words desc
        isCycle (_, d) = "loop" `elem` words d || "cycle" `elem` words d
        extractCycleDesc d = if "loop" `elem` words d || "cycle" `elem` words d 
                             then takeWhile (/= '\n') d 
                             else d
        extractStepDesc d = takeWhile (/= '\n') d

-- Helper: Pretty print de un conjunto de nodos
ppNodeSet :: Nodes -> Doc AnsiStyle
ppNodeSet nodes 
    | Set.null nodes = pretty "∅" <+> pretty "(no nodes)"
    | Set.size nodes > 10 = 
        braces $ hsep $ punctuate comma 
            (map pretty (take 10 $ Set.toList nodes) ++ [pretty "..."])
    | otherwise = 
        braces $ hsep $ punctuate comma 
            (map pretty (Set.toList nodes))

-- Render final
ppEvalResult :: EvalResult -> String
ppEvalResult = render . evalResultToDoc