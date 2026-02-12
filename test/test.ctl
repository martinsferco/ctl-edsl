define P :: Formula = p
define Q :: Formula = q
define R :: Formula = r
define S :: Formula = s
define TT :: Formula = tt

define NP :: Formula = !P
define NQ :: Formula = !Q
define NR :: Formula = !R
define NS :: Formula = !S
define NTT :: Formula = !TT

define P_and_Q :: Formula = P && Q
define Q_and_R :: Formula = Q && R
define R_and_S :: Formula = R && S
define S_and_TT :: Formula = S && TT

define P_or_Q :: Formula = P || Q
define Q_or_R :: Formula = Q || R
define R_or_S :: Formula = R || S
define S_or_TT :: Formula = S || TT

define Impl_PQ :: Formula = P -> Q
define Impl_QR :: Formula = Q -> R
define Impl_RS :: Formula = R -> S
define Impl_STT :: Formula = S -> TT

define AX_P :: Formula = A() P
define EX_P :: Formula = E() P
define AX_Q :: Formula = A() Q
define EX_Q :: Formula = E() Q

define AF_P :: Formula = A<> P
define EF_P :: Formula = E<> P
define AF_Q :: Formula = A<> Q
define EF_Q :: Formula = E<> Q

define AG_P :: Formula = A[] P
define EG_P :: Formula = E[] P
define AG_Q :: Formula = A[] Q
define EG_Q :: Formula = E[] Q

define U_PQ :: Formula = E[P U Q]
define U_QR :: Formula = E[Q U R]
define U_RS :: Formula = A[R U S]
define U_STT :: Formula = A[S U TT]

define L1 :: Formula = AG_P -> AF_Q
define L2 :: Formula = (AX_P && EX_Q) || EF_Q
define L3 :: Formula = A[] (P -> EF_Q)
define L4 :: Formula = E<> (Q && AG_P)
define L5 :: Formula = A[] (L1 && L2)
define L6 :: Formula = E<> (L3 || L4)
define L7 :: Formula = A[] (L5 -> L6)
define L8 :: Formula = E[L1 U L3]
define L9 :: Formula = A[L2 U L4]
define L10 :: Formula = (L7 && L8) || L9

define U1 :: Formula = E[P U (Q && E[R U S])]
define U2 :: Formula = A[(P || Q) U (R && AF_P)]
define U3 :: Formula = E[(AX_P && EX_Q) U (AG_P || EF_Q)]
define U4 :: Formula = A[(L1 || L2) U (L3 && L4)]

define Fair_PQ :: Formula = A[] (P -> A<> Q)
define Fair_QR :: Formula = A[] (Q -> E<> R)
define Fair_RS :: Formula = A[] (R -> A<> S)
define Fair_All :: Formula = Fair_PQ && Fair_QR && Fair_RS

define Mega1 :: Formula = A[] ((P -> EF_Q) && (Q -> EF_Q) && (R -> EF_Q))
define Mega2 :: Formula = E<> (AG_P && AF_Q && EG_P)
define Mega3 :: Formula = A[] ((L10 || U1) -> (Mega1 && Fair_All))
define Mega4 :: Formula = E<> ((U2 && U3) || (L7 && U4))
define Mega5 :: Formula = A[] (Mega3 -> Mega4)
define Mega6 :: Formula = (Mega5 && Mega2) || Fair_All

define Final1 :: Formula = Mega6
define Final2 :: Formula = A[] (Final1 -> E<> (U1 && L4))
define Final3 :: Formula = E<> (Final2 && A[] (P_or_Q))
define Final4 :: Formula = A[(Final1 || Final3) U (Mega4 && Fair_PQ)]

|= Final1 as file
|= Final2 as file
|= Final3 as file
|= Final4 as file
|= Mega6 as file
|= U4 as file
|= L10 as file
|= Fair_All as file
