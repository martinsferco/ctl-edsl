// ==========================
// Definiciones de átomos y fórmulas
// ==========================

define P :: Formula =
  p

define Q :: Formula =
  q

define R :: Formula =
  r

define NotP :: Formula =
  !p

define PAndQ :: Formula =
  p && q

define POrQ :: Formula =
  p || q

define PImpliesQ :: Formula =
  p -> q

define AlwaysP :: Formula =
  A[] p

define EventuallyQ :: Formula =
  E<> q

define UntilPQ :: Formula =
  E[p U q]

// ==========================
// Definición de nodos
// ==========================

define NODES1 :: Nodes =
{
  (_s0) => {_s1, _s2}
  _s1   => {_s1}
  _s2   => {_s0}
}

define NODES2 :: Nodes =
{
  (_i) => {_i}
}

define NODES3 :: Nodes =
{
  (_a) => {_b}
  _b   => {_c}
  _c   => {_c}
}

// ==========================
// Definición de etiquetas
// ==========================

define LABELS1 :: Labels =
{
  _s0 <= {p}
  _s1 <= {q}
  _s2 <= {r}
}

define LABELS2 :: Labels =
{
  _i <= {}
}

define LABELS3 :: Labels =
{
  _a <= {p}
  _b <= {p, q}
  _c <= {q}
}

// ==========================
// Modelos
// ==========================

define M1 :: Model =
<
  NODES1,
  LABELS1
>

define M2 :: Model =
<
  NODES2,
  LABELS2
>

define M3 :: Model =
<
  NODES3,
  LABELS3
>

// ==========================
// Consultas de model checking
// ==========================

// Modelo satisface fórmula global
M1 |= p
M1 |= q || r
M1 |= p -> q
M1 |= A[] (p || q)
M1 |= E<> r
M1 |= E[p U q]

// Modelo, nodo específico
M1, _s0 |= p
M1, _s0 |= E<> q
M1, _s1 |= q
M1, _s2 |= r || p

// Modelo trivial
M2 |= A[] !p
M2, _i |= A[] !p
M2, _i |= E<> p

// Modelo con cadena
M3 |= E<> q
M3 |= A[] (p || q)
M3 |= E[p U q]
M3, _a |= p
M3, _b |= p && q
M3, _c |= q

// ==========================
// Fórmulas sin modelo (IsSatis)
// ==========================

|= p
|= p -> p
|= E<> p
|= A[] (p -> E<> q)
|= E[p U q]

// ==========================
// Export
// ==========================

export M1 as model1
export M2 as model2
export M3 as model3
