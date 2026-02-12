// ==========================================================
// 1. TEST DE SINTAXIS Y PRECEDENCIA (PARA EL PARSER)
// ==========================================================

define T1 :: Formula = T
define T2 :: Formula = ⊥
define T3 :: Formula = p
define T4 :: Formula = T1

// Cuantificadores combinados (A/E + Op Temporal)
define C1 :: Formula = A () p    
define C2 :: Formula = E () p    
define C3 :: Formula = A <> p    
define C4 :: Formula = E <> p    
define C5 :: Formula = A [] p    
define C6 :: Formula = E [] p    

// Operadores Lógicos (&&, ||, !)
define L1 :: Formula = !p && q || r
define L2 :: Formula = p || q && r   
define L3 :: Formula = (p || q) && r

// Binarios (Until)
define B1 :: Formula = A [p U q]
define B2 :: Formula = E [!p || r U q && s]

// Precedencia Compleja: A [] (p -> E <> (q && !r))
define P1 :: Formula = A [] (p -> E <> (q && !r))

// Asociatividad de la implicación (Derecha)
define P2 :: Formula = p -> q -> r


// ==========================================================
// 2. MODELO: CONTADOR BINARIO DE 3 BITS
// ==========================================================

define BitLabels :: Labels = {
    _000 <= { b0_f, b1_f, b2_f }
    _001 <= { b0_t, b1_f, b2_f }
    _010 <= { b0_f, b1_t, b2_f }
    _011 <= { b0_t, b1_t, b2_f }
    _100 <= { b0_f, b1_f, b2_t }
    _101 <= { b0_t, b1_f, b2_t }
    _110 <= { b0_f, b1_t, b2_t }
    _111 <= { b0_t, b1_t, b2_t }
}

define BitNodes :: Nodes = {
    (_000) => { _001 }
    _001 => { _010 }
    _010 => { _011 }
    _011 => { _100 }
    _100 => { _101 }
    _101 => { _110 }
    _110 => { _111 }
    _111 => { _000 }
}

define Counter :: Model = <BitNodes, BitLabels>


// ==========================================================
// 3. MODELO: EL LABERINTO (MAZE) CON CICLOS Y TRAMPAS
// ==========================================================

define MazeLabels :: Labels = {
    _start <= { entrada }
    _room1 <= { p }
    _room2 <= { q }
    _room3 <= { p, q }
    _trap  <= { dead }
    _exit  <= { win }
}

define MazeNodes :: Nodes = {
    (_start) => { _room1, _room2 }
    _room1   => { _room3 }
    _room1   => { _trap }
    _room2   => { _room1, _exit }
    _room3   => { _room2, _room3 } 
    _trap    => { _trap }          
    _exit    => { _exit }          
}

define Maze :: Model = <MazeNodes, MazeLabels>

Maze |= E [] p
Maze |= A <> win
Maze |= E [ p U win ]
Maze, _trap |= A [] dead

export Maze as maze_check

// ==========================================================
// 4. MODELO: JUSTICIA Y OSCILACIÓN (FAIRNESS)
// ==========================================================

define FairLabels :: Labels = {
    _up   <= { arriba }
    _down <= { abajo }
}

define FairNodes :: Nodes = {
    (_up) => { _up, _down }
    _down => { _up }
}

define FairModel :: Model = <FairNodes, FairLabels>


// ==========================================================
// 5. EVALUACIONES Y EXPORTACIONES
// ==========================================================

export Counter as counter_results
export Maze as maze_check

// Verificaciones sobre el Contador
Counter |= E <> (b0_t && b1_t && b2_t)
Counter |= A [] (b0_t -> A () b0_f)

// Verificaciones sobre el Laberinto
Maze |= E [] p
Maze |= A <> win
Maze |= E [ p U win ]
Maze, _trap |= A [] dead

// Verificaciones sobre el Modelo de Justicia
FairModel |= E [] arriba
FairModel |= A [] (abajo -> A () arriba)

// Test de satisfacción global (usando variables mayúsculas como alias)
define Phi :: Formula = A [] (p -> A <> q)
|= Phi as manolo_result