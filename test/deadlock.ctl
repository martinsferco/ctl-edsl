// Definición de etiquetas descriptivas
define MutexLabels :: Labels = {
    _idle        <= { n1, n2 }     // Estado inicial: nadie pide nada
    _p1_waiting  <= { t1, n2 }     // P1 quiere entrar, P2 no
    _p2_waiting  <= { n1, t2 }     // P2 quiere entrar, P1 no
    _both_trying <= { t1, t2 }     // Ambos quieren entrar (Conflicto)
    _p1_critical <= { c1, n2 }     // P1 dentro
    _p2_critical <= { n1, c2 }     // P2 dentro
    _p1_in_p2_wait <= { c1, t2 }   // P1 dentro y P2 esperando
    _p2_in_p1_wait <= { t1, c2 }   // P2 dentro y P1 esperando
}

// Definición de transiciones con nombres claros
define MutexNodes :: Nodes = {
    (_idle)        => { _p1_waiting, _p2_waiting }
    _p1_waiting    => { _p1_critical, _both_trying }
    _p2_waiting    => { _p2_critical, _both_trying }
    _both_trying   => { _p1_in_p2_wait, _p2_in_p1_wait } 
    _p1_critical   => { _idle, _p1_in_p2_wait }
    _p2_critical   => { _idle, _p2_in_p1_wait }
    _p1_in_p2_wait => { _p2_waiting } // P1 sale, queda P2 esperando
    _p2_in_p1_wait => { _p1_waiting } // P2 sale, queda P1 esperando
}

define MutexModel :: Model = <MutexNodes, MutexLabels>

// --- Fórmulas de Verificación ---

// Seguridad: Nunca están ambos en la sección crítica
define Safety :: Formula = A[] !(c1 && c2)

// Vivacidad: Si P1 entra en estado de espera, eventualmente entrará a la sección crítica
define NoStarvationP1 :: Formula = A[] (t1 -> A<> c1)

export MutexModel as mutex

// Comprobaciones
// |= Safety
// mutex, _both_trying |= A<> (c1 || c2)