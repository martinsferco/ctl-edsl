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
export Counter as counter_results

Counter |= E <> (b0_t && b1_t && b2_t)
Counter |= A [] (b0_t -> A () b0_f)