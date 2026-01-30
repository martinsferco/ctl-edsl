

define Lab :: Labels = {
	_n1 <= { p,q,r,a } 
	_n2 <= {  p }
	_n3 <= {  p}
}

define OthersLabels :: Labels = {
	_n1 <= { p, q } 
	_n1 <= { p, q }
	_n1 <= { p, q }
}


define MyNodes :: Nodes = {
	
  (_n1) => { _n2, _n3 }
	_n3  => { _n3 }
	_n2  => { _n2, _n1 }
}


define NewModel :: Model = <MyNodes, OthersLabels>
define OtherModel :: Model = < MyNodes,  { }	>



define NotP :: Formula = !p
define Phi :: Formula = A [p -> q U E[] NotP]
define Q :: Formula = A() p

export NewModel as myModel

// NewModel |= Q
// NewModel, _n2 |= Q
