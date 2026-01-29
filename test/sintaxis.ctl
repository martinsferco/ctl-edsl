

define Lab :: Labels = {
	_n1 <= { p } 
	_n2 <= {   }
	_n3 <= {  }
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


define NewModel :: Model = <MyNodes, Lab>
// define OtherModel :: Model = < MyNodes, OthersLabels >

define NotP :: Formula = !p
define Phi :: Formula = A [p -> q U E[] NotP]
define Q :: Formula = E() p

NewModel |= Q
NewModel, _n2 |= Q
