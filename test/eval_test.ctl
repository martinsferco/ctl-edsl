define L :: Labels = {

	_n1 <= { p }
	_n2 <= { p }
	_n3 <= { p }
	_n4 <= { p }
	_n5 <= { p }
	_n6 <= { p }
	_n7 <= { q }

}
define N :: Nodes = {

	(_n1) => { _n2 }
	_n2 => { _n3 }
	_n3 => { _n4 }
	_n4 => { _n5 }
	_n5 => { _n6 }
	_n6 => { _n7 }
	_n7 => { _n7 }
}


define M :: Model = <N,L>


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

export NewModel as myMod

NewModel |= Q
NewModel, _n2 |= Q

|= Q as manolo