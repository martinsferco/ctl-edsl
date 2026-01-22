

define Lab :: Labels = {
	_n1 <= { p, q } 
	_n1 <= { p, q }
	_n1 <= { p, q }
}

define OthersLabels :: Labels = {
	_n1 <= { p, q } 
	_n1 <= { p, q }
	_n1 <= { p, q }
}


define Trans :: Transitions = {

	_n1  => { _n1 }
  (_n1) => { _n1 }
	_n1  => { _n1 }
}


define NewModel :: Model = <Trans, Lab>
define OtherModel :: Model = < Trans, OthersLabels >


define Phi :: Formula = A [p -> q U E[] !p]

NewModel |= Phi 

export NewModel

isValid Phi