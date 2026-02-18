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
