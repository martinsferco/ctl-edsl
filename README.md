# CTL - Computational Tree Logic Interpreter

An interpreter for Computational Tree Logic (CTL) with model checking capabilities, written in Haskell.

## Dependencies and Compilation

### Prerequisites

Before building the interpreter, you need to install Stack (Haskell build tool):

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

### System Dependencies

Install the Graphviz C library (required for PDF export):

```bash
sudo apt-get update
sudo apt-get install -y graphviz libgraphviz-dev
```

### Haskell Dependencies

Install the Haskell Graphviz bindings:

```bash
cabal update
cabal install graphviz --lib
```

### Building the Project

Setup and compile the interpreter:

```bash
stack setup    # Downloads and installs GHC (Haskell compiler)
stack build    # Compiles the interpreter
```

## Running the Interpreter

After building, you can run the interpreter in three different modes:

### Evaluation Mode (default)

Executes the entire program, including definitions, model checking, and exports:

```bash
stack exec -- ctl --eval test/test.ctl
# or
stack exec -- ctl -e test/test.ctl
```

### Type Check Mode

Only validates types without executing model checking or exports:

```bash
stack exec -- ctl --typecheck test/test.ctl
# or
stack exec -- ctl -t test/test.ctl
```

### Interactive Mode

Starts a REPL (Read-Eval-Print Loop) for interactive execution:

```bash
stack exec -- ctl --interactive
# or
stack exec -- ctl -i

# Load files on startup (only definitions are loaded)
stack exec -- ctl -i test/test.ctl
```

**Important:** In interactive mode, when you load files using `-i` or `:load`:
- `define` statements are processed and stored
- Model checking (`|=`) and `export` statements are **NOT** evaluated automatically
- You must run them manually in the REPL

## Language Syntax

### Identifier Naming Rules

CTL uses three distinct naming conventions to differentiate identifiers:

| Type | Convention | Examples | Usage |
|------|------------|----------|-------|
| **Nodes** | Start with `_` | `_start`, `_room1`, `_000` | States in transition systems |
| **Atoms** | Lowercase first letter | `p`, `q`, `entrada`, `win` | Atomic propositions  |
| **Variables** | Uppercase first letter | `Counter`, `Phi`, `MyModel` | Named definitions (formulas, models, etc.) |

### CTL Operators

#### Logical Operators

| Operator | ASCII | UTF-8 | Example |
|----------|-------|-------|---------|
| Negation | `!` | `¬` | `!p` or `¬p` |
| Conjunction | `&&` | `∧` | `p && q` or `p ∧ q` |
| Disjunction | `\|\|` | `∨` | `p \|\| q` or `p ∨ q` |
| Implication | `->` | `→` | `p -> q` or `p → q` |
| True | `T` | `⊤` | `T` or `⊤` |
| False | `F` | `⊥` | `F` or `⊥` |

#### CTL Temporal Operators

CTL uses **path quantifiers** combined with **state predicates**:

| Operator | ASCII | UTF-8 | Meaning |
|----------|-------|-------|---------|
| All Next | `A () p` | `∀○ p` | p holds in all next states |
| Exists Next | `E () p` | `∃○ p` | p holds in at least one next state |
| Inevitable | `A <> p` | `∀◇ p` | p eventually holds in all paths |
| Possible | `E <> p` | `∃◇ p` | p eventually holds in some path |
| Invariant | `A [] p` | `∀□ p` | p always holds in all paths |
| Invariant for some trace | `E [] p` | `∃□ p` | p always holds in some path |
| All Until | `A [p U q]` | `∀[p U q]` | p holds until q in all paths |
| Exists Until | `E [p U q]` | `∃[p U q]` | p holds until q in some path |

#### Examples

```javascript
// Bad states are never reached
define Safety :: Formula = A [] !bad
define Safety2 :: Formula = A□ ¬bad

// Good states are eventually reached
define Liveness :: Formula = A <> good
define Liveness2 :: Formula = A◇ good

// Every request eventually gets a response
define Response :: Formula = A [] (request -> A <> response)

// It's possible to reach the goal
define Reachable :: Formula = E <> goal
```

### Model Definition

Models represent transition systems with states, transitions, and labeling:

```javascript
// Define labeling function: which atoms are true in each state
define MyLabels :: Labels = {
    _state0 <= { prop1, prop2 }     // prop1 and prop2 are true in _state0
    _state1 <= { prop1 }            // only prop1 is true in _state1
    _state2 <= { }                  // no propositions are true in _state2
}

// Define transition relation: edges between states
define MyNodes :: Nodes = {
    (_state0) => { _state1, _state2 }   // Initial state (in parentheses)
    _state1 => { _state2 }              // _state1 can transition to _state2
    _state2 => { _state2 }              // _state2 has a self-loop
}

// Combine nodes and labels into a model
define MyModel :: Model = <MyNodes, MyLabels>
```

**Important Notes:**

1. **Initial State:** The initial states are indicated by wrapping them in parentheses: `(_state0)`
2. **Merging:** If you define the same node multiple times, the results are merged:

```javascript
// Labels are merged
define L :: Labels = {
    _s0 <= { p }
    _s0 <= { q }        // Result: _s0 has both p and q
}

// Transitions are merged
define N :: Nodes = {
    (_s0) => { _s1 }
    _s0 => { _s2 }      // Result: _s0 can go to both _s1 and _s2
}
```

### Model Checking

CTL supports three types of model checking operations:

#### Global Model Checking

Check if a formula holds from the initial state(s):

```javascript
MyModel |= A [] p        
```

#### Local Model Checking

Check if a formula holds from a specific state:

```javascript
MyModel, _state |= E <> q  
```

#### Satisfiability Checking

Check if a formula is satisfiable and generate a model:

```javascript
|= Phi as result_name      
```

### Export to PDF

Export models as visual graphs in PDF format:

```javascript
export MyModel as mymodel_graph
```

This generates a PDF file with a graphical representation of the transition system.

## Examples - Maze

```javascript
define MazeLabels :: Labels = {
    _start <= { entrance }
    _room1 <= { treasure }
    _room2 <= { key }
    _trap  <= { danger }
    _exit  <= { victory }
}

define MazeNodes :: Nodes = {
    (_start) => { _room1, _room2 }
    _room1   => { _trap, _exit }
    _room2   => { _room1 }
    _trap    => { _trap }              // Inescapable trap
    _exit    => { _exit }              // Terminal state
}

define Maze :: Model = <MazeNodes, MazeLabels>

// Model checking
Maze |= E <> victory                   // Can we win? YES
Maze |= A <> victory                   // Do all paths lead to victory? NO (trap exists)
Maze, _trap |= A [] danger             // Once trapped, always trapped? YES
Maze |= E [entrance U victory]         // Can we go from entrance to victory? YES

// Export visualization
export Maze as maze_graph
```

## Interactive Commands

When running in interactive mode (REPL), you can use these commands:

| Command | Description |
|---------|-------------|
| `:load <file>` or `:l <file>` | Load and process a CTL file (only definitions) |
| `:reload` or `:r` | Reload the last loaded file |
| `:browse` or `:b` | Display all definitions in the global scope |
| `:type <var>` or `:t <var>` | Show the type of a defined variable |
| `:show <var>` or `:s <var>` | Show the value of a definition |
| `:help` or `:h` | Display list of available commands |
| `:quit` or `:q` | Exit the interpreter |
---
