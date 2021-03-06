module Sudoku
type Sudoku

val make : int list list -> Sudoku
val listify : Sudoku -> int list list

val print : Sudoku -> unit

val isValid : Sudoku -> bool
val isDone : Sudoku -> bool
