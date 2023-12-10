/-
 - Created in 2023 by Gaëtan Serré
-/

import Ten.Types

/--
Returns an option containg the index of the starting pipe within _pipes_ or none if _pipes_ does not contain the starting pipe.
-/
partial def contains_start (pipes : Array Pipe) : Option Nat :=
  let rec aux (i : Nat) :=
    if i == pipes.size then none
    else if pipes.get! i == Pipe.Start then some i
    else aux (i + 1)
  aux 0

/--
Returns the starting coordinates in the matrix of pipes.
-/
partial def get_start_coord (pipes : Array (Array Pipe)) : Coord :=
  let rec aux (i : Nat) :=
    if i == pipes.size then panic! "Start not found"
    else
      match contains_start (pipes.get! i) with
      | some idx => {i := i, j := idx}
      | none => aux (i + 1)
  aux 0

/--
Given the starting coordinates, returns the list of possible directions.
-/
def get_possible_dir_from_start (start : Coord) (pipes : Array (Array Pipe)) : List Dir :=
  open Dir in
  let dirs := #[N, S, E, W]
  dirs.foldl (
    fun l d ↦
      match getNewDir d (pipes.getC (start + d)) with
      | none => l
      | some _ => d :: l
  ) []

/--
Given the starting coord, the first direction and the matrix of pipes, returns the list of coordinates of the loop's pipes.
-/
partial def get_loop (start : Coord) (dir : Dir) (pipes : Array (Array Pipe)) : List Coord :=
  let rec aux (c : Coord) (dir : Dir) (coords_path : List Coord) :=
    if c == start then coords_path
    else
      let new_dir := getNewDir dir (pipes.getC c)
      match new_dir with
      | some d =>
        aux (c + d) d (c :: coords_path)
      | none => panic! "blocked"
  aux (start + dir) dir [start]

def get_determinant (c1 c2 : Coord) : Int :=
  let get_int_coord (c : Coord) : Int × Int := (c.i, c.j)
  let (x1, y1) := get_int_coord c1
  let (x2, y2) := get_int_coord c2
  x1 * y2 - x2 * y1

/--
Computes area of the polygon drawed by the path using the Shoelace formula.
-/
def compute_area (coords_path : List Coord) : Nat :=
  let rec aux (l : List Coord) (prev : Coord) (res : Int) :=
    match l with
    | [] => Int.natAbs (res / 2)
    | c :: tl =>
      aux tl c (res + get_determinant prev c)
  let first := coords_path.head!
  aux (coords_path.tail! ++ [first]) first 0

def count_inside (coords_path : List Coord) : Nat :=
  let a := compute_area coords_path
  /-
    Area of _a_ means it contains _a_ squares of length 1.
    So the area of the polygon drawed by the path can be represented as a _a_ × 1 rectangle.
    We need to remove the number of 1 × 1 square that are represented by the edges.
    For each edge _e_ that is not a corner, if we draw a 1 × 1 square within the rectangle where one corner is _e_,  half of the square is on the edges and need to be removed. So we need to remove 1/2 * path.length squares.
    However, for each corner, their square is entirely contained in the squares of their neighbors. As only 1/4 of a 1 × 1 square drawed from a corner can be within the rectangle, we need to add 1/4 of square for each corner i.e. 1 square.
  -/
  a + 1 - coords_path.length / 2

def print_results (lines : Array String) : IO Unit :=
  let pipes := lines.foldl (fun a s ↦ a ++ #[ArrayPipeOfString s]) #[]
  let start := get_start_coord pipes
  let dir := (get_possible_dir_from_start start pipes).head!
  let p1 := (get_loop start dir pipes).length / 2

  let p2 := count_inside <| get_loop start dir pipes


  IO.println s!"Part one: {p1} Part two {p2}"

def main : IO Unit :=
  IO.FS.lines "data.txt" >>= print_results
