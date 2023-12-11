/-
 - Created in 2023 by Gaëtan Serré
-/

import Eleven.Utils

/-
Used for starting part one. Useless with the code of part two.
-/

/-
partial def construct_blank_line (size : Nat) : Array String :=
  let rec aux (i : Nat) :=
    if i == size then []
    else "." :: aux (i + 1)
  (aux 0).toArray

def expand_lines (lines : Array (Array String)) (exp_l : List Nat) :=
  match exp_l with
  | [] => lines
  | l_i :: tl =>
    let lines := lines.insertAt! l_i (construct_blank_line ((lines.get! 0).size))
    expand_lines lines tl

partial def expand_columns (lines : Array (Array String)) (exp_c : List Nat) :=
  let rec aux (lines : Array (Array String)) (l_i : Nat) (c_i : Nat) :=
    if l_i == lines.size then lines
    else
      let n_line := (lines.get! l_i).insertAt! c_i "."
      aux (lines.set! l_i n_line) (l_i + 1) c_i

  match exp_c with
  | [] => lines
  | c_i :: tl =>
    expand_columns (aux lines 0 c_i) tl -/

/-
Common
-/

/--
Checks if a line contains only ".".
-/
def line_all_space (line : Array String) : Bool :=
  let rec aux (s : List String) :=
    match s with
    | [] => true
    | c :: tl =>
      if c != "." then false
      else aux tl
  aux line.data

/--
Returns the list of expandable lines.
-/
partial def get_expandable_lines (lines : Array (Array String)) : List Nat :=
  let rec aux (i : Nat) (res : List Nat) :=
    if i == lines.size then res
    else if line_all_space (lines.get! i) then aux (i + 1) (i :: res)
    else aux (i + 1) res
  aux 0 []

/--
Checks if the column _c_ contains only ".".
-/
partial def column_all_space (lines : Array (Array String)) (c : Nat) : Bool :=
  let rec aux (l_idx : Nat) :=
    if l_idx == lines.size then true
    else if (lines.get! l_idx).get! c != "." then false
    else aux (l_idx + 1)
  aux 0

/--
Returns the list of expandable columns.
-/
partial def get_expandable_columns (lines : Array (Array String)) : List Nat :=
  let rec aux (c_i : Nat) (res : List Nat) :=
    if c_i == (lines.get! 0).size then res
    else if column_all_space lines c_i then aux (c_i + 1) (c_i :: res)
    else aux (c_i + 1) res
  aux 0 []

/--
Returns the list of the coordinates of the galaxies in the lines.
-/
partial def get_coords_galaxies (lines : Array (Array String)) : List Coord :=
  let rec get_coords_line (line : List String) (l_i : Nat) (c_i : Nat) (res : List Coord) :=
    match line with
    | [] => res
    | hd :: tl =>
      if hd == "#" then get_coords_line tl l_i (c_i + 1) ({i := l_i, j := c_i} :: res)
      else get_coords_line tl l_i (c_i + 1) res

  let rec aux (l_i : Nat) (res : List Coord) :=
    if l_i == lines.size then res
    else
      aux (l_i + 1) ((get_coords_line (lines.get! l_i).data l_i 0 []) ++ res)
  aux 0 []

/--
Compute the Manhattan distance, which is the smallest distance between two galaxies where the only steps available are left, right, up and down.
-/
def manhattan_distance (c1 c2 : Coord) :=
  let (x1, y1) :=((c1.i : Int), (c1.j : Int))
  let (x2, y2) :=((c2.i : Int), (c2.j : Int))
  (x2 - x1).natAbs + (y2 - y1).natAbs

/--
Given a list a galaxies coordinates, compute the list of distance between all pairs (no symmetry). To achieve that, for any i-th galaxy in the list, it only has to compute the distance between in and all the n-th galaxies, where i < n.
-/
def get_distance_pairs (galaxies : List Coord) :=
  let rec aux (lg : List Coord) (res : List Nat) :=
    match lg with
    | [] => res
    | c :: tl =>
      let dist_pair := tl.foldl (
        fun l g ↦
          if g != c then manhattan_distance c g :: l
          else l
      ) []
      aux tl (dist_pair ++ res)
  aux galaxies []

/--
Given a list of expandable lines and a list galaxies coordinates, for each expandable line _l_, adds (n - 1) to the _i_ coordinate of each galaxy that is below _l_.
-/
def expand_x_coord (galaxies : List Coord) (exp_lines : List Nat) (n : Nat) : List Coord :=
  let rec aux (galaxies : List Coord) (exp_lines : List Nat) :=
    match exp_lines with
    | [] => galaxies
    | l_i :: tl =>
      let galaxies := galaxies.foldr (
        fun g l ↦
          if g.i > l_i then {i := g.i + n - 1, j := g.j} :: l
          else g :: l
      ) []
      aux galaxies tl
  aux galaxies exp_lines

/--
Given a list of expandable columns and a list galaxies coordinates, for each expandable column _c_, adds (n - 1) to the _j_ coordinate of each galaxy on the right of _c_.
-/
def expand_y_coord (galaxies : List Coord) (exp_columns : List Nat) (n : Nat) : List Coord :=
  let rec aux (galaxies : List Coord) (exp_columns : List Nat) :=
    match exp_columns with
    | [] => galaxies
    | c_i :: tl =>
      let galaxies := galaxies.foldr (
        fun g l ↦
          if g.j > c_i then {i := g.i, j := g.j + n - 1} :: l
          else g :: l
      ) []
      aux galaxies tl
  aux galaxies exp_columns

def sum_distances (distances : List Nat) :=
  distances.foldl (fun s d ↦ s + d) 0

/--
Transforms an array of String to an array of array of string. It just split each string to an array of its character (casted to string).
-/
def unfold_lines (lines : Array String) : Array (Array String) :=
  lines.foldl (
    fun a s ↦
      a ++ #[(s.toList.foldr (fun c l ↦ c.toString :: l) []).toArray]
  ) #[]

def print_results (lines : Array String) : IO Unit :=
  let lines := unfold_lines lines
  let exp_lines := get_expandable_lines lines
  let exp_columns := get_expandable_columns lines
  let galaxies := get_coords_galaxies lines
  let exp_galaxies := (expand_y_coord . exp_columns 2) <| (expand_x_coord . exp_lines 2) <| galaxies
  let s1 := sum_distances <| get_distance_pairs exp_galaxies

  let exp_galaxies_2 := (expand_y_coord . exp_columns 1000000) <| (expand_x_coord . exp_lines 1000000) <| galaxies
  let s2 := sum_distances <| get_distance_pairs exp_galaxies_2

  IO.println s!"Part one: {s1} Part two: {s2}"

def main : IO Unit :=
  IO.FS.lines "data.txt" >>= print_results
