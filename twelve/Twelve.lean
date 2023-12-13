/-
 - Created in 2023 by Gaëtan Serré
-/

import Twelve.Utils
import Twelve.Memoize

/--
Checks if the string begins with a valid group of size _n_. It automatically count '?' as '#' if needed.
-/
def check_group (s : String) (n : Nat) : Bool :=
  if s.length < n then false
  else
    let rec aux (l : List Char) (i : Nat) :=
      match l with
      | [] => true
      | c :: tl =>
       if i == n then c != '#'
       else if c == '.' then false
       else aux tl (i + 1)
    aux s.toList 0

/--
Computes the total number of possible arrangements of '?' characters for a given line. Its implements a Depth-first search and is deeply recursive. Will use memoization later.
Stops early if there is not enough characters to fill the requirements. Call itself if the first char is  '.' with the rest of the line. If the first char is a '#', checks if the beginning of the string can be a group of size _groups[0]_. If so, call itself with the rest of the line, otherwise returns 0. If the first character is a '?', call itself twice: one where '?' ← '.' and one where '?' ← '#', and sums the result.
-/
partial def get_arrangements (fix : (String × List Nat) → Nat) :=
  let aux (data : String × List Nat) : Nat :=
    let (line, groups) := data
    if groups.length == 0 then if line.contains '#' then 0 else 1
    else if line.length < groups.sum + groups.length - 1 then 0
    else if line.geti 0 == '.' then fix ((line.extract (String.Pos.mk 1) line.endPos), groups)
    else if line.geti 0 == '#' then
      if check_group line groups.head! then fix ((line.extract (String.Pos.mk (groups.head! + 1)) line.endPos), groups.tail!)
      else 0
    else fix ((line.replace_first '.'), groups) + fix ((line.replace_first '#'), groups)
  aux

/--
Formats one line into a string and a list of groups. Repeats each data _r_ times.
-/
def format_line (line : String) (r : Nat) : String × List Nat :=
  let splitted := (line.split (fun c ↦ c == ' ')).toArray
  let s := (join . "?") <| [splitted.get! 0].repeat_ r
  let groups := (((splitted.get! 1).split (fun c ↦ c == ',')).foldr (fun s l ↦ s.toNat! :: l) []).repeat_ r
  (s, groups)

/--
Creates a memoized version of *get_arrangement* and calls it on all lines.
Sums all the results.
-/
def solve_lines (lines : Array String) (r : Nat) : Nat :=
  let get_arrangements := memoFix get_arrangements
  (lines.foldl (
    fun l s ↦
    let (line, groups) := format_line s r
    get_arrangements (line, groups) :: l
  ) []).sum

def print_results (lines : Array String) : IO Unit :=
  IO.println s!"Part one: {solve_lines lines 1} Part two: {solve_lines lines 5}"

def main : IO Unit :=
  IO.FS.lines "data.txt" >>= print_results
