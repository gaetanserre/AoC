
/-
 - Created in 2023 by Gaëtan Serré
-/

namespace List
/--
Iterate a function _f_ over a List.
-/
def iter {α : Type u} (l : List α) (f : α -> IO Unit) : IO Unit := do
  match l with
  | [] => pure ()
  | hd :: tl =>
    f hd
    iter tl f
end List

namespace Array
/--
Iterate a function _f_ over an Array.
-/
def iter {α : Type u} [Inhabited α] (a : Array α) (f : α -> IO Unit) : IO Unit := do
  for i in (List.range a.size) do
    f (a.get! i)
end Array


/--
Read a file
-/

def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let fileExists ← filename.pathExists
  if not fileExists then
    let stderr ← IO.getStderr
    stderr.putStrLn s!"File not found: {filename}"
    pure none
  else
    let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle handle))

partial def get_lines (stream : IO.FS.Stream) (l : List String) : IO (List String) := do
  let line ← stream.getLine
  if line.isEmpty then
    pure l.reverse
  else
    get_lines stream (line :: l)

/--
Remove '\n' character from a string.
-/
def remove_endline (s : String) : String :=
  s.foldl (fun a c ↦ if c != '\n' then a ++ c.toString else a) ""

/--
Transforms a String such that "45  6 12 5" to a list Nat.
-/
def list_string_to_list_nat (l : List String) : List Nat :=
  let rec aux (l : List String) :=
    match l with
    | [] => []
    | hd :: tl =>
      let s_n := hd.toNat?
      match s_n with
      | none => aux tl
      | some n => n :: aux tl
  aux l

/--
Separate the data of a card to an array containing the list of winning numbers and the list of actual numbers.
-/
def split_numbers (s : String) : Array (List Nat) :=
  let ls := (s.split (fun c ↦ c == '|')).toArray
  let (l1, l2) := (ls.get! 0, ls.get! 1)
  #[list_string_to_list_nat (l1.split (fun c ↦ c == ' ')), list_string_to_list_nat (l2.split (fun c ↦ c == ' '))]

/--
Check how many number in _numbers_ are winning ones.
-/
def check_match_numbers (winning : List Nat) (numbers : List Nat) : Nat :=
  let rec aux (numbers : List Nat) (count : Nat) :=
    match numbers with
    | [] => count
    | hd :: tl =>
      if winning.contains hd then aux tl (count + 1)
      else aux tl count
  aux numbers 0

/-
Part one
-/
def sum_points (l : List Nat) :=
  l.foldl (fun s e ↦ s + (if e == 0 then 0 else Nat.pow 2 (e - 1))) 0

/--
Transforms the lines of the data file to the list of number of winning numbers per game.
-/
def format_cards (lines : List String) : List Nat :=
  -- Remove game tag
 let no_game_tag := lines.map (fun l ↦ ((remove_endline l).dropWhile (fun c ↦ c != ':')).extract (String.Pos.mk 2) (l.endPos))
 -- Split winning | numbers and convert to integer
 let format_numbers := no_game_tag.foldr (fun s l ↦ (split_numbers s) :: l) []
 -- Check number of matches
 format_numbers.foldr (fun aln l ↦ (check_match_numbers (aln.get! 0) (aln.get! 1)) :: l) []

/-
Part two
-/

/--
Compute the total number of each cards. The array nb_copies contained the number of each card. The list _cards_ contains each unique card. For each card _c_ in _cards_, iterate through the winning number of _c_ and, for each card it should copy, add the number of copies of _c_ in nb_copies.
-/
def add_copies (nb_copies : Array Nat) (cards : List Nat) : Array Nat :=
  let rec aux (cards : List Nat) (idx : Nat) (nb_copies : Array Nat) :=
    match cards with
    | [] => nb_copies
    | nb :: tl =>
      let cards_to_copy := (List.range nb).foldr (fun n l ↦ if idx + (n + 1) >= nb_copies.size then l else (n + 1) :: l) []
      let new_copies := cards_to_copy.foldl (
        fun a nb =>
          a.set! (idx + nb) (a.get! (idx + nb) + (a.get! idx) )
      ) nb_copies
      aux tl (idx + 1) new_copies
  aux cards 0 nb_copies

/--
Constructs an array of size _n_ filled with ones.
-/
partial def mk_one_array (n : Nat) :=
  let rec aux (i : Nat) (res : List Nat) :=
    if i >= n then res
    else aux (i + 1) (1 :: res)
  Array.mk (aux 0 [])

/--
Sum the total number of copies.
-/
def sum_copies (l : List Nat) :=
  l.foldl (fun s e ↦ s + e) 0

def print_results (lines : List String) : IO Unit := do
  let cards := format_cards lines
  let nb_copies := mk_one_array (lines.length)
  let final_copies := add_copies nb_copies cards

  IO.println s!"Part one: {sum_points cards} Part two: {sum_copies final_copies.data}"

def main : IO Unit := do
let stream ← fileStream "data.txt"
  match stream with
  | none =>
    pure ()
  | some stream =>
    let lines := get_lines stream []
    lines >>= print_results
