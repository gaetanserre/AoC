/-
 - Created in 2023 by Gaëtan Serré
-/

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

partial def string_to_array (s : String) : Array String :=
  let res := #[]
  let rec aux s idx a :=
    if idx == s.length then a
    else
      let s_i := s.get! (String.Pos.mk idx)
      if s_i == '\n' then aux s (idx + 1) a
      else aux s (idx + 1) (a.push s_i.toString)
  aux s 0 res

def list_string_to_array_array (lines : List String) : Array (Array String) :=
  let lines_array := lines.foldr (fun s l ↦ (string_to_array s) :: l) []
  lines_array.toArray

def remove_empty_string (l : List String) : List String :=
  l.foldr (fun s l => if s == "" then l else s :: l) []

def list_string_to_nat (l : List String) : List Nat :=
  l.foldr (fun s l => s.toNat! :: l) []

/--
Given an Array of String _s_ and a idx, look for the biggest number that contains _s[idx]_ (if _s[idx]_ is an integer). Returns the number as well as the number of digits located on the right of _s[idx]_.
-/
partial def get_number_from_string_idx (s : Array String) (idx : Nat) : (String × Nat) :=
  let rec aux_left (i : Nat) (res : String) :=
    if i == 0 then
      let s_0 := s.get! 0
        match s_0.toNat? with
        | some _ => s_0 ++ res
        | none => res
    else
      let s_i := s.get! i
      match s_i.toNat? with
      | some _ => aux_left (i - 1) (s_i ++ res)
      | none => res

  let rec aux_right (i : Nat) (res : String) :=
    if i == s.size then res
    else
      let s_i := s.get! i
      match s_i.toNat? with
      | some _ => aux_right (i + 1) (res ++ s_i)
      | none => res

  let s_i := s.get! idx
  match s_i.toNat? with
  | none => ("", 0)
  | some _ =>
    let left_nb := aux_left (idx - 1) ""
    let right_nb := aux_right (idx + 1) ""
    (left_nb ++ s_i ++ right_nb, right_nb.length)

/--
Given an Array of String _s_ and a symbol idx, look for numbers on the left and right of the symbol.
-/
def get_number_horizontal (line : Array String) (symbol_idx : Nat) : List Nat :=
  let left_nb := (get_number_from_string_idx line (symbol_idx - 1)).1
  let right_nb := (get_number_from_string_idx line (symbol_idx + 1)).1
  list_string_to_nat (remove_empty_string [left_nb, right_nb])

/--
Given an Array of String _s_ and a symbol idx, look for numbers on the top/bot left/right diagonal and top/bot vertical of the symbol. Uses some "optimization" and does not look for numbers that have been found already (e.g. if the top left number overflows by one digit on the right, it means that this numbers "contains" the number located at the top vertical and the top right number (if it exists)).
-/
def get_number_diagonal_and_vertical (lines : Array (Array String)) (symbol_idx : Nat) (no_top : Bool) (no_bot : Bool) :=
  let aux (line_idx : Nat) :=
    let left := (get_number_from_string_idx (lines.get! line_idx) (symbol_idx - 1))
    if left.2 == 0 then
      let mid := (get_number_from_string_idx (lines.get! line_idx) symbol_idx)
      if mid.1 == "" then
        let right := (get_number_from_string_idx (lines.get! line_idx) (symbol_idx + 1))
        [left.1, mid.1, right.1]
      else [left.1, mid.1]
    else [left.1]

  if no_bot && no_top then []
  else if no_top then list_string_to_nat (remove_empty_string (aux 1))
  else if no_bot then list_string_to_nat (remove_empty_string (aux 0))
  else list_string_to_nat (remove_empty_string ((aux 0) ++ (aux 1)))

/--
Given the top/mid/bot lines and a symbol idx, returns the list of all numbers next to the symbol.
-/
def get_number (lines : Array (Array String)) (symbol_idx : Nat) (no_top : Bool) (no_bot : Bool) :=
  (get_number_horizontal (lines.get! 1) symbol_idx)
  ++
  (get_number_diagonal_and_vertical #[lines.get! 0, lines.get! 2] symbol_idx no_top no_bot)

/-
Part one
-/

/--
Given all the lines, the actual line idx and a symbol idx, select the lines to look for numbers (take care of the first and last line of the file) and returns all the numbers next to the symbol.
-/
def select_lines_and_get_numbers (lines : Array (Array String)) (line_idx : Nat) (symbol_idx : Nat) :=
  if line_idx == 0 && line_idx == lines.size - 1 then
    let lines_around := #[lines.get! line_idx, lines.get! line_idx, lines.get! line_idx]
    get_number lines_around symbol_idx true true
  else if line_idx == 0 then
    let lines_around := #[lines.get! 0, lines.get! 0, lines.get! 1]
    get_number lines_around symbol_idx true false
  else if line_idx == lines.size - 1 then
    let lines_around := #[lines.get! (line_idx - 1), lines.get! line_idx, lines.get! line_idx]
    get_number lines_around symbol_idx false true
  else
    let lines_around := #[lines.get! (line_idx - 1), lines.get! line_idx, lines.get! (line_idx + 1)]
    get_number lines_around symbol_idx false false

/--
Given all lines and a line idx, returns all the numbers associated to each symbol located on the actual line.
-/
partial def get_numbers_line (lines : Array (Array String)) (line_idx : Nat) : List Nat :=
  let line := lines.get! line_idx
  let rec aux (i : Nat) (res : List Nat) :=
    if i == line.size then res
    else
      let line_i := line.get! i
      if line_i == "." then aux (i + 1) res
      else
        match (line.get! i).toNat? with
        | some _ => aux (i + 1) res
        | none =>
          let numbers := select_lines_and_get_numbers lines line_idx i
          aux (i + 1) (res ++ numbers)
  aux 0 []

/-
Part two
-/

/--
Given all lines and a line idx, for each symbol '*' located on the line, recover the associated numbers and multiply them. Returns a list of all the multiplication results.
-/
partial def get_numbers_line_part2 (lines : Array (Array String)) (line_idx : Nat) : List Nat :=
  let line := lines.get! line_idx
  let rec aux (i : Nat) (res : List Nat) :=
    if i == line.size then res
    else
      let line_i := line.get! i
      if line_i == "*" then
        match (line.get! i).toNat? with
        | some _ => aux (i + 1) res
        | none =>
          let numbers := select_lines_and_get_numbers lines line_idx i
          match numbers with
          | [_] => aux (i + 1) res
          | _   => aux (i + 1) (res ++ [numbers.foldl (fun m e ↦ m*e) 1])
      else aux (i + 1) res
  aux 0 []

/-
Common
-/

/--
Given all lines and a function on how to collect numbers, returns all meaningful numbers.
-/
partial def get_numbers (lines : Array (Array String)) (get_numbers_line : Array (Array String) → Nat → List Nat)  : List Nat :=
  let rec aux (i : Nat) (res : List Nat) :=
    if i == lines.size then res
    else aux (i + 1) ((get_numbers_line lines i) ++ res)
  aux 0 []

/--
Given all lines and a function on how to collect numbers, sums all meaningful numbers.
-/
def sum_numbers (lines : Array (Array String)) (get_numbers_line : Array (Array String) → Nat → List Nat) : Nat :=
  (get_numbers lines get_numbers_line).foldl (fun s e ↦ s + e) 0

def print_results (lines : List String) : IO Unit := do
  let array_lines := list_string_to_array_array lines
  let sum_p1 := sum_numbers array_lines get_numbers_line
  let sum_p2 := sum_numbers array_lines get_numbers_line_part2
  IO.println s!"Part one: {sum_p1} Part two: {sum_p2}"

def main : IO Unit := do
  let stream ← fileStream "data.txt"
  match stream with
  | none =>
    pure ()
  | some stream =>
    let lines := get_lines stream []
    lines >>= print_results
