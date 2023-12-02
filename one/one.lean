
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

/-
Part one
-/

/--
Get the first digit of a string (written actual digit).
-/
def get_first_nat (s : List Char) : Option String :=
  match s with
  | [] => none
  | hd :: tl =>
    let hd_str := hd.toString
    match hd_str.toNat? with
    | none => get_first_nat tl
    | some _ => some hd_str

/--
Recover the first and last digit of each string of a list and sum the composition.
-/
def sum_first_last_nat (l : List String) : IO Nat := do
  let rec aux (l : List String) (s : Nat) :=
    match l with
    | [] => s
    | hd :: tl =>
      let first_nat := match get_first_nat hd.toList with
        | none => ""
        | some n => n
      let last_nat := match get_first_nat hd.toList.reverse with
        | none => ""
        | some n => n
      aux tl (s + (first_nat ++ last_nat).toNat!)
  pure (aux l 0)

/-
Part two
-/

/-
List of written digits, reverted written digits and equivalent in number.
-/
def digits := ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
def rev_digits := digits.map (fun s => s.toList.reverse.asString)
def equiv_digits := ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

/--
  Check if a string starts with one of the digit provided in _digits_. If so, returns an option containing the equivalent written number.
-/
def str_start_digits (s : String) (digits : List String) : Option String :=
  let rec aux (s : String) (digits : List String) (equiv_digits : List String) :=
    match digits, equiv_digits with
    | hs :: ts, hn :: tn =>
      if s.startsWith hs then
        some hn
      else
        aux s ts tn
    | _, _ => none
  aux s digits equiv_digits

/--
Recover the first digit of a string, either written using number or letters.
-/
def get_first_nat2 (s : List Char) (digits : List String) : Option String :=
  match s with
  | [] => none
  | hd :: tl =>
    let hd_str := hd.toString
    match hd_str.toNat? with
    | some _ => some hd_str
    | none =>
      match str_start_digits s.asString digits with
        | some s => some s
        | none => get_first_nat2 tl digits

/--
Recover the first and last digit of each string of a list and sum the composition.
-/
def sum_first_last_nat2 (l : List String) : IO Nat := do
    let rec aux (l : List String) (s : Nat) :=
      match l with
      | [] => s
      | hd :: tl =>
        let first_nat := match get_first_nat2 hd.toList digits with
          | none => ""
          | some n => n
        let last_nat := match get_first_nat2 hd.toList.reverse rev_digits with
          | none => ""
          | some n => n
        aux tl (s + (first_nat ++ last_nat).toNat!)
    pure (aux l 0)

def print_results (lines : List String) : IO Unit := do
  let sum_1 := sum_first_last_nat lines
  let sum_2 := sum_first_last_nat2 lines
  sum_1 >>= fun (s : Nat) ↦ IO.print s!"First part: {s}"
  sum_2 >>= fun (s : Nat) ↦ IO.println s!" Second part: {s}"

def main : IO Unit := do
  let stream ← fileStream "data.txt"
  match stream with
  | none =>
    pure ()
  | some stream =>
    let lines := get_lines stream []
    lines >>= print_results
