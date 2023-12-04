
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
Remove space and '\n' characters from a string.
-/
def remove_space_endline (s : String) : String :=
  s.foldl (fun a c ↦ if c != ' ' && c != '\n' then a ++ c.toString else a) ""

/--
Recover the number of a specific string: "n[red|green|blue]".
Returns the following array: a.set! (0 if red, 1 if green, 2 if blue) n.
-/
def get_number_color (s : String) (a : Array Nat) : Array Nat :=
  let rec aux (s : List Char) res :=
    match s with
    | [] => #[]
    | c :: ts =>
      let c_str := c.toString
      match c_str.toNat? with
      | none =>
        match c with
        | 'r' =>
          a.set! 0 res.toNat!
        | 'g' =>
          a.set! 1 res.toNat!
        | 'b' =>
          a.set! 2 res.toNat!
        | _   => #[]
      | some _ => aux ts (res ++ c_str)

  aux s.toList ""

/--
Takes a specific string: "nred,mgreen,kblue" and returns #[n, m, k].
-/
def remove_color_name (s : String) : Array Nat :=
  let rec aux (l : List String) (a : Array Nat) :=
    match l with
    | [] => a
    | hd :: tl =>
      aux tl (get_number_color hd a)
  aux (s.split (fun c ↦ c == ',')) #[0, 0, 0]

/--
Format the string containing the games. Take a list of string: ["Game 1: n red, m green, k blue; ...", ..., "Game i: n red, m green, k blue; ..."] and returns of list of lists of arrays: [ [#[n, m, k], #[n, m, k], ...,], ..., [#[n, m, k], #[n, m, k], ...,] ].
-/
def format_games (lines : List String) : List (List (Array Nat)) :=
  let no_game_lines := lines.map (fun l ↦ ((remove_space_endline l).dropWhile (fun c ↦ c != ':')).extract (String.Pos.mk 1) (l.endPos))
  let lines_no_separator := no_game_lines.foldr (fun l a ↦ l.split (fun c ↦ c == ';') :: a) []

  let rec aux (l : List (List String)) (a : List (List (Array Nat))) :=
    match l with
    | [] => a.reverse
    | hd :: tl =>
      aux tl ((hd.foldr (fun s a ↦ (remove_color_name s) :: a) []) :: a)

  aux lines_no_separator []

/-
Part one
-/

/--
Check if an array such that #[n, m, k] is valid w.r.t. the rules conditions.
-/
def check_array (a : Array Nat) : Bool := a.get! 0 <= 12 && a.get! 1 <= 13 && a.get! 2 <= 14

/--
Check if a game (represented by a list of arrays) is valid by checking all arrays until one is not valid (or each is valid).
-/
def check_game (g : List (Array Nat)) : Bool :=
  match g with
  | [] => true
  | hd :: tl =>
    if check_array hd then check_game tl
    else false


/--
Iterates through the games (represented by an array of game) and count the number of possible games w.r.t. the rules conditions.
-/
partial def sum_id_possible_games (games : Array (List (Array Nat))) : Nat :=
  let rec aux a s i :=
    if i == a.size then s
    else if check_game (a.get! i) then aux a (s + (i + 1)) (i + 1)
    else aux a s (i + 1)
  aux games 0 0

/-
Part two
-/

/--
Takes a game and returns the maximum number of cubes of the color represented by _idx_ over the game.
-/
def get_max_color_idx (game : List (Array Nat)) (idx : Nat) : Nat :=
  let color := game.foldl (fun l a => a.get! idx :: l) []
  color.foldl (fun m e => if e > m then e else m) 0

/--
Takes a game and returns an array containing the maximum number of cubes of each color represented over the game.
-/
partial def get_max_color (game : List (Array Nat)) : Array Nat :=
  let rec aux (i : Nat) res :=
    match i with
    | 3 => res
    | _ => aux (i + 1) (res.set! i (get_max_color_idx game i))
  aux 0 #[0, 0, 0]

/--
Take a game and returns its power (the product of the array returned by *get_max_color*).
-/
def get_power_game (game : List (Array Nat)) : Nat :=
  (get_max_color game).foldl (fun p e => p*e) 1

/--
Sum the powers of each game in _games_.
-/
def add_power_games (games : Array (List (Array Nat))) : Nat :=
  games.foldl (fun s g => s + (get_power_game g)) 0

def print_results (lines : List String) : IO Unit := do
  let games := (format_games lines).toArray
  let sum := sum_id_possible_games games
  let power := add_power_games games

  IO.println s!"Sum: {sum} Power: {power}"

def main : IO Unit := do
  let stream ← fileStream "data.txt"
  match stream with
  | none =>
    pure ()
  | some stream =>
    let lines := get_lines stream []
    lines >>= print_results
