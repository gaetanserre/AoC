/-
 - Created in 2023 by Gaëtan Serré
-/

/--
Reads a file
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
Removes '\n' character from a string.
-/
def remove_endline (s : String) : String :=
  s.foldl (fun a c ↦ if c != '\n' then a ++ c.toString else a) ""


variable {α : Type u}

def divide (l : List α) : List (List α) :=
  l.foldl (fun r e ↦ [e] :: r) []

partial def merge (l1 : List α) (l2 : List α) (compare : α → α → Bool) :=
  let rec aux l1 l2 (res : List α) :=
    match (l1, l2) with
    | ([], []) => res.reverse
    | (hd :: tl, []) => aux tl [] (hd :: res)
    | ([], hd :: tl) => aux [] tl (hd :: res)
    | (hd1 :: tl1, hd2 :: tl2) =>
      if compare hd1 hd2 then
        aux tl1 l2 (hd1 :: res)
      else
        aux l1 tl2 (hd2 :: res)
  aux l1 l2 []

def merge_list (l : List (List α)) (compare : α → α → Bool) : List (List α) :=
  let rec aux l res :=
    match l with
    | [] => res
    | hd::[] => hd :: res
    | hd::(hd2::tl) =>
      aux tl ((merge hd hd2 compare) :: res)
  aux l []

partial def merge_sort (l : List α) (compare : α → α → Bool) : List α :=
  let rec aux l :=
    match l with
    | [] => []
    | hd::[] => hd
    | _ => aux (merge_list l compare)
  aux (divide l)

inductive Card where
  | C (n : Nat) : Card
  | T : Card
  | J : Card
  | Q : Card
  | K : Card
  | A : Card

namespace Card

def natOfCard (c : Card) : Nat :=
  match c with
  | C n  => n
  | T    => 10
  | J    => 11
  | Q    => 12
  | K    => 13
  | A    => 14

def StringOfCard (c : Card) : String :=
  match c with
  | C n  => s!"{n}"
  | T    => "T"
  | J    => "J"
  | Q    => "Q"
  | K    => "K"
  | A    => "A"

instance : LE Card where
  le := fun c1 c2 ↦ Card.natOfCard c1 <= Card.natOfCard c2

theorem le_iff {c1 c2 : Card} : c1 ≤ c2 ↔ Card.natOfCard c1 <= Card.natOfCard c2 := Iff.rfl

instance : DecidableRel ((. ≤ .) : Card → Card → Prop) := fun _ _ ↦ decidable_of_decidable_of_iff Card.le_iff.symm

instance : BEq Card where
  beq := fun c1 c2 ↦ Card.natOfCard c1 == Card.natOfCard c2

instance : ToString (List Card) where
  toString l := s!"{l.foldr (fun c l => (Card.StringOfCard c) :: l) []}"

end Card

def stringToCard (s : String) : List Card :=
  s.foldr (
    fun c l ↦
      let c_n := (c.toString).toNat?
      let card :=
        match c_n with
        | some n => Card.C n
        | none =>
          match c with
          | 'T' => Card.T
          | 'J' => Card.J
          | 'Q' => Card.Q
          | 'K' => Card.K
          | 'A' => Card.A
          | _   => Card.A
      card :: l
  ) ([] : List Card)

inductive HandT where
  | Five (lc : List Card)  : HandT
  | Four (lc : List Card)  : HandT
  | Full (lc : List Card)  : HandT
  | Three (lc : List Card) : HandT
  | TwoP (lc : List Card)  : HandT
  | P (lc : List Card)     : HandT
  | Card (lc : List Card)  : HandT

namespace HandT

def natOfHandT (h : HandT) : Nat :=
  match h with
  | Card _  => 1
  | P _     => 2
  | TwoP _  => 3
  | Three _ => 4
  | Full _  => 5
  | Four _  => 6
  | Five _  => 7

def getlistcards (h : HandT) :=
  match h with
  | Card lc  => lc
  | P lc     => lc
  | TwoP lc  => lc
  | Three lc => lc
  | Full lc  => lc
  | Four lc  => lc
  | Five lc  => lc

instance : LE HandT where
  le := fun h1 h2 ↦ HandT.natOfHandT h1 <= HandT.natOfHandT h2

theorem le_iff {h1 h2 : HandT} : h1 ≤ h2 ↔ HandT.natOfHandT h1 <= HandT.natOfHandT h2 := Iff.rfl

instance : DecidableRel ((. ≤ .) : HandT → HandT → Prop) := fun _ _ ↦ decidable_of_decidable_of_iff HandT.le_iff.symm

def HandtToString (h : HandT) : String :=
match h with
| Card _  => "Card"
| P _     => "Pair"
| TwoP _  => "Two Pairs"
| Three _ => "Brelan"
| Full _  => "Full"
| Four _  => "Square"
| Five _  => "Five"

instance : ToString (HandT) where
toString := HandtToString

end HandT

partial def compare (h1 h2 : HandT) : Bool :=
  if h1.natOfHandT < h2.natOfHandT then true
  else
    let rec aux (lc1 lc2 : List Card) :=
    match (lc1, lc2) with
    | ([], []) => true
    | (c1 :: tl1, c2 :: tl2) =>
      if c1.natOfCard > c2.natOfCard then false
      else aux tl1 tl2
    | _ => true
    aux (h1.getlistcards) (h2.getlistcards)

def cardsToHandT (lc : List Card) : HandT :=
  let rec aux lc_aux (removed : Array Card) (unique : Array Card) :=
    match lc_aux with
    | [] =>
      match unique.size with
      | 5 => HandT.Card lc
      | 4 => HandT.P lc
      | 3 =>
        match removed.size with
        | 2 => HandT.TwoP lc
        | 1 => HandT.Three lc
        | _ => HandT.Three lc
      | 2 =>
        match removed.size with
        | 2 => HandT.Full lc
        | 1 => HandT.Four lc
        | _ => HandT.Four lc
      | 1 => HandT.Five lc
      | _ => HandT.Five lc
    | c :: tl =>
      if unique.contains c then
        if removed.contains c then aux tl removed unique
        else aux tl (removed ++ #[c]) unique
      else aux tl removed (unique ++ #[c])
  aux lc #[] #[]

def parse_file (lines : List String) :=
  let rec aux (lines : List String) (games : List (HandT × Nat)) :=
    match lines with
    | [] => games
    | hd::tl =>
      let arr_s := ((remove_endline hd).split (fun c ↦ c == ' ')).toArray
      let (c_s, b_s) := (arr_s.get! 0, arr_s.get! 1)
      let hand := cardsToHandT (stringToCard c_s)
      aux tl ((hand, b_s.toNat!) :: games)
  aux lines []

def print_results (lines : List String) :=
  let games := parse_file lines
  IO.println games


def main : IO Unit := do
  let stream ← fileStream "data.txt"
    match stream with
    | none =>
      pure ()
    | some stream =>
      let lines := get_lines stream []
      lines >>= print_results
