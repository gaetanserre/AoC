import Seven.utils

namespace Card

def natOfCard (c : Card) : Nat :=
  match c with
  | C n  => n
  | T    => 10
  | J    => 11
  | Q    => 12
  | K    => 13
  | A    => 14

instance : LE Card where
  le := fun c1 c2 ↦ c1.natOfCard <= c2.natOfCard

theorem le_iff {c1 c2 : Card} : c1 ≤ c2 ↔ c1.natOfCard <= c2.natOfCard := Iff.rfl

instance : DecidableRel ((. ≤ .) : Card → Card → Prop) := fun _ _ ↦ decidable_of_decidable_of_iff Card.le_iff.symm

instance : BEq Card where
  beq := fun c1 c2 ↦ c1.natOfCard == c2.natOfCard

end Card

/--
Compare two hands by first checking the type and then each card of both hands.
-/
partial def compare (h1 h2 : HandT) : Bool :=
  if h1.natOfHandT < h2.natOfHandT then true
  else if h1.natOfHandT > h2.natOfHandT then false
  else
    let rec aux (lc1 lc2 : List Card) :=
    match (lc1, lc2) with
    | ([], []) => true
    | (c1 :: tl1, c2 :: tl2) =>
      if c1.natOfCard > c2.natOfCard then false
      else if c2.natOfCard > c1.natOfCard then true
      else aux tl1 tl2
    | _ => true
    aux (h1.getlistcards) (h2.getlistcards)

namespace HandT

instance : LE HandT where
  le := fun h1 h2 ↦ compare h1 h2

theorem le_iff {h1 h2 : HandT} : h1 ≤ h2 ↔ compare h1 h2 := Iff.rfl

instance : DecidableRel ((. ≤ .) : HandT → HandT → Prop) := fun _ _ ↦ decidable_of_decidable_of_iff HandT.le_iff.symm

end HandT

def parse_file (lines : List String) :=
  let rec aux (lines : List String) (games : List (HandT × Nat)) :=
    match lines with
    | [] => games
    | hd::tl =>
      let arr_s := ((remove_endline hd).split (fun c ↦ c == ' ')).toArray
      let (c_s, b_s) := (arr_s.get! 0, arr_s.get! 1)
      let hand := HandTOfcard (CardOfstring c_s)
      aux tl ((hand, b_s.toNat!) :: games)
  sort_games (aux lines [])

def print_results (lines : List String) :=
  let games := parse_file lines
  IO.println s!"Part one: {get_winnings games}"

def main : IO Unit := do
  let stream ← fileStream "data.txt"
    match stream with
    | none =>
      pure ()
    | some stream =>
      let lines := get_lines stream []
      lines >>= print_results
