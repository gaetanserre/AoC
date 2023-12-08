/-
 - Created in 2023 by Gaëtan Serré
-/

import Seven.merge_sort
import Seven.readFile

inductive Card where
  | C (n : Nat) : Card
  | T : Card
  | J : Card
  | Q : Card
  | K : Card
  | A : Card

namespace Card

def StringOfCard (c : Card) : String :=
  match c with
  | C n  => s!"{n}"
  | T    => "T"
  | J    => "J"
  | Q    => "Q"
  | K    => "K"
  | A    => "A"

instance : Inhabited Card where
  default := A

instance : ToString (List Card) where
  toString l := s!"{l.foldr (fun c l => (Card.StringOfCard c) :: l) []}"

instance : ToString (Array Card) where
  toString a := s!"{a.foldr (fun c l => #[Card.StringOfCard c] ++ l) #[]}"

end Card

def CardOfstring (s : String) : List Card :=
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

/--
Deduces from the number of unique cards and removed cards which hand it is.
-/
def get_hand_from_removed_and_unique (lc : List Card) (removed : Array Card) (unique : Array Card) : HandT :=
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

variable [BEq Card] [LE Card] [DecidableRel ((. <= .) : Card → Card → Prop)] [LE HandT] [DecidableRel ((. <= .) : HandT → HandT → Prop)]

/--
Transforms a list of cards to a hand. Keep track of unique cards and removed cards to call *get_hand_from_removed_and_unique*.
-/
def HandTOfcard (lc : List Card) : HandT :=
  let rec aux lc_aux (removed : Array Card) (unique : Array Card) :=
    match lc_aux with
    | [] => get_hand_from_removed_and_unique lc removed unique
    | c :: tl =>
      if unique.contains c then
        if removed.contains c then aux tl removed unique
        else aux tl (removed ++ #[c]) unique
      else aux tl removed (unique ++ #[c])
  aux lc #[] #[]

namespace Array
partial def get_max_idx {α : Type u} [LE α] [DecidableRel ((. <= .) : α → α → Prop)] [Inhabited α] (a : Array α) :=
  let rec aux (i : Nat) (m_e : α) (m_i : Nat) :=
    if i == a.size then m_i
    else
      let e := a.get! i
      if e >= m_e then aux (i + 1) e i
      else aux (i + 1) m_e m_i
  aux 0 (a.get! 0) 0
end Array

/--
Transforms a list of cards to a hand. When encounter a Joker, duplicates the card that have been removed the most. If no card has been removed, select a random one to copy. Keep track of unique cards and removed cards to call *get_hand_from_removed_and_unique*.
-/
def HandTOfcard2 (lc : List Card) :=
  let rec aux lc_aux (removed : Array Card) (removed_c : Array Nat) (unique : Array Card) :=
    match lc_aux with
    | [] => get_hand_from_removed_and_unique lc removed unique
    | c :: tl =>
      if c == Card.J then
        if removed.size == 0 then
          aux tl (removed ++ #[unique.get! 0]) (removed_c ++ #[1]) unique
        else
          let max_idx := removed_c.get_max_idx
          let removed_c := removed_c.set! max_idx ((removed_c.get! max_idx) + 1)
          aux tl removed removed_c unique
      else
        if unique.contains c then
          match removed.getIdx? c with
          | some i =>
            let removed_c := removed_c.set! i ((removed_c.get! i) + 1)
            aux tl removed removed_c unique
          | none => aux tl (removed ++ #[c]) (removed_c ++ #[1]) unique
        else aux tl removed removed_c (unique ++ #[c])
  let merged_lc := merge_sort lc (. >= .)
  if merged_lc.head! == Card.J then HandT.Five lc
  else aux merged_lc #[] #[] #[]

def sort_games (games : List (HandT × Nat)) : List (HandT × Nat) :=
  merge_sort games (fun g1 g2 ↦ g1.1 <= g2.1)

def get_winnings (games : List (HandT × Nat)) : Nat :=
  let rec aux (games : List (HandT × Nat)) (res : Nat) (count : Nat) :=
    match games with
    | [] => res
    | hd::tl => aux tl (res + hd.2 * count) (count + 1)
  aux games 0 1

/- def print_results (lines : List String) :=
  let games := parse_file lines
  /- let h1 := HandTOfcard2 (CardOfstring "JJA87")
  let h2 := HandTOfcard2 (CardOfstring "K3J8Q")
  let b : Bool := h1 >= h2
  IO.println s!"{h1} {h2} {b}" -/
  IO.println (get_winnings games)

def main : IO Unit := do
  let stream ← fileStream "data.txt"
    match stream with
    | none =>
      pure ()
    | some stream =>
      let lines := get_lines stream []
      lines >>= print_results -/
