/-
 - Created in 2023 by Gaëtan Serré
-/

import Lean.Data.HashMap

namespace String

def geti (s : String) (i : Nat) : Char :=
  s.get! (Pos.mk i)

def extracti (s : String) (start_i : Nat) (end_i : Nat) : String :=
  s.extract (Pos.mk start_i) (Pos.mk end_i)

def endIdx (s : String) : Nat :=
  s.endPos.byteIdx

def replace_first (s : String) (c : Char) : String :=
  c.toString ++ s.extracti 1 s.endIdx

end String

namespace List

def sum {α : Type u} [OfNat α 0] [HAdd α α α] (l : List α) : α :=
  l.foldl (fun s e ↦ s + e) 0

partial def repeat_ {α : Type u} (l : List α) (n : Nat) : List α :=
  let rec aux (ll : List α) (n : Nat) :=
    if n == 1 then ll
    else aux (l ++ ll) (n - 1)
  aux l n

end List

def join (l : List String) (c : String) : String :=
  let s  := l.foldl (fun res s ↦ res ++ c ++ s) ""
  s.extracti 1 s.endIdx


def test {α : Type u} {β : Type v} [BEq α] [Hashable α] : Lean.HashMap α B := {}
