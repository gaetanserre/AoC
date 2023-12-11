/-
 - Created in 2023 by Gaëtan Serré
-/

instance : ToString (Array (Array String)) where
  toString a := a.foldl (
    fun res as ↦
    let s := as.foldl (fun res s ↦ res ++ s) ""
    res ++ s ++ "\n"
  ) ""

structure Coord where
  i : Nat
  j : Nat

namespace Coord

def toString (c : Coord) : String :=
  s!"({c.i}, {c.j})"

instance : ToString Coord where
  toString := toString

instance : BEq Coord where
  beq c1 c2 := c1.i == c2.i && c1.j == c2.j

def checkWithinArray {α : Type u} [Inhabited α] (c : Coord) (a : Array (Array α)) : Bool :=
  a.size > c.i && (a.get! 0).size > c.j

end Coord

namespace Array

def getC {α : Type u} [Inhabited α] (a : Array (Array α)) (c : Coord) : α :=
  (a.get! (c.i)).get! c.j

def setC {α : Type u} [Inhabited α] (a : Array (Array α)) (c : Coord) (v : α) : Array (Array α) :=
  a.set! c.i ((a.get! c.i).set! c.j v)

end Array
