/-
 - Created in 2023 by Gaëtan Serré
-/

inductive Pipe where
  | Ground : Pipe
  | V : Pipe
  | H : Pipe
  | NE : Pipe
  | NW : Pipe
  | SW : Pipe
  | SE : Pipe
  | Start : Pipe

namespace Pipe

def toString (p : Pipe) : String :=
  match p with
  | Ground => "."
  | V => "|"
  | H => "-"
  | NE => "L"
  | NW => "J"
  | SW => "7"
  | SE => "F"
  | Start => "S"

instance : ToString Pipe where
  toString := toString

def toNat (p : Pipe) : Nat :=
  match p with
  | Ground => 0
  | V => 1
  | H => 2
  | NE => 3
  | NW => 4
  | SW => 5
  | SE => 6
  | Start => 7

instance : BEq Pipe where
  beq p1 p2 := p1.toNat == p2.toNat

instance : Inhabited Pipe where
  default := Pipe.Ground

end Pipe

instance : ToString (Array (Array Pipe)) where
  toString a := a.foldl (fun s e ↦ s ++ (toString e) ++ "\n") ""

def PipeOfChar (c : Char) :=
  open Pipe in
  match c with
  | '.' => Ground
  | '|' => V
  | '-' => H
  | 'L' => NE
  | 'J' => NW
  | '7' => SW
  | 'F' => SE
  | 'S' => Start
  | _   => panic! s!"Unrecognized {c} char."

def ArrayPipeOfString (s : String) : Array Pipe :=
  s.foldl (fun a c ↦ a ++ #[PipeOfChar c]) #[]

inductive Dir where
  | N : Dir
  | S : Dir
  | E : Dir
  | W : Dir

namespace Dir

def toString (d : Dir) : String :=
  match d with
  | N => "N"
  | S => "S"
  | E => "E"
  | W => "W"

instance : ToString Dir where
  toString := toString

def toNat (d : Dir) : Nat :=
  match d with
  | N => 0
  | S => 1
  | E => 2
  | W => 3

instance : BEq Dir where
  beq d1 d2 := d1.toNat == d2.toNat

instance : Inhabited Dir where
  default := N

end Dir

def getNewDir (d : Dir) (p : Pipe) : Option Dir :=
  let get_bended_dir (d1 : Dir) (d2 : Dir) (d3 : Dir) (d4 : Dir) :=
    if d == d1 then some d2
    else if d == d3 then some d4
    else none

  open Pipe in
  match p with
  | Ground => none
  | V => if d == Dir.N || d == Dir.S then some d else none
  | H => if d == Dir.E || d == Dir.W then some d else none
  | NE => get_bended_dir Dir.S Dir.E Dir.W Dir.N
  | NW => get_bended_dir Dir.S Dir.W Dir.E Dir.N
  | SE => get_bended_dir Dir.N Dir.E Dir.W Dir.S
  | SW => get_bended_dir Dir.N Dir.W Dir.E Dir.S
  | Start => none

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

def AddDirToCoor (c : Coord) (d : Dir) : Coord :=
  open Dir in
  match d with
  | N => {i := c.1 - 1, j := c.j}
  | S => {i := c.1 + 1, j := c.j}
  | E => {i := c.1, j := c.j + 1}
  | W => {i := c.1, j := c.j - 1}

instance : HAdd Coord Dir Coord where
  hAdd := AddDirToCoor

instance : Inhabited Coord where
  default := {i := 0, j := 0}

namespace Array

def getC {α : Type u} [Inhabited α] (a : Array (Array α)) (c : Coord) : α :=
  (a.get! (c.i)).get! c.j

def setC {α : Type u} [Inhabited α] (a : Array (Array α)) (c : Coord) (v : α) : Array (Array α) :=
  a.set! c.i ((a.get! c.i).set! c.j v)

end Array
