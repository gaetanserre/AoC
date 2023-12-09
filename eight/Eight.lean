/-
 - Created in 2023 by Gaëtan Serré
-/

import Lean.Data.HashMap

import Eight.circular_array

open Nat

inductive Dir where
  | L : Dir
  | R : Dir

namespace Dir

def StringOfDir (d : Dir) :=
  match d with
  | L => "L"
  | R => "R"

instance : ToString (Dir) where
  toString := StringOfDir

instance : Inhabited Dir where
  default := L

def DirOfChar (c : Char) :=
  match c with
  | 'L' => L
  | 'R' => R
  | _  => panic! "assert false"

end Dir

inductive Tree where
  | Leaf : Tree
  | Node (name : String) (sl : String) (sr : String) : Tree

namespace Tree

def getStr (t : Tree) : String :=
  match t with
  | Leaf => "ZZZ"
  | Node n sl sr => s!"{n} = ({sl}, {sr})"

instance : Inhabited Tree where
  default := Leaf

instance : BEq Tree where
  beq t1 t2 :=
    match (t1, t2) with
    | (Leaf, Leaf) => true
    | (Node n i1 i2, Node m j1 j2) => n == m && i1 == j1 && i2 == j2
    | _ => false

instance : ToString Tree where
  toString := getStr

instance : Hashable Tree where
  hash t := t.getStr.hash

end Tree

instance : ToString (Lean.HashMap String Tree) where
  toString h := (h.toList).foldr (fun e s ↦ s!"{e.1} {e.2} " ++ s) ""

/--
From the first line construct an array of directions.
-/
def DirsOfString (s : String) : Array Dir :=
  s.foldl (fun a c ↦ a ++ #[Dir.DirOfChar c]) #[]

/--
Constructs a hashmap node_name → Tree. Each Node is parametrized by two strings representing the name of their children. This representation allows cycles.
-/
def TreeEdgesOfLString (l : List String) : Lean.HashMap String Tree :=
  let rec aux (l : List String) (h : Lean.HashMap String Tree) :=
    match l with
    | [] => h
    | s :: tl =>
      let node_name := s.extract (String.Pos.mk 0) (String.Pos.mk 3)
      if node_name == "ZZZ" then aux tl (h.insert node_name Tree.Leaf)
      else
        let l_name := s.extract (String.Pos.mk 7) (String.Pos.mk 10)
        let r_name := s.extract (String.Pos.mk 12) (String.Pos.mk 15)
        aux tl ((h.insert node_name (Tree.Node node_name l_name r_name)))
  aux l {}

/-
Part one
-/

/--
Iterates until finding the arrival.
-/
partial def find_time_arrival (start : Tree) (dirs : C_Array Dir) (tree_edges : Lean.HashMap String Tree) :=
  let rec aux (t : Tree) (i : Nat) :=
    match t with
    | Tree.Leaf => i
    | Tree.Node _ sl sr =>
      match dirs.get! i with
      | Dir.L => aux (tree_edges.find! sl) (i + 1)
      | Dir.R => aux (tree_edges.find! sr) (i + 1)
  aux start 0

/-
Part two
-/

/--
Returns the list of Tree whose name ends with _c_.
-/
def getNodeEndsWith (tree_edges : Lean.HashMap String Tree) (c : String) : List Tree :=
  let rec aux (l : List (String × Tree)) (res : List Tree) :=
    match l with
    | [] => res
    | (s, t) :: tl =>
      if s.endsWith c then aux tl (t :: res)
      else aux tl res
  aux (tree_edges.toList) []

def getStartingNodes (tree_edges : Lean.HashMap String Tree) :=
  getNodeEndsWith tree_edges "A"

/--
Iterates until finding the arrival.
-/
partial def find_time_arrival_2 (start : Tree) (dirs : C_Array Dir) (tree_edges : Lean.HashMap String Tree) :=
  let rec aux (t : Tree) (i : Nat) :=
    match t with
    | Tree.Leaf => i
    | Tree.Node n sl sr =>
      if n.endsWith "Z" then i
      else
        match dirs.get! i with
        | Dir.L => aux (tree_edges.find! sl) (i + 1)
        | Dir.R => aux (tree_edges.find! sr) (i + 1)
  (aux start 0)

/--
Compute the Least Common Multiple of a list of Nat.
-/
def lcm (l : List Nat) :=
  l.foldl (fun r e ↦ r * (e / Nat.gcd r e)) 1

/--
Computes the time to arrives to a final Tree for each Tree in _starts_. Then computes the lcm of all the times. As, after arriving to a final Tree, one iteration, goes back to the first Tree after the start Tree, the way from each start Tree to the corresponding final Tree is a cycle. The lcm returns the minimum steps to do for all the cycles to end a the same time.
-/
partial def find_time_simul_arrival (starts : List Tree) (dirs : C_Array Dir) (tree_edges : Lean.HashMap String Tree) : Nat :=
  let rec aux (l : List Tree) (arrivals : List Nat) :=
    match l with
    | [] => arrivals
    | t :: tl => aux tl (find_time_arrival_2 t dirs tree_edges :: arrivals)
  lcm (aux starts [])


def print_results (lines : Array String) : IO Unit :=
  let dirs : C_Array Dir := {data := DirsOfString (lines.get! 0)}
  let nodes_ls := (lines.extract 2 (lines.size)).data
  let tree_edges := TreeEdgesOfLString nodes_ls

  let step_1 := find_time_arrival (tree_edges.find! "AAA") dirs tree_edges
  let starts := getStartingNodes tree_edges
  let step_2 := find_time_simul_arrival starts dirs tree_edges

  IO.println s!"Part one: {step_1} Part two: {step_2}"

def main : IO Unit :=
  IO.FS.lines "data.txt" >>= print_results
