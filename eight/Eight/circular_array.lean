/-
 - Created in 2023 by Gaëtan Serré
-/

structure C_Array (α : Type u) where
  data : Array α
  n : Nat := data.size

def C_Array.get! {α : Type u} [Inhabited α] (a : C_Array α) (i : Nat) : α :=
  a.data.get! (i % a.n)

instance {α : Type u} [ToString (Array α)] : ToString (C_Array α) where
  toString a := ToString.toString a.data
