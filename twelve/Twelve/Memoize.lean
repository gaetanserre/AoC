import Lean.Data.HashMap

/-
Copyright (c) 2022 Gabriel Ebner. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Gabriel Ebner, E.W.Ayers, Gaëtan Serré
-/

/-!
# Fixpoint function with memoization

-/

private unsafe def memoFixImplObj (α : Type) (β : Type) [Hashable α] [BEq α] (f : (α → β) → (α → β)) (a : α) :
    β := unsafeBaseIO do
  let cache : IO.Ref (Lean.HashMap α β) ← ST.mkRef ∅
  let rec fix (a) := unsafeBaseIO do
    if let some b := (← cache.get).find? a then
      return b
    let b := f fix a
    cache.modify (·.insert a b)
    pure b
  pure $ fix a

private unsafe def memoFixImpl {α : Type} {β : Type} [Hashable α] [BEq α] [Nonempty β] :
    (f : (α → β) → (α → β)) → (a : α) → β :=
  unsafeCast (memoFixImplObj α β)

/-- Takes the fixpoint of `f` with caching of values that have been seen before.
Type α needs to implements hash and boolean equality.

This is useful for implementing deep recursive functions that depend on simple types, such as fibonacci.
-/
@[implemented_by memoFixImpl]
opaque memoFix {α : Type} {β : Type} [Hashable α] [BEq α] [Nonempty β] (f : (α → β) → (α → β)) : α → β
