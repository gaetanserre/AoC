/-
 - Created in 2023 by Gaëtan Serré
-/

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
