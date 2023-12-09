/-
- Created in 2023 by Gaëtan Serré
-/

/--
Transforms a list of String to a list of Int.
-/
def parse_line (line : String) : List Int :=
  let rec aux (l : List String) :=
    match l with
    | [] => []
    | hd :: tl =>
      hd.toInt! :: aux tl
  aux (line.split (λ c ↦ c == ' '))

/--
Given a list of Int, computes the list of the differences between the i-th and the (i + 1)-th elements e.g. : [1, 2, 4] ↦ [1, 2].
-/
def create_list_difference (l : List Int) : List Int :=
  ((l.tail!).foldl (λ mem i => (i, (i - mem.1) :: mem.2)) (l.head!, [])).2.reverse

/--
Checks if a list of Int is filled with zeros.
-/
def is_all_zero (l : List Int) : Bool :=
  match l with
  | [] => true
  | hd :: tl =>
    if hd == 0 then is_all_zero tl
    else false

/--
Iteratively computes within difference of _l_ until it's filled with zeros and returns all the steps.
-/
partial def difference_until_zero (l : List Int) : List (List Int) :=
  let rec aux (l : List Int) (res : List (List Int)) :=
    if is_all_zero l then l :: res
    else
      aux (create_list_difference l) (l :: res)
  aux l []

/--
Computes the predicted values. If part one (_minus = false_), select the last element of each difference and add the previous predicted value. If part two (_minus = true_), select the first and retrieve the previous predicted value.
-/
def fill_placeholder (diff : List (List Int)) (minus : Bool) : List Int :=
  let rec aux (l : List (List Int)) (res : List Int) (prev : Int) :=
    match l with
    | [] => res
    | hd :: tl =>
      let placeholder := if minus then hd.head! else (hd.reverse).head!
      let filled := if minus then placeholder - prev else placeholder + prev
      aux tl (filled :: res) filled
  aux diff.tail! [0] 0

/--
Given the list of all lines, compute every placeholders and adds the ones corresponding to the original lines together.
-/
def sum_all_placeholder (lines : List String) (minus : Bool) : Int :=
  let rec aux (l : List String) (res : Int) :=
    match l with
    | [] => res
    | s :: tl =>
      let res := res + ((fill_placeholder . minus) <| difference_until_zero <| parse_line s).head!
      aux tl res
  aux lines 0

def print_results (lines : Array String) :=
  let p1 := sum_all_placeholder lines.data false
  let p2 := sum_all_placeholder lines.data true
  IO.println s!"Part one: {p1} Part two: {p2}"

def main : IO Unit :=
  IO.FS.lines "data.txt" >>= print_results
