/-
 - Created in 2023 by Gaëtan Serré
-/

/--
Decrements the time by one and increments the speed by one until speed*time > threshold.
When it's the case, the number of ways to win the race is the number of combinaisons (speed, time) minus two times the number of combinaisons that does not beat the record (as the set of combinaisons is symmetric).
-/
partial def compute_nb_ways (time : Nat) (threshold : Nat) : Nat :=
  let nb_possibilities := time + 1
  let rec aux (speed : Nat) (time : Nat) (count : Nat) :=
    if threshold < time * speed then nb_possibilities - (count * 2)
    else if count == nb_possibilities / 2 then 0
    else aux (speed + 1) (time - 1) (count + 1)
  aux 0 time 0

def compute_product :=
  let r1 := compute_nb_ways 54 446
  let r2 := compute_nb_ways 81 1292
  let r3 := compute_nb_ways 70 1035
  let r4 := compute_nb_ways 88 1007
  r1 * r2 * r3 * r4

def print_results :=
  let p_1 := compute_product
  -- Takes ~3 seconds to compute on a Macbook Pro M2.
  let p_2 := compute_nb_ways 54817088 446129210351007
  IO.println s!"Part one: {p_1} Part two: {p_2}"

def main : IO Unit :=
  print_results
