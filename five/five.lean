
/-
 - Created in 2023 by Gaëtan Serré
-/

namespace List
/--
Iterates a function _f_ over a List.
-/
def iter {α : Type u} (l : List α) (f : α -> IO Unit) : IO Unit := do
  match l with
  | [] => pure ()
  | hd :: tl =>
    f hd
    iter tl f
end List

namespace Array
/--
Iterates a function _f_ over an Array.
-/
def iter {α : Type u} [Inhabited α] (a : Array α) (f : α -> IO Unit) : IO Unit := do
  for i in (List.range a.size) do
    f (a.get! i)
end Array


/--
Reads a file
-/
def get_lines (filename : System.FilePath) : IO (List String) :=
  (IO.FS.lines filename) >>= (fun a ↦ pure (a.data))

/--
Removes '\n' character from a string.
-/
def remove_endline (s : String) : String :=
  s.foldl (fun a c ↦ if c != '\n' then a ++ c.toString else a) ""

/--
Transforms a String such that "45  6 12 5" to a list Nat.
-/
def list_string_to_list_nat (l : List String) : List Nat :=
  let rec aux (l : List String) :=
    match l with
    | [] => []
    | hd :: tl =>
      let s_n := hd.toNat?
      match s_n with
      | none => aux tl
      | some n => n :: aux tl
  aux l

/--
Checks if a source Nat is contained in a range (Array of 3 Nats).
-/
def in_range (source : Nat) (range : Array Nat) : Bool :=
  range.get! 1 <= source && source < range.get! 1 + range.get! 2

/-
Part one
-/

/--
Given a string of form "seeds: 79 14 55 13", returns [79, 14, 55, 13].
-/
def construct_seeds (seeds_str : String) : List Nat :=
  let seeds_str := seeds_str.extract (String.Pos.mk 6) seeds_str.endPos
  list_string_to_list_nat (seeds_str.split (fun c ↦ c == ' '))

/--
Given all lines, a start point and an end point, returns the list of ranges (Array #[dest, source, length]) contained between the start and end points.
-/
def construct_ranges (lines : List String) (start_str : String) (stop_str : String) : List (Array Nat) :=
  let rec aux (lines : List String) (flag : Bool) :=
    match lines with
    | [] => []
    | s :: tl =>
      if s == stop_str then []
      else if s == start_str then aux tl true
      else if !flag then aux tl flag
      else
        let range := (list_string_to_list_nat (s.split (fun c ↦ c == ' '))).toArray
        range :: aux tl flag
  aux lines false

/--
Given a source, returns the corresponding dest number w.r.t. the ranges. It checks if the source is contained in any range. If so, returns the corresponding dest, otherwise, returns the source itself.
-/
def get_dest_number (source : Nat) (ranges : List (Array Nat)) : Nat :=
  let rec aux (ranges : List (Array Nat)) : Nat :=
    match ranges with
    | [] => source
    | range :: tl =>
      if in_range source range then
        let diff := source - (range.get! 1)
        range.get! 0 + diff
      else aux tl
  aux ranges

/--
Construct all ranges.
-/
def construct_all_ranges (lines : List String) : Array (List (Array Nat)) :=
  #[
    construct_ranges lines "seed-to-soil map:" "soil-to-fertilizer map:",
    construct_ranges lines "soil-to-fertilizer map:" "fertilizer-to-water map:",
    construct_ranges lines "fertilizer-to-water map:" "water-to-light map:",
    construct_ranges lines "water-to-light map:" "light-to-temperature map:",
    construct_ranges lines "light-to-temperature map:" "temperature-to-humidity map:",
    construct_ranges lines "temperature-to-humidity map:" "humidity-to-location map:",
    construct_ranges lines "humidity-to-location map:" ""
  ]

/--
Given a seed, go through each range to find the corresponding location.
-/
partial def get_location (seed : Nat) (ranges : Array (List (Array Nat))) : Nat :=
  let rec aux (source : Nat) (idx : Nat) :=
    if idx == ranges.size then source
    else
      aux (get_dest_number source (ranges.get! idx)) (idx + 1)
  aux seed 0

/--
Returns the list of locations of each seed.
-/
def get_locations (lines : List String) (construct_seeds : String → List Nat) : List Nat :=
  let removed_spaces := (lines.foldr (fun s l ↦
      let s_ := remove_endline s
      if s_ == "" then l
      else s_ :: l) [])
  let seeds := construct_seeds (removed_spaces.get! 0)
  let ranges := construct_all_ranges removed_spaces
  seeds.foldr (fun s l ↦ (get_location s ranges) :: l) []

/--
Gets the minimum location.
-/
def get_min_location (locations : List Nat) :=
  locations.foldl (fun mn l ↦ if l < mn then l else mn) locations.head!

/-
Part two
-/

/--
Transforms a list of seed of form [79, 14, 55, 13] to a list of ranges of form [#[79, 14], #[55, 13]].
-/
def construct_seed_ranges (seeds : List Nat) : List (Array Nat) :=
  let rec aux (seeds : List Nat) :=
    match seeds with
    | [] => []
    | hd :: (hd2 :: tl) =>
      #[hd, hd2] :: aux tl
    | _ => []
  aux seeds

/--
Maps a list of ranges to another list of ranges given the mapping represented by _ranges_.
-/
partial def transform_range (range : Array Nat) (ranges : List (Array Nat)) : List (Array Nat) :=
  let rec aux (range : Array Nat) (aux_ranges : List (Array Nat)) :=
    match aux_ranges with
    | [] => [range]
    | hr :: tl =>
      let start := range.get! 0
      let end_p := start + (range.get! 1) - 1
      if in_range start hr then
        if in_range end_p hr then
          let start_dest := get_dest_number start [hr]
          let length := get_dest_number end_p [hr] - start_dest + 1
          [#[start_dest, length]]
        else
          let start_dest := get_dest_number start [hr]
          let length := ((hr.get! 0) + (hr.get! 2)) - start_dest

          let last_s_in_range := (hr.get! 1) + (hr.get! 2) - 1
          let range_length := (range.get! 1) - length
          let range := #[last_s_in_range + 1, range_length]
          #[get_dest_number start [hr], length] :: aux range ranges
      else aux range tl
  aux range ranges

/--
Transforms a seed range to a location range by passing through all the mappings.
-/
partial def get_location_range (seed_range : Array Nat) (ranges : Array (List (Array Nat))) : List (Array Nat) :=
  let rec aux (source_range : List (Array Nat)) (idx : Nat) :=
    if idx == ranges.size then source_range
    else

      let source_range := source_range.foldl (fun l r ↦ l ++ transform_range r (ranges.get! idx)) []
      aux source_range (idx + 1)
  aux [seed_range] 0

/--
Get all the location ranges.
-/
def get_locations_range (seed_ranges : List (Array Nat)) (ranges : Array (List (Array Nat))) : List (Array Nat) :=
  seed_ranges.foldr (fun s l ↦ (get_location_range s ranges) ++ l) []

/--
Given a list of ranges, returns the minimum start point.
-/
def get_min_location_ranges (ranges : List (Array Nat)) :=
  ranges.foldl (fun mn r => if r.get! 0 < mn then r.get! 0 else mn) ((ranges.head!).get! 0)

def print_results (lines : List String) : IO Unit := do

  let removed_spaces := (lines.foldr (fun s l ↦
      let s_ := remove_endline s
      if s_ == "" then l
      else s_ :: l) [])
  let ranges := construct_all_ranges removed_spaces
  let seeds := construct_seeds (removed_spaces.get! 0)
  let locations := seeds.foldr (fun s l ↦ (get_location s ranges) :: l) []
  let min_location := get_min_location locations

  let seed_ranges := construct_seed_ranges seeds
  let location_ranges := get_locations_range seed_ranges ranges
  let min_location_ranges := get_min_location_ranges location_ranges
  IO.println s!"Part one {min_location} Part two {min_location_ranges}"

def main : IO Unit := do
  let lines := get_lines "data.txt"
  lines >>= print_results
