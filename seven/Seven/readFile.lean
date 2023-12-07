/-
 - Created in 2023 by Gaëtan Serré
-/

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
