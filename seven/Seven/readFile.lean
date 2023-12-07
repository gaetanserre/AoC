/-
 - Created in 2023 by Gaëtan Serré
-/

/--
Reads a file
-/

def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let fileExists ← filename.pathExists
  if not fileExists then
    let stderr ← IO.getStderr
    stderr.putStrLn s!"File not found: {filename}"
    pure none
  else
    let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle handle))

partial def get_lines (stream : IO.FS.Stream) (l : List String) : IO (List String) := do
  let line ← stream.getLine
  if line.isEmpty then
    pure l.reverse
  else
    get_lines stream (line :: l)

/--
Removes '\n' character from a string.
-/
def remove_endline (s : String) : String :=
  s.foldl (fun a c ↦ if c != '\n' then a ++ c.toString else a) ""
