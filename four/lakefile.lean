import Lake
open Lake DSL

package «four» where
  -- add package configuration options here

@[default_target]
lean_exe «four» where
  -- Enables the use of the Lean interpreter by the executable (e.g.,
  -- `runFrontend`) at the expense of increased binary size on Linux.
  -- Remove this line if you do not need such functionality.
  supportInterpreter := true
