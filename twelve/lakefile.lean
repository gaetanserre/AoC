import Lake
open Lake DSL

package «twelve» where
  -- add package configuration options here

lean_lib «Twelve» where
  -- add library configuration options here

@[default_target]
lean_exe «twelve» where
  root := `Twelve
