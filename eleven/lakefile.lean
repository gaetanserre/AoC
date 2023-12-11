import Lake
open Lake DSL

package «eleven» where
  -- add package configuration options here

lean_lib «Eleven» where
  -- add library configuration options here

@[default_target]
lean_exe «eleven» where
  root := `ELeven
