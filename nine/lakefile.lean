import Lake
open Lake DSL

package «nine» where
  -- add package configuration options here

lean_lib «Nine» where
  -- add library configuration options here

@[default_target]
lean_exe «nine» where
  root := `Nine
