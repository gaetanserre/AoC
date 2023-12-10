import Lake
open Lake DSL

package «ten» where
  -- add package configuration options here

lean_lib «Ten» where
  -- add library configuration options here

@[default_target]
lean_exe «ten» where
  root := `Ten
