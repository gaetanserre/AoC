import Lake
open Lake DSL

package «eight» where
  -- add package configuration options here

lean_lib «Eight» {
  -- add any library configuration options here
}

@[default_target]
lean_exe «eight» where
  root := `Eight
