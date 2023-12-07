import Lake
open Lake DSL

package «seven» {
  -- add any package configuration options here
}

lean_lib «Seven» {
  -- add any library configuration options here
}


@[default_target]
lean_exe «seven_b» where
  root := `seven_b
