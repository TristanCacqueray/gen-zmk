import Lake
open Lake DSL

package «gen-zmk» where

require batteries from
    git "https://github.com/leanprover-community/batteries" @ "v4.9.0"

lean_lib «GenZmk» where

@[default_target]
lean_exe «demo» {
  root := `Demo
}

lean_exe «qmk» {
  root := `Qmk
}
