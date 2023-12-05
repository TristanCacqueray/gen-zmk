import Lake
open Lake DSL

package «gen-zmk» where

require std from git "https://github.com/leanprover/std4" @ "v4.3.0"

lean_lib «GenZmk» where

@[default_target]
lean_exe «demo» {
  root := `Demo
}
