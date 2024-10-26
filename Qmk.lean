import «GenZmk».Keymap
import «GenZmk».Diagram

def main := do
  let ecfg <- Config.loadFile "./moonpointer.gmk"
  match ecfg with
    | .ok cfg => do
      IO.FS.writeFile "../qmk-config/gen_qmk.h" (cfg.renderQMK)
      IO.FS.writeFile "../qmk-config/moonpointer.svg" (renderDiagram cfg)
      IO.println s!"Done!"
    | .error err => do
     IO.println s!"error: {err}"
     IO.Process.exit 1
