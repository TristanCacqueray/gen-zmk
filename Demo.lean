
import «GenZmk».Keymap
import «GenZmk».Diagram

def main := do
  let cfg <- Config.readme
  IO.FS.writeFile "../zmk-config/config/gen-zmk.dtsi" (cfg.renderZMK)
  IO.FS.writeFile "../zmk-config/moonwalker.svg" (renderDiagram cfg)
  let meta <- System.FilePath.metadata "../zmk-config/moonwalker.svg"
  IO.println $ s!"Updated! {meta.modified.sec}"
