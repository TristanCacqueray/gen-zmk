import «GenZmk».Config
import Std.Data.List.Basic

def width := 100
def height := 60
def comboSquare := 18
def comboColor := "#eee"
def kpad := 3
def hpad := 10
def H := height
def Board.height (board : Board) : Nat :=
  board.dim.snd * (H + kpad)

def KeyPos.coord (startY : Nat) (pos : KeyPos) :=
  let thumbspad := if pos.hand == HandPosition.RightHand then hpad else 0
  let x := 1 + kpad + thumbspad + pos.col * (width + kpad)
  let y := startY + kpad + pos.row * (height + kpad)
  (x, y)

def mkElem (name : String) (attrs : List (String × String)) (child : String) : String :=
  s!"<{name} {attrsStr}>{childStr}</{name}>"
 where
  childStr := if child == "" then "" else s!"\n {child}\n"
  attrsStr := String.intercalate " " (mkAttr <$> attrs)
  mkAttr attr := attr.fst ++ "=\"" ++ attr.snd ++ "\""

def svg (dim : (Nat × Nat)) (child : String) : String :=
  mkElem "svg" [("width", s!"{dim.fst}px"), ("height", s!"{dim.snd}px"),
                ("viewBox", s!"0 0 {dim.fst} {dim.snd}"), ns, sp] child
 where
  sp := ("xml:space", "preserve")
  ns := ("xmlns", "http://www.w3.org/2000/svg")

def rect (fill : String) (dim : (Nat × Nat)) (pos : (Nat × Nat)) : String :=
  mkElem "rect" [("style", style), ("width", dim.fst.repr), ("height", dim.snd.repr),
                 ("x", pos.fst.repr), ("y", pos.snd.repr)] ""
 where
  style := s!"fill:{fill}" ++ if fill != comboColor then ";stroke:#c3c3c3" else ""

def text (sz : Nat) (color : Option String) (anchor : String) (pos : (Nat × Nat)) (txt : String) : String :=
  mkElem "text" [("style", style), ("dominant-baseline", "central"), ("text-anchor", anchor),
                 ("x", pos.fst.repr), ("y", pos.snd.repr)] txt
 where
  colorStyle := match color with | none => "" | some c => s!";fill:{c}"
  style := s!"font-size:{sz}px;font-family:'monospace'" ++ colorStyle

def renderLayer (config : Config) (startY : Nat) (layer : Layer) : String :=
   "\n".intercalate (layoutTitle :: keys)
 where
  layoutTitle := match layer.overlay with
    | none => text 24 layerColor "middle" (150, startY + config.system.board.height - 25) layer.name
    | some (_, Overlay.TopRight) =>
        text 20 layerColor "star" (180, startY + config.system.board.height - 35) layer.name
    | some (_, Overlay.TopLeft) =>
        text 20 layerColor "end" (120, startY + config.system.board.height - 35) layer.name

  labelColor := fun (label : String) =>
    let lastLabel := (label.split (. == ' ')).reverse.head!
    match config.layerPos (String.toLower lastLabel) with
    | some opos => do
        if opos == 0 then none else some (config.theme.layersColor.get! opos)
    | none => none
  board := config.system.board

  layerColor := config.theme.layersColor.get? =<< config.layerPos layer.name
  renderBinding (pos : Nat × Nat) (binding : Binding) : String :=
    let renderTopRight (label: String) :=
      text 12 layerColor "end" (pos.fst + width - kpad, pos.snd + 20) label
    let renderTopLeft (label: String) :=
      text 12 layerColor "start" (pos.fst - kpad, pos.snd + 20) label
    let renderCenter (label: String) := match layer.overlay with
      | some (_, Overlay.TopRight) => renderTopRight label
      | some (_, Overlay.TopLeft) => renderTopLeft label
      | none => text 12 (labelColor label) "middle" (pos.fst + width / 2 - 2, pos.snd + height / 2 + 3) label
    let renderTop (label: String) :=
      text 12 (labelColor label) "middle" (pos.fst + width / 2 - 2, pos.snd + 20) label
    let renderUnder (label: String) :=
      text 10 (labelColor label) "middle" (pos.fst + width / 2 - 2, pos.snd + height / 2 + 15) label
    let renderHold : Binding -> String
      | Binding.key c => renderUnder c.toUpper
      | Binding.mac _name arg => renderUnder arg
      | _ => ""
    match binding with
    | Binding.unicode c => match c with
        | Sum.inl c => renderCenter c.toString
        | Sum.inr (c, _) => renderCenter c.toString
    | Binding.key c => match c with
        | "," => renderCenter s!"{c} &lt;"
        | "." => renderCenter s!"{c} &gt;"
        | "/" => renderCenter s!"{c} ?"
        | ";" => renderCenter s!"{c} :"
        | "&" => renderCenter "&amp;"
        | "rpar" => renderCenter ")"
        | "lpar" => renderCenter "("
        | _ => renderCenter (c.toUpper)
    | Binding.hold'tap _ hold tap => renderBinding pos tap ++ renderHold hold
    | Binding.mac name arg => match name with
        | "shift" => renderCenter s!"⇧ {arg.toUpper}"
        | "alt" => renderCenter s!"M-{arg.toUpper}"
        | "gui" => renderCenter s!"🐧 {arg.toUpper}"
        | "to" => renderCenter s!"👉 {arg.toUpper}"
        | "mo" => renderCenter s!"👇 {arg.toUpper}"
        | "sl" => renderCenter s!"🤏 {arg.toUpper}"
        | "vol" => renderCenter s!"🔉 {arg.toUpper}"
        | "pg" => match arg with
            | "up" => renderCenter s!"⇞"
            | _ => renderCenter s!"⇟"
        | "br" => match arg with
            | "up" => renderCenter s!"🔆"
            | _ => renderCenter s!"🔅"
        | "mouse" => renderCenter s!"🕹 {arg.toUpper}"
        | "click" => renderCenter s!"🖟 {arg.toUpper}"
        | "scroll" => renderCenter s!"scroll-{arg}"
        | "out" => renderCenter s!"out-{arg}"
        | "bt" => renderCenter s!"bt-{arg}"
        | _ => renderCenter arg.toUpper
    | Binding.na => ""

  keys := layer.bindings.mapIdx renderKey
  renderKey := fun pos binding => match board.keyPos pos with
    | none => s!"unknown key{pos}!"
    | some pos =>
        let coord := pos.coord startY
        let label := "\n    ".intercalate [renderBinding coord binding]
        "\n".intercalate [rect "none" (width, height) coord, label]

def renderCombos (config : Config) (startY : Nat) (combos : List Combo) : String :=
   "\n".intercalate (combos.map renderCombo)
 where
  renderLabel := fun coord label =>
      text 12 none "middle" (coord.fst + 6, coord.snd + (comboSquare / 2)) label
  renderCombo := fun combo =>
    let label := match combo.binding with
      | Binding.key v => match v with
         | "'\"" => "\""
         | "&" => "&amp;"
         | "lpar" => "("
         | "rpar" => ")"
         | _ => v
      | Binding.na => "_"
      | _ => "??"
    let dim := (comboSquare, comboSquare)
    match combo.keys.map config.system.board.keyPos with
    | [some p1, some p2] =>
        if p1.hand == p2.hand && p1.row == p2.row then
          let (x, y) := p1.coord startY
          let coord := (x + width + (kpad / 2) - (comboSquare / 2),
                        y + (height / 2) - (comboSquare / 2))
          "\n".intercalate [rect comboColor dim coord, renderLabel coord label]
        else if p1.hand == p2.hand && p1.col == p2.col then
          let (x, y) := p1.coord startY
          let coord := (x + (width / 2) - (comboSquare / 2),
                        y + height + (kpad / 2) - comboSquare / 2)
          "\n".intercalate [rect comboColor dim coord, renderLabel coord label]
        else
          ""
    | _ => "" -- TODO

def renderDiagram (config : Config) : String :=
  let boards := config.layers.reverse.map drawLayer
  let combos := config.combos.map drawCombos
  svg svgDim ("".intercalate $ rect "#fff" (svgDim.fst, svgDim.snd - 1) (0, 0) :: title :: url :: (boards ++ combos))
 where
  boardDim := config.system.board.dim
  titleHeight := 42
  svgDim := (boardDim.fst * (width + kpad) + hpad + 2 * kpad,
             titleHeight + drawnLayers.length * (boardDim.snd * (height + kpad) + bpad) + 2 * kpad)
  -- todo: load from config
  title := text 23 none "end" (svgDim.fst / 2, 20) "Moonwalker"
  url := text 23 none "start" (svgDim.fst / 2 - 15, 20) "TristanCacqueray/zmk-config"

  drawCombos := fun combos =>
    let layer := combos.layers.head!
    let layerPos := match drawnLayers.toArray.findIdx? (. == layer) with
      | some i => i
      | none => 0
    let baseY := (titleHeight + kpad + layerPos * (boardDim.snd * (height + kpad) + bpad))
    renderCombos config baseY combos.combos
  drawnLayers : List String := config.layers.reverse.filterMap (fun layer => match layer.overlay with
    | none => some layer.name
    | _ => none
  )
  bpad := 30
  drawLayer := fun layer =>
    let layerPosName := match layer.overlay with
      | some (name, _) => name
      | none => layer.name
    let layerPos := match drawnLayers.toArray.findIdx? (. == layerPosName) with
      | some i => i
      | none => 0
    let baseY := (titleHeight + kpad + layerPos * (boardDim.snd * (height + kpad) + bpad))
    renderLayer config baseY layer
