-- | This module transforms the Config into ZMK keymap
--

import «GenZmk».Config
import «GenZmk».Parser

def mkHexID : String -> String
  | s => String.join (s.toList.map toHex)
 where
  toHex : Char -> String
    | c => (Nat.toDigits 16 c.toNat).asString

def ZMK.bindingID : Binding -> String
  | Binding.key v => "emk_" ++ mkHexID v
  | Binding.unicode v => "emu_" ++ match v with
      | Sum.inl c => mkHexID c.toString
      | Sum.inr (c,s) => mkHexID c.toString ++ mkHexID s.toString
  | Binding.mac "kbd" v => "eme_" ++ mkHexID v
  | _ => "na-id"

def ZMK.decodeChar : Char -> String
  | ',' => "COMMA"
  | '.' => "DOT"
  | '/' => "FSLH"
  | '-' => "MINUS"
  | '_' => "UNDER"
  | ':' => "COLON"
  | ';' => "SEMI"
  | '=' => "EQUAL"
  | '!' => "EXCL"
  | '@' => "AT"
  | '[' => "LBKT"
  | ']' => "RBKT"
  | '*' => "STAR"
  | '#' => "HASH"
  | '$' => "DLLR"
  | '&' => "AMPS"
  | '\\' => "BSLH"
  | '{' => "LBRC"
  | '}' => "RBRC"
  | '~' => "TILDE"
  | '%' => "PRCNT"
  | '+' => "PLUS"
  | '\'' => "SQT"
  | '"' => "DQT" -- "
  | '(' => "LPAR"
  | ')' => "RPAR"
  | '^' => "CARET"
  | '`' => "GRAVE"
  | '|' => "PIPE"
  | c => if c >= '0' && c <= '9' then s!"N{c}" else c.toString.toUpper

def ZMK.key : String -> String
  | "alt" => "LALT"
  | "ctrl" => "LCTRL"
  | "shift" => "LSHIFT"
  | "meta" => "LGUI"
  | "print-screen" => "PSCRN"
  | "'\"" => "DQT"
  | v => match v.toList with
    | [c] => ZMK.decodeChar c
    | _ => v.toUpper

def QMK.decodeChar : Char -> String
  | ',' => "COMMA"
  | '.' => "DOT"
  | '/' => "SLASH"
  | '-' => "MINUS"
  | '_' => "UNDERSCORE"
  | ':' => "COLON"
  | ';' => "SEMICOLON"
  | '=' => "EQUAL"
  | '!' => "EXCLAIM"
  | '@' => "AT"
  | '[' => "LEFT_BRACKET"
  | ']' => "RIGHT_BRACKET"
  | '*' => "ASTERISK"
  | '#' => "HASH"
  | '$' => "DOLLAR"
  | '&' => "AMPERSAND"
  | '\\' => "BACKSLASH"
  | '{' => "LEFT_CURLY_BRACE"
  | '}' => "RIGHT_CURLY_BRACE"
  | '~' => "TILDE"
  | '%' => "PERCENT"
  | '+' => "PLUS"
  | '\'' => "QUOTE"
  | '"' => "DOUBLE_QUOTE" -- "
  | '(' => "LEFT_PAREN"
  | ')' => "RIGHT_PAREN"
  | '^' => "CIRCUMFLEX"
  | '`' => "GRAVE"
  | '|' => "PIPE"
  | c => if c >= '0' && c <= '9' then s!"{c}" else c.toString.toUpper


-- TODO: implement the full kbd string format
def ZMK.decodeKBD : String -> (String ⊕ String)
  | "C-SPC" => Sum.inl "LC(SPACE)"
  | "C-x t RET" => Sum.inr "&kp LC(X) &kp T &kp RET"
  | "C-c <left>" => Sum.inr "&kp LC(C) &kp LEFT"
  | "C-c <right>" => Sum.inr "&kp LC(C) &kp RIGHT"
  | v => match v.toList with
    | ['M', '-', x] => Sum.inl s!"LA({x.toUpper.toString})"
    | ['C', '-', x] => Sum.inl s!"LC({x.toUpper.toString})"
    | ['C', '-', 'x', ' ', c] => Sum.inr s!"&kp LC(X) &kp {ZMK.decodeChar c}"
    | ['C', '-', 'x', ' ', 'p', ' ', c] => Sum.inr s!"&kp LC(X) &kp P &kp {ZMK.decodeChar c}"
    | ['C', '-', 'x', ' ', 't', ' ', 'O'] => Sum.inr s!"&kp LC(X) &kp T &kp LS(O)"
    | ['C', '-', 'x', ' ', 't', ' ', c] => Sum.inr s!"&kp LC(X) &kp T &kp {ZMK.decodeChar c}"
    | xs => Sum.inr (" ".intercalate ((xs.map ZMK.decodeChar).map ("&kp " ++ .)))
def QMK.decodeKBD : String -> String
  | "C-SPC" => "SS_LCTL(\" \")"
  | "C-x t RET" => "SS_LCTL(\"x\") \"t\\n\""
  | "C-c <left>" => "SS_LCTL(\"c\") SS_TAP(X_LEFT)"
  | "C-c <right>" => "SS_LCTL(\"c\") SS_TAP(X_RIGHT)"
  | v => match v.toList with
    | ['M', '-', x] => s!"SS_LALT(\"{x.toString}\")"
    | ['C', '-', x] => s!"SS_LCTL(\"{x.toString}\")"
    | ['C', '-', 'x', ' ', c] => s!"SS_LCTL(\"x\") \"{c.toString}\""
    | ['C', '-', 'x', ' ', p, ' ', c] => s!"SS_LCTL(\"x\") \"{p.toString}{c.toString}\""
    | xs => s!"\"{v}\""

def Config.renderZMKBinding (config : Config) : Binding -> String
  | Binding.key v => "&kp " ++ ZMK.key v
  | Binding.na => "&none"
  | Binding.hold'tap name (Binding.key hold) (Binding.key tap) =>
      let holdKey := match config.layerPos hold with
        | some n => s!"{n}"
        | _ => ZMK.key hold
      s!"&{name} {holdKey} {ZMK.key tap}"
  | b@(Binding.hold'tap _ _ _) => s!"Unknown {reprStr b}"
  | b@(Binding.mac name v) => match name with
      | "shift" => s!"&kp LS({ZMK.key v})"
      | "gui" => s!"&kp LG({ZMK.key v})"
      | "alt" => s!"&kp LA({ZMK.key v})"
      | "ctrl" => s!"&kp LC({ZMK.key v})"
      | "to" => s!"&to {config.layerPos! v.toLower}"
      | "mo" => s!"&mo {config.layerPos! v.toLower}"
      | "sl" => s!"&sl {config.layerPos! v.toLower}"
      | "pg" => s!"&kp PG_{v.toUpper}"
      | "vol" => match v with
          | "up" => "&kp C_VOLUME_UP"
          | _ => "&kp C_VOLUME_DOWN"
      | "br" => match v with
          | "up" => "&kp C_BRIGHTNESS_INC"
          | _ => "&kp C_BRIGHTNESS_DEC"
      | "click" =>
          let click := match v with
            | "left" => "L"
            | "right" => "R"
            | _ => "M"
          s!"&mkp {click}CLK"
      | "mouse" =>
          let dir := match v with
            | "up"    => "MOVE_Y(-"
            | "left"  => "MOVE_X(-"
            | "down"  => "MOVE_Y("
            | "right" => "MOVE_X("
            | _ => "OOPS("
          s!"&mmv {dir}1250)"
      | "scroll" =>
          let dir := match v with
            | "up" => "MOVE_Y("
            | _ => "MOVE_Y(-"
          s!"&msc {dir}10)"
      | "out" => "&out " ++ if v == "toggle" then "OUT_TOG" else v.toUpper
      | "bt" => match v with
          | "clear" => "&bt BT_CLR"
          | _ => s!"&bt BT_SEL {v.toNat! - 1}"
      | "kbd" => match ZMK.decodeKBD v with
          | Sum.inl k => s!"&kp {k}"
          | _ => "&" ++ ZMK.bindingID b
      | _ => s!"&{name} {ZMK.key v}"
  | b@(Binding.unicode _) => "&" ++ ZMK.bindingID b
  -- | b =>


def Config.renderZMKLayer (config : Config) (layer : Layer) : String :=
   "\n".intercalate [
       "/ {"
     , "  keymap {"
     , "    compatible = \"zmk,keymap\";"
     , "    layer_" ++ layer.name ++ " {"
     , "      label = \"" ++ layer.name ++ "\";"
     , "      bindings = <"
     , "  " ++ bindings
     , "      >;"
     , "    };"
     , "  };"
     , "};"
  ]
  where
    bindings := String.intercalate "\n  " $ layer.bindings.map config.renderZMKBinding

def Char.toUTF16BE (c : Char) : Option (UInt8 × UInt8) :=
  if c.val < 0x10000 then
    let c1 := UInt32.shiftRight c.val 8
    let c2 := UInt32.land c.val 0xff
    some (c1.toUInt8, c2.toUInt8)
  else none

def unicodeSequence (c : Char) : Option (List String) := do
  let (c1, c2) <- c.toUTF16BE
  let toSeq (c : UInt8) := match Nat.toDigits 16 c.toNat with
    | [n] => some ['0', n]
    | [n, m] => some [n, m]
    | _ => none
  let s1 <- toSeq c1
  let s2 <- toSeq c2
  let toKey (c : Char) := if c >= '0' && c <= '9' then s!"N{c}" else s!" {c.toUpper}"
  (s1 ++ s2).map toKey

def ZMK.renderMacro : Binding -> Option String
  | Binding.unicode val => some $ match val with
    | Sum.inl c =>
        "ZMK_UNICODE_SINGLE(" ++ ", ".intercalate (("emu_" ++ mkHexID c.toString) ::
        (unicodeSequence c).getD ["OOPS"]) ++ ") // " ++ c.toString
    | Sum.inr (c, s) =>
        "ZMK_UNICODE_PAIR(" ++ ", ".intercalate (("emu_" ++ mkHexID c.toString ++ mkHexID s.toString) ::
         (unicodeSequence c).getD ["OOPS"] ++ (unicodeSequence s).getD ["OOPS"]) ++ s!") // {c}/{s}"
  | Binding.mac "kbd" val => match ZMK.decodeKBD val with
    | Sum.inl _ => none
    | Sum.inr macroBinding =>
        s!"ZMK_BEHAVIOR(eme_{mkHexID val}, macro, bindings = <{macroBinding}>; wait-ms = <0>; tap-ms = <5>;)"
  | _ => none

def ZMK.renderCombo (config : Config) (speed : String) (layers : String) (combo : Combo) :=
    "/ { combos { compatible = \"zmk,combos\"; combo_" ++ name ++
    " { timeout-ms = <" ++ speed ++ ">; bindings = <" ++ str ++ ">; key-positions = <" ++ keys ++
    ">; layers = <" ++ layers ++ ">; }; }; };"
 where
  name := ZMK.bindingID combo.binding
  str := config.renderZMKBinding combo.binding
  keys := " ".intercalate (combo.keys.map (fun n => s!"{n}"))

def ZMK.renderCombos (config : Config) (layerCombos : LayerCombos) :=
  "\n".intercalate (layerCombos.combos.map (ZMK.renderCombo config  s!"{layerCombos.speed}" layers))
 where
  layers := " ".intercalate ((layerCombos.layers.filterMap config.layerPos).map (fun n => s!"{n}"))


def Config.renderZMK (config : Config) : String :=
  String.intercalate "\n" (combos ++ macros ++ config.layers.reverse.map config.renderZMKLayer ++ [""])
where
  macs := (List.join $ config.layers.map (Layer.bindings)).filterMap ZMK.renderMacro
  macros := macs.eraseDups
  combos := config.combos.map (ZMK.renderCombos config)

def QMK.key : String -> String
  | "alt" => "LALT"
  | "ctrl" => "LEFT_CTRL"
  | "shift" => "LEFT_SHIFT"
  | "meta" => "LGUI"
  | "ret" => "ENTER"
  | "bspc" => "BACKSPACE"
  | "RET" => "ENTER"
  | "print-screen" => "PRINT_SCREEN"
  | "'\"" => "DOUBLE_QUOTE"
  | v => match v.toList with
    | [c] => QMK.decodeChar c
    | _ => v.toUpper

def QMK.layerNameStr (name : String) : String := s!"_layer_{name}"
def QMK.layerName (layer : Layer) : String := QMK.layerNameStr layer.name
def QMK.modName (k : String) : String := match k with
  | "ctrl" => "LCTL"
  | "shift" => "LSFT"
  | "alt" => "LALT"
  | _ => "Unknown mod " ++ k
def QMK.customUnicodeName : (Char ⊕ (Char × Char)) -> String
    | Sum.inl c => "M_" ++ mkHexID c.toString
    | Sum.inr (c, s) => "M_" ++ mkHexID c.toString ++ mkHexID s.toString
def QMK.customKbdName (val : String) : String := s!"M_{mkHexID val}"

def QMK.renderBinding : Binding -> String
  | Binding.key v => "KC_" ++ QMK.key v
  | Binding.unicode val => QMK.customUnicodeName val
  | Binding.na => "XXXXXXX"
  | Binding.hold'tap name (Binding.key hold) (Binding.key tap) =>
      match name with
       | "mt_repeat" => s!"MT(MOD_{QMK.modName hold}, KC_{QMK.key tap})"
       | "hll" => s!"LT({QMK.layerNameStr hold}, KC_{QMK.key tap})"
       | "hlr" => s!"LT({QMK.layerNameStr hold}, KC_{QMK.key tap})"
       | "hhr" => s!"LT({QMK.layerNameStr hold}, KC_{QMK.key tap})"
       | "hml" => s!"MT(MOD_{QMK.modName hold}, KC_{QMK.key tap})"
       | "hmr" => s!"MT(MOD_{QMK.modName hold}, KC_{QMK.key tap})"
       | _ => "XXXXXXX"
  | Binding.mac name v => match name with
     | "shift" => s!"LSFT(KC_{QMK.key v})"
     | "gui" => s!"LGUI(KC_{QMK.key v})"
     | "alt" => s!"LALT(KC_{QMK.key v})"
     | "ctrl" => s!"LCTL(KC_{ZMK.key v})"
     | "to" => s!"TO({QMK.layerNameStr v})"
     | "mo" => s!"MO({QMK.layerNameStr v})"
     | "sl" => s!"OSL({QMK.layerNameStr v})"
     | "pg" => s!"KC_PG{v.toUpper}"
     | "vol" => match v with
       | "up" => "KC_AUDIO_VOL_UP"
       | _ => "KC_AUDIO_VOL_DOWN"
     | "br" => match v with
       | "up" => "KC_BRIGHTNESS_UP"
       | _ => "KC_BRIGHTNESS_DOWN"
     | "click" =>
          let click := match v with
            | "left" => "1"
            | "right" => "2"
            | _ => "3"
          s!"KC_MS_BTN{click}"
     | "scroll" =>
          let dir := match v with
            | "up" => "UP"
            | "left" => "LEFT"
            | "right" => "RIGHT"
            | _ => "DOWN"
          s!"KC_MS_WH_{dir}"
     | "kbd" => QMK.customKbdName v
     | _ => "XXXXXXX"
  | _ => "XXXXXXX"

def QMK.layerMap (layer : Layer) : String := s!"[{QMK.layerName layer}] = LAYOUT_split_3x6_3(\n" ++
  "    " ++ (",\n    ".intercalate (layer.bindings.map QMK.renderBinding)) ++
  ")"

def QMK.renderLayerEnum (layers : List Layer) : String :=
  "\n".intercalate (["enum layer {", " " ++ ", ".intercalate (layers.map QMK.layerName), "};"])

def QMK.renderKeymaps (layers : List Layer) : String :=
  "\n".intercalate (["const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {",
                     "  " ++ ",\n  ".intercalate (layers.map QMK.layerMap),
                     "};"
  ])

def QMK.comboName (combo : Combo) : String := s!"combo_{QMK.renderBinding combo.binding}"

def QMK.renderComboMem (base : Layer) (combos : LayerCombos) : String :=
  "\n".intercalate (combos.combos.map renderCombo)
where
  renderKey pos := match base.bindings.get? pos with
    | some b => QMK.renderBinding b
    | none => "OOOPS"
  renderCombo combo :=
    let keys := ", ".intercalate (combo.keys.map renderKey)
    "const uint16_t PROGMEM " ++ QMK.comboName combo ++ "[] = {" ++ keys ++ ", COMBO_END};"

def QMK.renderComboMap (combos : LayerCombos) : String :=
  ",\n  ".intercalate (combos.combos.map renderCombo)
where
  renderCombo combo := "COMBO(" ++ QMK.comboName combo ++ ", " ++ QMK.renderBinding combo.binding ++ ")"

def QMK.renderComboTerm (acc : List String) (pos : Nat) (combos : List LayerCombos) : List String :=
  match combos with
   | [] => acc.reverse
   | combo :: rest =>
      let count := combo.combos.length
      let term := s!"  if (index < {pos + count}) return {combo.speed};"
      QMK.renderComboTerm (term :: acc) (pos + count) rest

def QMK.renderCombos (base : Option Layer) (combos : List LayerCombos) : String :=
  match base with
    | some layer => "\n".intercalate [
           "\n".intercalate (combos.map (QMK.renderComboMem layer)),
           "\ncombo_t key_combos[] = {\n  " ++
           ",\n  ".intercalate (combos.map QMK.renderComboMap) ++
           "\n};",
           "uint16_t get_combo_term(uint16_t index, combo_t *combo) {\n" ++
           "\n".intercalate (QMK.renderComboTerm [] 0 combos) ++
           "\n  return 40;" ++
           "\n}"
      ]
    | none => "#error \"Can't find base layer\""

def QMK.customKeycode : Binding -> Option (Binding × String)
  | b@(Binding.unicode val) => some (b, QMK.customUnicodeName val)
  | b@(Binding.mac "kbd" val) => some (b, QMK.customKbdName val)
  | _ => none

def QMK.renderCustomKeycodes (config : Config) : String :=
  "enum custom_keycodes {\n  " ++
  ",\n  ".intercalate keycodes ++
  ",\n};\n#define MACRO_HANDLERS \\\n  " ++
  " \\\n  ".intercalate handlers
where
  codes := (List.join $ config.layers.map (Layer.bindings)).filterMap QMK.customKeycode
  mkAction b := match b with
   | Binding.unicode val => match val with
     | Sum.inl c => s!"send_unicode_string(\"{c}\")"
     | Sum.inr (c, s) => s!"send_unicode_string(is_shifted ? \"{s}\" : \"{c}\")"
   | Binding.mac "kbd" val => s!"SEND_STRING({QMK.decodeKBD val})"
   | _ => "TODO!"
  mkHandler b := "case " ++ b.2 ++ ": " ++ mkAction b.1 ++ "; break;"

  handlers := codes.eraseDups.map mkHandler
  keycodes := match codes.eraseDups.map Prod.snd with
    | head :: rest => (head ++ " = SAFE_RANGE") :: rest
    | r => r

def QMK.renderLayerOled (layers : List Layer) : String :=
  "#define LAYER_HANDLERS \\\n  " ++
  " \\\n  ".intercalate handlers
where
  mkHandler l := "case " ++ QMK.layerName l ++ s!": oled_write_P(PSTR(\"{l.name}\\n\"), false); break;"
  handlers := layers.map mkHandler

def Config.renderQMK (config : Config) : String :=
  "// Generated with https://github.com/TristanCacqueray/gen-zmk/\n\n" ++
  String.intercalate "\n\n" (["#pragma once",
    QMK.renderLayerEnum config.layers.reverse,
    QMK.renderLayerOled config.layers.reverse,
    QMK.renderCustomKeycodes config,
    QMK.renderCombos config.layers.head? config.combos,
    QMK.renderKeymaps config.layers.reverse])

#eval Config.renderQMK <$> Config.demo
