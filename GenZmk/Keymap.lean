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
    | ['C', '-', 'c', ' ', 'p', ' ', c] => Sum.inr s!"&kp LC(C) &kp P &kp {ZMK.decodeChar c}"
    | ['C', '-', 'x', ' ', 't', ' ', 'O'] => Sum.inr s!"&kp LC(X) &kp T &kp LS(O)"
    | ['C', '-', 'x', ' ', 't', ' ', c] => Sum.inr s!"&kp LC(X) &kp T &kp {ZMK.decodeChar c}"
    | xs => Sum.inr (" ".intercalate ((xs.map ZMK.decodeChar).map ("&kp " ++ .)))


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

def Config.renderKeymap (config : Config) : String :=
  String.intercalate "\n" (combos ++ macros ++ config.layers.reverse.map config.renderZMKLayer ++ [""])
where
  macs := (List.join $ config.layers.map (Layer.bindings)).filterMap ZMK.renderMacro
  macros := macs.eraseDups
  combos := config.combos.map (ZMK.renderCombos config)

#eval Config.renderKeymap <$> Config.demo
