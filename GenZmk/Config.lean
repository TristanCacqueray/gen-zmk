import «GenZmk».Parser
open Parser

-- data
inductive Binding where
  | unicode (val : (Char ⊕ (Char × Char)))
  | key (val : String)
  | hold'tap (name : String) (hold : Binding) (tap : Binding)
  | mac (name : String) (arg : String)
  | na
deriving Repr, BEq

inductive Overlay where
  | TopRight
  | TopLeft
deriving Repr, BEq

structure Layer where
  name     : String
  overlay  : Option (String × Overlay)
  bindings : List Binding
deriving Repr

inductive Os where
  | Linux
deriving Repr, BEq

inductive Board where
  | Corne
deriving Repr, BEq

structure SystemConfig where
  os    : Os
  board : Board
deriving Repr

structure Alias where
  name    : String
  binding : Binding
deriving Repr

inductive HandPosition where
  | LeftHand
  | RightHand
deriving Repr, BEq

structure KeyPos where
  hand : HandPosition
  col  : Nat
  row  : Nat
deriving Repr, BEq

inductive Flavor where
  | Balanced
  | HoldPreferred
deriving Repr

structure HoldTap where
  name     : String
  position : Option HandPosition
  flavor   : Flavor
  hold     : String
  tap      : String
  holdTrig : Bool
deriving Repr

structure Combo where
  binding  : Binding
  keys     : List Nat
deriving Repr

structure LayerCombos where
  speed    : Nat
  layers   : List String
  combos   : List Combo
deriving Repr

structure Theme where
  layersColor : List String
deriving Repr

def Theme.default := Theme.mk
  ["#000",
   "#7e301e",
   "#85521c",
   "#5da655",
   "#7a7026", -- greek
   "#5e1e2d",
   "#6b2228",
   "#84391c",
   "#000",
   "#000"
  ]

structure Config where
  system   : SystemConfig
  theme    : Theme
  layers   : List Layer
  aliases  : List Alias
  holdTaps : List HoldTap
  combos   : List LayerCombos
deriving Repr

def Config.default : Config := Config.mk (SystemConfig.mk Os.Linux Board.Corne) Theme.default [] [] [] []

def Config.layerPos (config : Config) : String -> Option Nat
  | layer => config.layers.reverse.toArray.findIdx? (fun x => x.name == layer)

def Config.layerPos! (config : Config) : String -> Nat
  | layer => (config.layerPos layer).getD 42

-- helpers
abbrev Result (α : Type) := ReaderT Config (Except String) α

abbrev Parser (α : Type) := Sexpr -> Result α

def getArg (name : String) (default : String) : List Sexpr -> Sexpr
  | [] => Sexpr.atom default
  | Sexpr.atom k :: v :: rest => if k == ":" ++ name then v else getArg name default rest
  | _ :: rest => getArg name default rest

def getArgStr (name : String) (default : String) (exprs : List Sexpr) : String :=
  match getArg name default exprs with
    | Sexpr.atom k => k
    | _ => default

def getArgBool (name : String) : List Sexpr -> Bool
  | [] => false
  | Sexpr.atom k :: rest => if k == ":" ++ name then true else getArgBool name rest
  | _ :: rest => getArgBool name rest


def fail [Repr α] (name : String) (other : α) : Result β :=
  Except.error s!"Excepted {name} got {reprStr other}"

def Alias.find? (name : String) : Result (Option Alias) := do
  let cfg <- ReaderT.read
  pure $ cfg.aliases.find? (fun a => a.name == name)

-- parser
def Binding.parse : Parser Binding
  | Sexpr.atom str =>
     if str.all (. == '_') then pure Binding.na
     else if str.all (. > '~') then match str.toList with
       | [c] => pure $ Binding.unicode (Sum.inl c)
       | [c, s] => pure $ Binding.unicode (Sum.inr (c, s))
       | _ => fail "unicode" (Sexpr.atom str)
     else do
      let aliasM : Option Alias <- Alias.find? str
      match aliasM with
       | some alias => pure $ alias.binding
       | none => pure $ Binding.key str
  | Sexpr.list [Sexpr.atom htName, b1, b2] => do
      -- todo: check hold-tap exists
      Binding.hold'tap htName <$> Binding.parse b1 <*> Binding.parse b2
  | Sexpr.list [Sexpr.atom name, Sexpr.atom arg] => pure $ Binding.mac name arg
  | other => fail "key" other

def Overlay.parse : Parser Overlay
  | Sexpr.atom "'top-right" => pure Overlay.TopRight
  | Sexpr.atom "'top-left" => pure Overlay.TopLeft
  | other => fail "os" other

def Os.parse : Parser Os
  | Sexpr.atom "'linux" => pure Linux
  | other => fail "os" other

def Board.parse : Parser Board
  | Sexpr.atom "'corne" => pure Corne
  | other => fail "board" other

def Board.dim : Board -> (Nat × Nat)
  | Corne => (12, 4)

def cornePos (key : Nat) : Option KeyPos :=
  if key >= 42 then none
  else if key < 12 then some (KeyPos.mk (if key < 6 then lh else rh) key 0)
  else if key < 24 then some (KeyPos.mk (if key < 18 then lh else rh) (key - 12) 1)
  else if key < 36 then some (KeyPos.mk (if key < 30 then lh else rh) (key - 24) 2)
  else some (KeyPos.mk (if key < 39 then lh else rh) (3 + key - 36) 3)
 where
  lh := HandPosition.LeftHand
  rh := HandPosition.RightHand

def Board.keyPos : Board -> Nat -> Option KeyPos
  | Corne => cornePos

partial def Board.allPos (board : Board) : List (Nat × KeyPos) := go 0 []
 where
  go (key : Nat) (acc : List (Nat × KeyPos)) := match board.keyPos key with
    | none => acc.reverse
    | some k => go (key + 1) ((key, k) :: acc)

def Board.keyPosName (board : Board) (keyName : String) : Option Nat := match keyName.toList with
  | [side, row, col] => do
      let hand <- match side with | 'L' => some HandPosition.LeftHand | 'R' => some HandPosition.RightHand | _ => none
      let row <- match row with | 'T' => some 0 | 'M' => some 1 | 'B' => some 2 | 'H' => some 3 | _ => none
      let col <- if col.isDigit then some (col.toNat - '0'.toNat) else none
      let brow := board.allPos.filter (fun (_, keyPos) => keyPos.hand == hand && keyPos.row == row)
      let crow := match hand with
        | HandPosition.LeftHand => brow.reverse
        | HandPosition.RightHand => brow
      -- dbg_trace "{reprStr crow}"
      (fun a => a.fst) <$> (crow.drop col).head?
  | _ => none

#eval Board.Corne.keyPosName "LT2"

def HandPosition.parse : Parser HandPosition
  | Sexpr.atom "'left" => pure LeftHand
  | Sexpr.atom "'right" => pure RightHand
  | other => fail "hand" other

def Flavor.parse : Parser Flavor
  | Sexpr.atom "balanced" => pure Balanced
  | Sexpr.atom "hold-preferred" => pure Balanced
  | other => fail "board" other

def SystemConfig.parse : Parser SystemConfig
  | Sexpr.list (Sexpr.atom "def-cfg" :: rest) => go rest
  | other => fail "def-cfg" other
 where
  go exprs := SystemConfig.mk
    <$> Os.parse (getArg "os" "'linux" exprs)
    <*> Board.parse (getArg "board" "'corne" exprs)

def HoldTap.doParse (name : String) (pos : Option HandPosition) (exprs : List Sexpr): Result HoldTap :=
  HoldTap.mk name pos
    <$> Flavor.parse (getArg "flavor" "balanced" exprs)
    <*> pure (getArgStr "hold" "&kp" exprs)
    <*> pure (getArgStr "tap" "&kp" exprs)
    <*> pure (getArgBool "hold-trigger-on-release" exprs)

def HoldTap.parseHT : Parser HoldTap
  | Sexpr.list (Sexpr.atom "def-hold-tap" :: Sexpr.atom name :: rest) =>
      HoldTap.doParse name none rest
  | other => fail "def-hold-tap" other

def HoldTap.parseHRM : Parser HoldTap
  | Sexpr.list (Sexpr.atom "def-hrm" :: Sexpr.atom name :: pos :: rest) => do
      let hpos <- HandPosition.parse pos
      HoldTap.doParse name (some hpos) rest
  | other => fail "def-hrm" other

def Layer.parse : Parser Layer
  | Sexpr.list (Sexpr.atom "def-layer" :: Sexpr.atom name :: Sexpr.atom ":overlay" :: Sexpr.atom oname :: pos :: rest) => do
      let opos <- Overlay.parse pos
      Layer.mk name (oname, opos) <$> go [] rest
  | Sexpr.list (Sexpr.atom "def-layer" :: Sexpr.atom name :: rest) => Layer.mk name none <$> go [] rest
  | other => fail "layer" other
 where
   go (acc : List Binding) : List Sexpr -> Result (List Binding)
    | [] => pure acc.reverse
    | x :: rest => Binding.parse x >>= fun k => go (k :: acc) rest

def LayerCombos.parse : Parser LayerCombos
  | Sexpr.list (Sexpr.atom "def-combos" :: Sexpr.atom speed :: Sexpr.list layers :: rest) => do
      let env <- ReaderT.read
      LayerCombos.mk speed.toNat! <$> parseLayers layers <*> parseCombos env.system.board rest
  | other => fail "combos" other
 where
   parseLayers : List Sexpr -> Result (List String)
    | [] => pure []
    | Sexpr.atom name :: rest => List.cons name <$> parseLayers rest
    | other => fail "combo layers" other
   parseCombos (board : Board) : List Sexpr -> Result (List Combo)
    | [] => pure []
    | key :: Sexpr.list keys :: rest => do
        let k <- Binding.parse key
        let kb := match k with
          | Binding.na => Binding.key "_"
          | _ => k
        let ks <- parseKeys board keys
        List.cons (Combo.mk kb ks) <$> parseCombos board rest
    | other => fail "combo" other
   parseKeys (board : Board) : List Sexpr -> Result (List Nat)
    | [] => pure []
    | Sexpr.atom keyName :: rest => match board.keyPosName keyName with
        | none => fail "key name" (Sexpr.atom keyName)
        | some keyPos => List.cons keyPos <$> parseKeys board rest
    | other => fail "combo keys" other

def Config.parse : List Sexpr -> Result Config
  | [] => ReaderT.read
  | expr :: rest => do
      let topLevel <- parseTopLevel expr
      ReaderT.adapt topLevel (Config.parse rest)
 where
  parseTopLevel : Sexpr -> Result (Config -> Config)
   | expr@(Sexpr.list (Sexpr.atom "def-cfg" :: _)) => do
      let cfg <- SystemConfig.parse expr
      pure (fun c => {c with system := cfg })
   | (Sexpr.list (Sexpr.atom "def-alias" :: Sexpr.atom name :: v :: [])) => do
      let binding <- Binding.parse v
      pure (fun c => {c with aliases := Alias.mk name binding :: c.aliases })
   | expr@(Sexpr.list (Sexpr.atom "def-hrm" :: _)) => do
      let ht <- HoldTap.parseHRM expr
      pure (fun c => {c with holdTaps := ht :: c.holdTaps})
   | expr@(Sexpr.list (Sexpr.atom "def-hold-tap" :: _)) => do
      let ht <- HoldTap.parseHT expr
      pure (fun c => {c with holdTaps := ht :: c.holdTaps})
   | expr@(Sexpr.list (Sexpr.atom "def-layer" :: _)) => do
      let layer <- Layer.parse expr
      pure (fun c => {c with layers := layer :: c.layers})
   | expr@(Sexpr.list (Sexpr.atom "def-combos" :: _)) => do
      let combo <- LayerCombos.parse expr
      pure (fun c => {c with combos := combo :: c.combos})
   | other => fail "top-level" other

def Config.decode (exprs : List Sexpr) := (Config.parse exprs).run Config.default

def Config.demo := Config.decode =<< Sexpr.parse "
(def-cfg
  ;; os sets the unicode sequence code
  :os 'linux
  ;; board sets the layout and bilateral positions
  :board 'corne
)

(def-alias shft_bspc (mt_repeat LSHIFT BSPC))
(def-layer french é shft_bspc)
(def-layer greek λ)
"

-- #eval Config.demo

def Config.readme : IO Config := do
  let lines <- IO.FS.lines "./README.md"
  let body := (lines.toList.dropWhile (. != "```lisp")).takeWhile (. != "```")
  match Config.decode =<< Parser.Sexpr.parse ("\n".intercalate (body.drop 1)) with
   | .ok conf => pure conf
   | .error err => do
     IO.println s!"error: {err}"
     pure Config.default

#eval Config.readme
