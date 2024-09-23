# gen-zmk - generate keyboard config and diagrams

## Overview and scope

The goal of this project is to convert such user defined configurations:

```
(def-cfg
  ;; os sets the unicode sequence code
  :os 'linux
  ;; board sets the layout and bilateral positions
  :board 'corne
)

(def-alias shft_bspc (mt_repeat shift bspc))

(def-combos 18 (base num qwerty)
    - (LT2 LM2) _ (LT1 LM1) & (LM1 LB1) # (LM2 LB2)
    $ (LM0 LB0)
    ` (RM1 RB1) \ (RM2 RB2) ' (RT2 RM2) '" (RT1 RM1)
)

(def-combos 30 (base num qwerty)
    = (RM0 RM1) | (RM3 RM4) ! (LT3 LM3) @ (LT3 LT2) '( (LM3 LM2) ') (LM2 LM1)
    [ (LM4 LM3) ] (LM1 LM0) { (LT2 LT1) } (LT1 LT0) ~ (RT1 RT2) % (RT2 RT3)
    * (RM1 RM2) + (RM2 RM3) ^ (RT3 RM3)
    )

(def-layer base
  (to mice) q w e r t   y u i o p (sl french)
  tab       (hll wm a)  (hll num s)  (hml ctrl d) (hml alt f) g
  h         (hmr alt j) (hmr ctrl k) (hlr num l)  (hhr wm ;)  (sl greek)
  esc       z x c v b   n m , . / ret
    meta alt shft_bspc space (mo fn) (mo num)
)

;; M-y is past from kill-ring, C-y is kill-ring, S-y is paste through mouse middle-click
(def-layer num
  _ F20 F21   up  F22   F23  (click middle) 7 8 9 *          _
  _ F18 left down right F19  (pg up)        4 5 6 (kbd ":=") _
  _ F13 F14   F15 F16   F17  (pg dn)        1 2 3 \ RET
    (to base) (to base) (to base) _ 0 .
  )
(def-layer fn :overlay num 'top-right
  (to base) INS HOME up END (pg up)          _  F7 F8 F9 F10 _
  _ DEL left down right     (pg dn)     (pg up) F4 F5 F6 F11 _
  _ (vol dn) (vol up) (br dn) (br up) _ (pg dn) F1 F2 F3 F12 _
  (to base) (to base) (to base) _ (to base) print-screen
  )

(def-alias SE (shift esc))
(def-layer french :overlay base 'top-right
  _ __ èÉ éÉ êÊ __ __ ùÙ îÎ œŒ SE _
  _ àÀ âÂ ëË __ €  __ üÜ ïÏ ôÔ …  _
  _ __ __ çÇ __ __ __ __ «  »  __ _
    __ __ shift __ (to base) __)
(def-layer greek :overlay base 'top-left
  _ ωΩ ηΗ ϵΕ ρΡ τΤ γΓ υΥ ιΙ οΟ πΠ _
  _ αΑ σΣ δΔ φΦ θΘ χΧ ×  κΚ λΛ ψΨ _
  _ ζΖ ξΞ __ ωΩ βΒ νΝ μΜ N  ℝ  ⊕  _
    __ __ shift __ (to base) __)

(def-label (kbd "C-c p p") "📂 project")
(def-label (kbd "C-c p f") "📂 proj-file")
(def-label (gui ret) "🚀 terminal")
(def-label (gui v) "🚀 gmrun")
(def-label (gui F) "💻 code")
(def-label (gui G) "💻 comm")
(def-label (gui H) "💻 web")
(def-label (gui A) "💻 1")
(def-label (gui S) "💻 2")
(def-label (gui D) "💻 3")
(def-label (gui J) "💻 7")
(def-label (gui K) "💻 8")
(def-label (gui L) "💻 9")
(def-label (gui tab) "⇄ win")
(def-label (kbd "C-x b") "⇄ buffer")
(def-label (kbd "C-x 0") "⌧ win")
(def-label (kbd "C-x 1") "⌧ other-win")
(def-label (kbd "C-x 2") "✂ horiz")
(def-label (kbd "C-x 3") "✂ vert")
(def-label SE "🎤 push")

(def-layer wm
  __ __ __           (ctrl up)   __            __         (gui v)   (gui J) (gui K) (gui L) (kbd "C-c p p") __
  (gui tab) __ (ctrl left) (ctrl down) (ctrl right) __  (gui ret) (gui F) (gui G) (gui H) (kbd "C-c p f") __
  __ __ (gui A)      (gui S)      (gui D)      (kbd "C-x b") (kbd "C-x 0") (kbd "C-x 1") (kbd "C-x 2") (kbd "C-x 3") __ __
  _ _ _ (kbd "C-SPC") (to base) __
)

(def-layer mice
  (to base)   _ (alt left) up (alt right) _ _ (click left) (mouse up) (click right) _ _
  (to sys)    _ left down right (pg up) (scroll up) (mouse left) (mouse down) (mouse right) _ _
  (to qwerty) _ _ ctrl alt (pg dn) (scroll dn) (click middle) _ _ _ ret
  meta _ shift (click left) (to base) _
)

(def-layer sys
  (to base) (out toggle) _ _ _ _ _ _ _ _ _ _
  (to base) _ _ _ _ _ _ (bt 4) (bt 5) _ _ _
  _ _ _ _ _ (bt clear) _ (bt 1) (bt 2) (bt 3) _ _
  _ _ _ _ (to base) _
  )

(def-layer qwerty
  (to base) q w e r t   y u i o p  (sl french)
  tab       a s d f g   h j k l ;  (sl greek)
  esc       z x c v b   n m , . /  ret
    ctrl alt shft_bspc space (mo fn) (mo num)
)

```

… into [zmk config](https://github.com/TristanCacqueray/zmk-config/blob/main/config/gen-zmk.dtsi) and such documentation diagrams:

![diagrams](https://raw.githubusercontent.com/TristanCacqueray/zmk-config/main/moonwalker.svg)


## Features

- [x] Unicode sequence
- [x] Keyboard macro using emacs (kbd) syntax
- [x] Layer IDs
- [ ] Macro description (e.g. shift-esc is mumble push-to-talk)
- [ ] QMK/KMonad
- [ ] bilateral tap-hold config


# Contribute

Update demo with:

```ShellSession
watchexec --ignore "*.svg" --ignore "*.keymap" lake exe demo
```
