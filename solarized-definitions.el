;;; solarized-definitions.el --- helpers for solarized-theme

(defconst solarized-dark-palette
  ;; name     sRGB      256       16              8
  '((base03  "#002B36" "#1C1C1C" "brightblack"   "black")
    (base02  "#073642" "#262626" "black"         "black")
    (base01  "#586E75" "#585858" "brightgreen"   "green")
    (base00  "#657B83" "#626262" "brightyellow"  "yellow")
    (base0   "#839496" "#808080" "brightblue"    "blue")
    (base1   "#93A1A1" "#8A8A8A" "brightcyan"    "cyan")
    (base2   "#EEE8D5" "#E4E4E4" "white"         "white")
    (base3   "#FDF6E3" "#FFFFD7" "brightwhite"   "white")
    (yellow  "#B58900" "#AF8700" "yellow"        "yellow")
    (orange  "#CB4B16" "#D75F00" "brightred"     "red")
    (red     "#DC322F" "#D70000" "red"           "red")
    (magenta "#D33682" "#AF005F" "magenta"       "magenta")
    (violet  "#6C71C4" "#5F5FAF" "brightmagenta" "magenta")
    (blue    "#268BD2" "#0087FF" "blue"          "blue")
    (cyan    "#2AA198" "#00AFAF" "cyan"          "cyan")
    (green   "#859900" "#5F8700" "green"         "green")))

(defconst solarized-light-palette
  ;; name     sRGB      256       16              8
  '((base03  "#FDF6E3" "#FFFFD7" "brightwhite"   "white")
    (base02  "#EEE8D5" "#E4E4E4" "white"         "white")
    (base01  "#93A1A1" "#8A8A8A" "brightcyan"    "cyan")
    (base00  "#839496" "#808080" "brightblue"    "blue")
    (base0   "#657B83" "#626262" "brightyellow"  "yellow")
    (base1   "#586E75" "#585858" "brightgreen"   "green")
    (base2   "#073642" "#262626" "black"         "black")
    (base3   "#002B36" "#1C1C1C" "brightblack"   "black")
    (yellow  "#B58900" "#AF8700" "yellow"        "yellow")
    (orange  "#CB4B16" "#D75F00" "brightred"     "red")
    (red     "#DC322F" "#D70000" "red"           "red")
    (magenta "#D33682" "#AF005F" "magenta"       "magenta")
    (violet  "#6C71C4" "#5F5FAF" "brightmagenta" "magenta")
    (blue    "#268BD2" "#0087FF" "blue"          "blue")
    (cyan    "#2AA198" "#00AFAF" "cyan"          "cyan")
    (green   "#859900" "#5F8700" "green"         "green")))

(defun solarized-column-index ()
  "Returns the palette column to use based on available features."
  (if window-system
      1
    (case (display-color-cells)
      (16 3)
      (8 4)
      (otherwise 2))))

(defun solarized-find-color (name palette)
  "Grab the named color from the palette."
  (let ((index (solarized-column-index)))
    (nth index (assoc name palette))))

(defun solarized-back (palette)
  "Returns additional attributes for some backgrounds."
  (if (or window-system (> display-color-cells 16))
      `(:background ,(solarized-find-color 'base03 palette))
    '()))

(provide 'solarized-definitions)

;;; solarized-definitions.el ends here
