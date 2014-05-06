;;; solarized-theme.el --- Emacs highlighting using Ethan Schoonover’s Solarized color scheme
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cl))

(deftheme solarized "Solarized")

(defcustom solarized-bold t
  "Stops Solarized from displaying bold when nil."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-underline t
  "Stops Solarized from displaying underlines when nil."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-italic t
  "Stops Solarized from displaying italics when nil."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-diff-mode 'normal
  "Sets the level of highlighting to use in diff-like modes."
  :type 'symbol
  :options '(high normal low)
  :group 'solarized)

(defcustom solarized-background 'dark
  "Sets the background."
  :type 'symbol
  :options '(dark light)
  :group 'solarized)

(defcustom solarized-distinct-fringe-background nil
  "Make the fringe background different from the normal background color."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-use-variable-pitch t
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-high-contrast-mode-line nil
  "Make the active/inactive mode line stand out more."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'solarized)

(defcustom solarized-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'solarized)

(defcustom solarized-force-256color nil
  "Force use of the 256-color palette."
  :type 'boolean
  :group 'solarized)

(defcustom solarized-broken-srgb (if (and (not (and (boundp 'ns-use-srgb-colorspace)
                                              ns-use-srgb-colorspace))
                                        (eq system-type 'darwin)
                                        (eq window-system 'ns)) t nil)
  "Emacs bug #8402 results in incorrect color handling on Macs.
If this is t \(the default on Macs), Solarized works around it
with alternative colors.  However, these colors are not totally
portable, so you may be able to edit the \"ARGB\" column in
solarized-definitions.el to improve them further."
  :type 'boolean
  :group 'solarized)

(defconst solarized-dark-palette
  ;; name     sRGB      RGB       256       16              8
  '((base03  "#002B36" "#042028" "#1C1C1C" "brightblack"   "black")
    (base02  "#073642" "#0A2832" "#262626" "black"         "black")
    (base01  "#586E75" "#465A61" "#585858" "brightgreen"   "green")
    (base00  "#657B83" "#52676f" "#626262" "brightyellow"  "yellow")
    (base0   "#839496" "#708183" "#808080" "brightblue"    "blue")
    (base1   "#93A1A1" "#81908f" "#8A8A8A" "brightcyan"    "cyan")
    (base2   "#EEE8D5" "#E9E2CB" "#E4E4E4" "white"         "white")
    (base3   "#FDF6E3" "#FCF4DC" "#FFFFD7" "brightwhite"   "white")
    (yellow  "#B58900" "#A57705" "#AF8700" "yellow"        "yellow")
    (orange  "#CB4B16" "#BD3612" "#D75F00" "brightred"     "red")
    (red     "#DC322F" "#C60007" "#D70000" "red"           "red")
    (magenta "#D33682" "#C61B6E" "#AF005F" "magenta"       "magenta")
    (violet  "#6C71C4" "#5859B7" "#5F5FAF" "brightmagenta" "magenta")
    (blue    "#268BD2" "#2075C7" "#0087FF" "blue"          "blue")
    (cyan    "#2AA198" "#259185" "#00AFAF" "cyan"          "cyan")
    (green   "#859900" "#728A05" "#5F8700" "green"         "green")))

(defconst solarized-light-palette
  ;; name     sRGB      ARGB      256       16              8
  '(
    (base03  "#FDF6E3" "#FCF4DC" "#FFFFD7" "brightwhite"   "white")
    (base02  "#EEE8D5" "#E9E2CB" "#E4E4E4" "white"         "white")
    (base01  "#93A1A1" "#81908f" "#8A8A8A" "brightcyan"    "cyan")
    (base00  "#839496" "#708183" "#808080" "brightblue"    "blue")
    (base0   "#657B83" "#52676f" "#626262" "brightyellow"  "yellow")
    (base1   "#586E75" "#465A61" "#585858" "brightgreen"   "green")
    (base2   "#073642" "#0A2832" "#262626" "black"         "black")
    (base3   "#002B36" "#042028" "#1C1C1C" "brightblack"   "black")
    (yellow  "#B58900" "#A57705" "#AF8700" "yellow"        "yellow")
    (orange  "#CB4B16" "#BD3612" "#D75F00" "brightred"     "red")
    (red     "#DC322F" "#C60007" "#D70000" "red"           "red")
    (magenta "#D33682" "#C61B6E" "#AF005F" "magenta"       "magenta")
    (violet  "#6C71C4" "#5859B7" "#5F5FAF" "brightmagenta" "magenta")
    (blue    "#268BD2" "#2075C7" "#0087FF" "blue"          "blue")
    (cyan    "#2AA198" "#259185" "#00AFAF" "cyan"          "cyan")
    (green   "#859900" "#728A05" "#5F8700" "green"         "green")))

(defconst solarized-contrast-components
  ;; name       sRGB      ARGB      256       16              8
  '((yellow  (("#7B6000" "#686207" "#875f00" "yellow"        "yellow")
              ("#DEB542" "#BDB962" "#d7a55f" "brightyellow"  "white")))
    (orange  (("#8B2C02" "#652D06" "#d75f00" "red"           "red")
              ("#F2804F" "#C08B70" "#ff875f" "brightyellow"  "white")))
    (red     (("#990A1B" "#680A24" "#870000" "red"           "red")
              ("#FF6E64" "#C27189" "#ff5f5f" "brightred"     "white")))
    (magenta (("#93115C" "#651177" "#87005f" "magenta"       "magenta")
              ("#F771AC" "#BD73E5" "#ff87af" "brightmagenta" "white")))
    (violet  (("#3F4D91" "#394EBF" "#5f5fd7" "blue"          "blue")
              ("#9EA0E5" "#8AA3FF" "#87afff" "brightmagenta" "white")))
    (blue    (("#00629D" "#1363CF" "#005faf" "blue"          "blue")
              ("#69B7F0" "#6CBAFF" "#5fafff" "brightblue"    "white")))
    (cyan    (("#00736F" "#187595" "#00875f" "cyan"          "cyan")
              ("#69CABF" "#71CEFF" "#5fd7d7" "brightcyan"    "white")))
    (green   (("#546E00" "#517108" "#5f8700" "green"         "green")
              ("#B4C342" "#A3C863" "#d7ff5f" "brightgreen"   "white")))))

(defun solarized-column-index ()
  "Return the palette column to use based on available features."
  (if solarized-force-256color
      3
    (if window-system
        (if solarized-broken-srgb 2 1)
      (case (display-color-cells)
        (16 4)
        (8 5)
        (otherwise 3)))))

(defun solarized-find-color (name palette)
  "Grab the color NAME from the PALETTE."
  (let ((index (solarized-column-index)))
    (nth index (assoc name palette))))

(defun solarized-diff-case (high-value low-value normal-windowed-value normal-value)
  "Check `solarized-diff-mode' and return the appropriate value.
These values are chosen from HIGH-VALUE, LOW-VALUE, NORMAL-WINDOWED-VALUE and
NORMAL-VALUE."
  (case solarized-diff-mode
    (high high-value)
    (low low-value)
    (normal (if window-system
                normal-windowed-value
              normal-value))))

(defun solarized-contrast-palette ()
  "Build the Solarized contrast palette."
  (cl-reduce 'append
             (cl-reduce 'append
                        (mapcar
                         (lambda (color)
                           (let ((index (- (solarized-column-index) 1))
                                 (hc-index (if (eq solarized-background 'light) 0 1))
                                 (lc-index (if (eq solarized-background 'light) 1 0)))
                             (mapcar
                              (lambda (cons)
                                (list
                                 (list (intern (concat (symbol-name (car color)) "-hc"))
                                       (nth index (nth hc-index cons)))
                                 (list (intern (concat (symbol-name (car color)) "-lc"))
                                       (nth index (nth lc-index cons)))))
                              (cdr color)))) solarized-contrast-components))))

(defmacro solarized-with-values (&rest body)
  "`let' bind all values for Solarized on BODY."
  (declare (indent 0))
  (let ((index (solarized-column-index))
        (palette (if (eq 'light solarized-background)
                     solarized-light-palette
                   solarized-dark-palette))
        (bold (if solarized-bold 'bold 'normal))
        (bright-bold (if solarized-bold 'normal 'bold))
        (underline (if solarized-underline t nil))
        (italic (if solarized-italic 'italic 'normal)))
    `(let* (,@(mapcar (lambda (cons) (list (car cons) (nth index cons))) palette)

            ;; Basic colors
            (solarized-fg ,(if (eq 'light solarized-background) 'base00 'base0))
            (solarized-bg ,(if (or window-system (> (display-color-cells) 16))
                               (if (eq 'light solarized-background) 'base3 'base03) palette
                             '()))
            (solarized-hl ,(if (eq 'light solarized-background) 'base2 'base02))
            (solarized-emph ,(if (eq 'light solarized-background) 'base01 'base1))
            (solarized-comment ,(if (eq 'light solarized-background) 'base1 'base01))

            ,@(solarized-contrast-palette)

            (s-variable-pitch (if solarized-use-variable-pitch
                                  'variable-pitch 'default))
            (s-fringe-bg (if solarized-distinct-fringe-background
                             solarized-hl solarized-bg))
            (s-mode-line-fg (if solarized-high-contrast-mode-line
                                solarized-bg solarized-fg))
            (s-mode-line-bg (if solarized-high-contrast-mode-line
                                solarized-fg solarized-hl))
            (s-mode-line-buffer-id-fg (if solarized-high-contrast-mode-line
                                          'unspecified solarized-emph))
            (s-mode-line-inactive-fg (if solarized-high-contrast-mode-line
                                         solarized-fg solarized-comment))
            (s-mode-line-inactive-bg (if solarized-high-contrast-mode-line
                                         solarized-hl solarized-bg))
            (s-mode-line-inactive-bc (if solarized-high-contrast-mode-line
                                         solarized-fg solarized-hl))

            (opt-under nil)
            (fmt-none '(:weight normal :slant normal  :underline nil        :inverse-video nil))
            (fmt-bold '(:weight ,bold  :slant normal  :underline nil        :inverse-video nil))
            (fmt-bldi '(:weight ,bold                 :underline nil        :inverse-video nil))
            (fmt-undr '(:weight normal :slant normal  :underline ,underline :inverse-video nil))
            (fmt-undb '(:weight ,bold  :slant normal  :underline ,underline :inverse-video nil))
            (fmt-undi '(:weight normal                :underline ,underline :inverse-video nil))
            (fmt-uopt '(:weight normal :slant normal  :underline opt-under :inverse-video nil))
            (fmt-curl '(:weight normal :slant normal  :underline t          :inverse-video nil))
            (fmt-ital '(:weight normal :slant ,italic :underline nil        :inverse-video nil))
            (fmt-stnd '(:weight normal :slant normal  :underline nil        :inverse-video t))
            (fmt-revr '(:weight normal :slant normal  :underline nil        :inverse-video t))
            (fmt-revb '(:weight ,bold  :slant normal  :underline nil        :inverse-video t))
            (fmt-revbb '(:weight ,bright-bold :slant normal :underline nil  :inverse-video t))
            (fmt-revbbu '(:weight ,bright-bold :slant normal  :underline ,underline :inverse-video t))
            (diff-added-fmt (solarized-diff-case fmt-revr fmt-undr fmt-bold ()))
            (diff-changed-fmt (solarized-diff-case fmt-revr fmt-undr fmt-bold ()))
            (diff-removed-fmt (solarized-diff-case fmt-revr fmt-bold fmt-bold ()))
            (diff-refine-change-fmt (solarized-diff-case fmt-revr fmt-undr fmt-bold ())))
       ,@body)))

(solarized-with-values
  (eval
   `(custom-theme-set-faces
     'solarized

     ;; ace jump mode
     '(ace-jump-face-background ((t (:foreground ,solarized-comment :background ,solarized-bg
                                                 :inverse-video nil))))
     '(ace-jump-face-foreground ((t (:foreground ,red :background ,solarized-bg
                                                 :inverse-video nil :weight bold))))

     ;; anzu
     '(anzu-mode-line ((t (:foreground ,solarized-emph :weight bold))))

     ;; auctex
     '(font-latex-bold-face ((t (:inherit bold :foreground ,solarized-emph))))
     '(font-latex-doctex-documentation-face ((t (:background unspecified))))
     '(font-latex-doctex-preprocessor-face
       ((t (:inherit (font-latex-doctex-documentation-face font-lock-builtin-face font-lock-preprocessor-face)))))
     '(font-latex-italic-face ((t (:inherit italic :foreground ,solarized-emph))))
     '(font-latex-math-face ((t (:foreground ,violet))))
     '(font-latex-sectioning-0-face ((t (:inherit font-latex-sectioning-1-face :height ,solarized-height-plus-1))))
     '(font-latex-sectioning-1-face ((t (:inherit font-latex-sectioning-2-face :height ,solarized-height-plus-1))))
     '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face :height ,solarized-height-plus-1))))
     '(font-latex-sectioning-3-face ((t (:inherit font-latex-sectioning-4-face :height ,solarized-height-plus-1))))
     '(font-latex-sectioning-4-face ((t (:inherit font-latex-sectioning-5-face :height ,solarized-height-plus-1))))
     '(font-latex-sectioning-5-face ((t (:inherit ,s-variable-pitch :foreground ,yellow :weight bold))))
     '(font-latex-sedate-face ((t (:foreground ,solarized-emph))))
     '(font-latex-slide-title-face ((t (:inherit (,s-variable-pitch font-lock-type-face) :weight bold :height ,solarized-height-plus-3))))
     '(font-latex-string-face ((t (:foreground ,cyan))))
     '(font-latex-subscript-face ((t (:height ,solarized-height-minus-1))))
     '(font-latex-superscript-face ((t (:height ,solarized-height-minus-1))))
     '(font-latex-verbatim-face ((t (:inherit fixed-pitch :foreground ,solarized-fg :slant italic))))
     '(font-latex-warning-face ((t (:inherit bold :foreground ,orange))))

     ;; auto complete
     '(ac-candidate-face ((t (:foreground ,orange :background ,solarized-hl))))
     '(ac-selection-face ((t (:foreground ,cyan-hc :background ,cyan-lc))))
     '(ac-candidate-mouse-face ((t (:foreground ,cyan-lc :background ,cyan-hc))))
     '(ac-completion-face ((t (:foreground ,solarized-emph :underline t))))
     '(ac-gtags-candidate-face ((t (:foreground ,blue :background ,solarized-hl))))
     '(ac-gtags-selection-face ((t (:foreground ,blue-hc :background ,blue-lc))))
     '(ac-yasnippet-candidate-face ((t (:foreground ,yellow :background ,solarized-hl))))
     '(ac-yasnippet-selection-face ((t (:foreground ,yellow-hc :background ,yellow-lc))))

     ;; basic coloring
     '(button ((t (:underline t))))
     '(link ((t (,@fmt-undr :foreground ,violet))))
     '(link-visited ((t (,@fmt-undr :foreground ,magenta))))
     '(default ((t (:foreground ,solarized-fg :background ,solarized-bg))))
     '(cursor ((t (:foreground ,solarized-bg :background ,solarized-fg))))
     '(escape-glyph ((t (:foreground ,red))))
     '(fringe ((t (:foreground ,solarized-comment :background ,solarized-hl))))
     '(header-line ((t (:foreground ,solarized-emph :background ,solarized-hl
                                    :inverse-video unspecified :underline unspecified
                                    :box (:line-width 1 :color ,solarized-hl :style unspecified)))))
     '(highlight ((t (:background ,solarized-hl))))
     '(lazy-highlight ((t (,@fmt-revr :foreground ,yellow :background ,solarized-bg))))
     '(match ((t (:foreground ,solarized-emph :background ,solarized-hl :weight bold))))
     '(menu ((t (:foreground ,solarized-fg :background ,solarized-hl))))
     '(minibuffer-prompt ((t (,@fmt-bold :foreground ,cyan))))
     '(mode-line ((t (:foreground ,s-mode-line-fg :background ,s-mode-line-bg
                                  :box (:line-width 1 :color ,s-mode-line-bg :style unspecified)))))
     '(mode-line-buffer-id ((t (:foreground ,s-mode-line-buffer-id-fg :weight bold))))
     '(mode-line-inactive ((t (:foreground ,s-mode-line-inactive-fg :background ,s-mode-line-inactive-bg
                                           :box (:line-width 1 :color ,s-mode-line-inactive-bc :style unspecified)))))

     '(mode-line-emphasis ((t (:inherit mode-line :foreground ,solarized-emph))))
     '(mode-line-highlight ((t (:inherit mode-line :foreground ,magenta :box nil ,@fmt-bold))))
     '(region ((t (:foreground ,solarized-comment :background ,solarized-bg ,@fmt-revbb))))
     '(secondary-selection ((t (:background ,solarized-hl))))
     '(trailing-whitespace ((t (,@fmt-revr :foreground ,red))))
     '(vertical-border ((t (:foreground ,solarized-fg))))

     ;; bookmarks
     '(bm-fringe-face ((t (:background ,orange :foreground ,solarized-bg))))
     '(bm-fringe-persistent-face ((t (:background ,blue :foreground ,solarized-bg))))

     ;; calfw
     '(cfw:face-day-title ((t (:background ,solarized-hl))))
     '(cfw:face-annotation ((t (:inherit cfw:face-day-title :foreground ,yellow))))
     '(cfw:face-default-content ((t (:foreground ,green))))
     '(cfw:face-default-day ((t (:inherit cfw:face-day-title :weight bold))))
     '(cfw:face-disable ((t (:inherit cfw:face-day-title :foreground ,solarized-comment))))
     '(cfw:face-grid ((t (:foreground ,solarized-comment))))
     '(cfw:face-header ((t (:foreground ,blue-hc :background ,blue-lc :weight bold))))
     '(cfw:face-holiday ((t (:background nil :foreground ,red :weight bold))))
     '(cfw:face-periods ((t (:foreground ,magenta))))
     '(cfw:face-select ((t (:background ,magenta-hc :foreground ,magenta-lc))))
     '(cfw:face-saturday ((t (:foreground ,cyan-hc :background ,cyan-lc))))
     '(cfw:face-sunday ((t (:foreground ,red-hc :background ,red-lc :weight bold))))
     '(cfw:face-title ((t (:inherit variable-pitch :foreground ,yellow
                                    :weight bold :height 2.0))))
     '(cfw:face-today ((t (:weight bold :background ,solarized-hl :foreground nil))))
     '(cfw:face-today-title ((t (:foreground ,yellow-hc :background ,yellow-lc
                                             :weight bold))))
     '(cfw:face-toolbar ((t (:foreground ,solarized-fg :background ,solarized-hl))))
     '(cfw:face-toolbar-button-off ((t (:foreground ,yellow-hc :background ,yellow-lc
                                                    :weight bold))))
     '(cfw:face-toolbar-button-on ((t (:foreground ,yellow-lc :background ,yellow-hc
                                                   :weight bold))))

     ;; comint
     '(comint-highlight-input ((t (:foreground ,yellow))))
     '(comint-highlight-prompt ((t (:foreground ,orange :background ,base03
                                                :weight bold))))

     ;; compilation
     '(compilation-column-face ((t (:foreground ,cyan :underline nil))))
     '(compilation-column-number ((t (:inherit font-lock-doc-face :foreground ,cyan :underline nil))))
     '(compilation-enter-directory-face ((t (:foreground ,green :underline nil))))
     '(compilation-error ((t (:inherit error :underline nil))))
     '(compilation-error-face ((t (:foreground ,red : :underline nil))))
     '(compilation-face ((t (:foreground ,solarized-fg :underline nil))))
     '(compilation-info ((t (:foreground ,solarized-comment :underline nil :bold nil))))
     '(compilation-info-face ((t (:foreground ,blue :underline nil))))
     '(compilation-leave-directory-face ((t (:foreground ,green :underline nil))))
     '(compilation-line-face ((t (:foreground ,green :underline nil))))
     '(compilation-line-number ((t (:foreground ,green :underline nil))))
     '(compilation-warning ((t (:inherit warning :underline nil))))
     '(compilation-warning-face ((t (:foreground ,yellow :weight normal :underline nil))))
     '(compilation-mode-line-exit ((t (:inherit compilation-info :foreground ,green :weight bold))))
     '(compilation-mode-line-fail ((t (:inherit compilation-error :foreground ,red :weight bold))))
     '(compilation-mode-line-run ((t (:foreground ,orange :weight bold))))

     ;; ctable
     '(ctbl:face-cell-select ((t (:foreground ,solarized-emph :background ,solarized-hl
                                              :underline ,solarized-emph :weight bold))))
     '(ctbl:face-continue-bar ((t (:foreground ,yellow :background ,solarized-hl))))
     '(ctbl:face-row-select ((t (:foreground ,solarized-fg :background ,solarized-hl
                                             :underline t))))

     ;; dired
     '(dired-directory ((t (:foreground ,blue :weight normal))))
     '(dired-flagged ((t (:foreground ,red))))
     '(dired-header ((t (:foreground ,solarized-bg :background ,blue))))
     '(dired-ignored ((t (:inherit shadow))))
     '(dired-mark ((t (:foreground ,yellow :weight bold))))
     '(dired-marked ((t (:foreground ,magenta :weight bold))))
     '(dired-perm-write ((t (:foreground ,solarized-fg :underline t))))
     '(dired-symlink ((t (:foreground ,cyan :slant italic))))
     '(dired-warning ((t (:foreground ,orange :underline t))))

     ;; diff
     '(diff-added ((t (:foreground ,green :background ,solarized-bg))))
     '(diff-changed ((t (:foreground ,blue :background ,solarized-bg))))
     '(diff-removed ((t (:foreground ,red :background ,solarized-bg))))
     '(diff-header ((t (:background ,solarized-bg))))
     '(diff-file-header ((t (:foreground ,solarized-fg :background ,solarized-bg :weight bold))))
     '(diff-refine-added ((t :foreground ,solarized-bg :background ,green)))
     '(diff-refine-change ((t :foreground ,solarized-bg :background ,blue)))
     '(diff-refine-removed ((t (:foreground ,solarized-bg :background ,red))))

     ;; dropdown
     '(dropdown-list-face ((t (:foreground ,cyan :background ,solarized-hl))))
     '(dropdown-list-selection-face ((t (:foreground ,cyan-hc :background ,cyan-lc))))

     ;; ecb
     '(ecb-default-highlight-face ((t (:foreground ,solarized-bg :background ,blue))))
     '(ecb-history-bucket-node-dir-soure-path-face ((t (:inherit ecb-history-bucket-node-face
                                                                 :foreground ,yellow))))
     '(ecb-source-in-directories-buffer-face ((t (:inherit ecb-directories-general-face
                                                           :foreground ,solarized-fg))))
     '(ecb-history-dead-buffer-face ((t (:inherit ecb-history-general-face
                                                  :foreground ,solarized-comment))))
     '(ecb-directory-not-accessible-face ((t (:inherit ecb-directories-general-face
                                                       :foreground ,solarized-comment))))
     '(ecb-bucket-node-face ((t (:inherit ecb-default-general-face :foreground ,blue
                                          :weight normal))))
     '(ecb-tag-header-face ((t (:background ,solarized-hl))))
     '(ecb-analyse-bucket-element-face ((t (:inherit ecb-analyse-general-face :foreground ,green))))
     '(ecb-directories-general-face ((t (:inherit ecb-default-general-face :height 1.0))))
     '(ecb-method-non-semantic-face ((t (:inherit ecb-methods-general-face :foreground ,cyan))))
     '(ecb-mode-line-prefix-face ((t (:foreground ,green))))
     '(ecb-tree-guide-line-face ((t (:inherit ecb-default-general-face :foreground ,solarized-hl
                                              :height 1.0))))

     ;; ediff
     '(ediff-current-diff-A ((t (:background ,orange-lc :foreground ,base03))))
     '(ediff-current-diff-B ((t (:background ,green-lc :foreground ,base03))))
     '(ediff-current-diff-C ((t (:background ,yellow-lc :foreground ,base03))))
     '(ediff-current-diff-Ancestor ((t (:background ,orange-lc :foreground ,base03))))
     '(ediff-fine-diff-A ((t (:background ,orange-hc :foreground ,base03))))
     '(ediff-fine-diff-B ((t (:background ,green-hc :foreground ,base03))))
     '(ediff-fine-diff-C ((t (:background ,yellow-hc :foreground ,base03))))
     '(ediff-fine-diff-Ancestor ((t (:background ,orange-hc :foreground ,base03))))
     '(ediff-even-diff-A ((t (:background ,base02 :foreground ,solarized-comment))))
     '(ediff-even-diff-B ((t (:background ,base02 :foreground ,solarized-comment))))
     '(ediff-even-diff-C ((t (:background ,base02 :foreground ,solarized-comment))))
     '(ediff-even-diff-Ancestor ((t (:background ,base02 :foreground ,solarized-comment))))
     '(ediff-odd-diff-A ((t (:background ,base02 :foreground ,solarized-emph))))
     '(ediff-odd-diff-B ((t (:background ,base02 :foreground ,solarized-emph))))
     '(ediff-odd-diff-C ((t (:background ,base02 :foreground ,solarized-emph))))
     '(ediff-odd-diff-Ancestor ((t (:background ,base02 :foreground ,solarized-emph))))

     ;; epc
     '(epc:face-title ((t (:foreground ,blue :background ,solarized-bg
                                       :weight normal :underline nil))))

     ;; erc
     '(erc-input-face ((t (:foreground ,solarized-comment))))
     '(erc-keyword-face ((t (,@fmt-bldi :foreground ,yellow))))
     '(erc-nick-default-face ((t (,@fmt-none :foreground ,cyan))))
     '(erc-my-nick-face ((t (:foreground ,blue))))
     '(erc-notice-face ((t (,@fmt-none :foreground ,blue))))
     '(erc-timestamp-face ((t (:foreground ,solarized-comment))))

     ;; eshell
     '(eshell-prompt ((t (,@fmt-bold :foreground ,green))))
     '(eshell-ls-archive ((t (:foreground ,magenta))))
     '(eshell-ls-backup ((t (:foreground ,yellow))))
     '(eshell-ls-clutter ((t (:foreground ,orange))))
     '(eshell-ls-directory ((t (:foreground ,blue))))
     '(eshell-ls-executable ((t (:foreground ,green))))
     '(eshell-ls-unreadable ((t (:foreground ,base00))))
     '(eshell-ls-missing ((t (:foreground ,red))))
     '(eshell-ls-product ((t (:foreground ,yellow))))
     '(eshell-ls-special ((t (:foreground ,violet))))
     '(eshell-ls-symlink ((t (:foreground ,cyan))))

     ;; flymake
     '(flymake-errline
       ((,'((supports :underline (:style wave)))
         (:underline (:style wave :color ,red) :inherit unspecified
                     :foreground ,red-hc :background ,red-lc))
        (t (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
     '(flymake-infoline
       ((,'((supports :underline (:style wave)))
         (:underline (:style wave :color ,green) :inherit unspecified
                     :foreground ,green-hc :background ,green-lc))
        (t (:foreground ,green-hc :background ,green-lc :weight bold :underline t))))
     '(flymake-warnline
       ((,'((supports :underline (:style wave)))
         (:underline (:style wave :color ,yellow) :inherit unspecified
                     :foreground ,yellow-hc :background ,yellow-lc))
        (t (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))

     ;; flycheck
     '(flycheck-error
       ((,'((supports :underline (:style wave)))
         (:underline (:style wave :color ,red) :inherit unspecified
                     :foreground ,red-hc :background ,red-lc))
        (t (:foreground ,red-hc :background ,red-lc :weight bold :underline t))))
     '(flycheck-warning
       ((,'((supports :underline (:style wave)))
         (:underline (:style wave :color ,yellow) :inherit unspecified
                     :foreground ,yellow-hc :background ,yellow-lc))
        (t (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline t))))
     '(flycheck-info
       ((,'((supports :underline (:style wave)))
         (:underline (:style wave :color ,blue) :inherit unspecified))
        (t (:foreground ,blue-hc :background ,blue-lc :weight bold :underline t))))
     '(flycheck-fringe-error ((t (:foreground ,red-hc :background ,red-lc :weight bold))))
     '(flycheck-fringe-warning ((t (:foreground ,yellow-hc :background ,yellow-lc :weight bold))))
     '(flycheck-fringe-info ((t (:foreground ,blue-hc :background ,blue-lc :weight bold))))

     ;; flyspell
     '(flyspell-duplicate
       ((,'((supports :underline (:style wave)))
         (:underline (:style wave :color ,yellow) :inherit unspecified))
        (t (:foreground ,yellow :weight bold :underline t))))
     '(flyspell-incorrect
       ((,'((supports :underline (:style wave)))
         (:underline (:style wave :color ,red) :inherit unspecified))
        (t (:foreground ,red :weight bold :underline t))))

     ;; font lock
     '(font-lock-builtin-face ((t (,@fmt-none :foreground ,green))))
     '(font-lock-comment-face ((t (,@fmt-ital :foreground ,solarized-comment))))
     '(font-lock-comment-delimiter-face ((t (,@fmt-ital :foreground ,solarized-comment))))
     '(font-lock-constant-face ((t (,@fmt-none :foreground ,cyan))))
     '(font-lock-doc-face ((t (,@fmt-ital :foreground ,solarized-comment))))
     '(font-lock-doc-string-face ((t (,@fmt-ital :foreground ,solarized-comment))))
     '(font-lock-function-name-face ((t (,@fmt-none :foreground ,blue))))
     '(font-lock-keyword-face ((t (,@fmt-none :foreground ,green))))
     '(font-lock-negation-char-face ((t (,@fmt-none :foreground ,red))))
     '(font-lock-preprocessor-face ((t (,@fmt-none :foreground ,orange))))
     '(font-lock-string-face ((t (,@fmt-none :foreground ,cyan))))
     '(font-lock-type-face ((t (,@fmt-none :foreground ,yellow))))
     '(font-lock-variable-name-face ((t (,@fmt-none :foreground ,blue))))
     '(font-lock-warning-face ((t (,@fmt-bold :foreground ,red))))

     ;; git gutter
     '(git-gutter:added ((t (:foreground ,solarized-bg :background ,green :weight bold))))
     '(git-gutter:deleted ((t (:foreground ,solarized-bg :background ,red :weight bold))))
     '(git-gutter:modified ((t (:foreground ,solarized-bg :background ,red :weight bold))))
     '(git-gutter:unchanged ((t (:foreground ,solarized-bg :background ,solarized-hl :weight bold))))

     ;; git gutter fringe
     '(git-gutter-fr:added ((t (:foreground ,green :weight bold))))
     '(git-gutter-fr:deleted ((t (:foreground ,red :weight bold))))
     '(git-gutter-fr:modified ((t (:foreground ,blue :weight bold))))

     ;; gnus
     '(gnus-group-mail-1 ((t (,@fmt-bold :foreground ,base3))))
     '(gnus-group-mail-1-empty ((t (:foreground ,base3))))
     '(gnus-group-mail-2 ((t (,@fmt-bold :foreground ,base2))))
     '(gnus-group-mail-2-empty ((t (:foreground ,base2))))
     '(gnus-group-mail-3 ((t (,@fmt-bold :foreground ,magenta))))
     '(gnus-group-mail-3-empty ((t (:foreground ,magenta))))
     '(gnus-group-mail-low ((t (,@fmt-bold :foreground ,base00))))
     '(gnus-group-mail-low-empty ((t (:foreground ,base00))))
     '(gnus-group-news-1 ((t (,@fmt-bold :foreground ,solarized-emph))))
     '(gnus-group-news-1-empty ((t (:foreground ,solarized-emph))))
     '(gnus-group-news-2 ((t (,@fmt-bold :foreground ,blue))))
     '(gnus-group-news-2-empty ((t (:foreground ,blue))))
     '(gnus-group-news-low ((t (,@fmt-bold :foreground ,violet))))
     '(gnus-group-news-low-empty ((t (:foreground ,violet))))
     '(gnus-header-content ((t (,@fmt-none :foreground ,solarized-comment))))
     '(gnus-header-from ((t (,@fmt-none :foreground ,base00))))
     '(gnus-header-name ((t (,@fmt-none :foreground ,solarized-comment))))
     '(gnus-header-newsgroups ((t (,@fmt-none :foreground ,solarized-hl))))
     '(gnus-header-subject ((t (,@fmt-none :foreground ,blue))))
     '(gnus-summary-cancelled ((t (,@fmt-none :foreground ,red))))
     '(gnus-summary-high-ancient ((t (,@fmt-bold :inherit gnus-summary-normal-ancient))))
     '(gnus-summary-high-read ((t (,@fmt-bold :inherit gnus-summary-normal-read))))
     '(gnus-summary-high-ticked ((t (,@fmt-bold :inherit gnus-summary-normal-ticked))))
     '(gnus-summary-high-unread ((t (,@fmt-bold :inherit gnus-summary-normal-unread))))
     '(gnus-summary-low-ancient ((t (,@fmt-ital :inherit gnus-summary-normal-ancient))))
     '(gnus-summary-low-read ((t (,@fmt-ital :inherit gnus-summary-normal-ancient))))
     '(gnus-summary-low-ticked ((t (,@fmt-ital :inherit gnus-summary-normal-ancient))))
     '(gnus-summary-low-unread ((t (,@fmt-ital :inherit gnus-summary-normal-unread))))
     '(gnus-summary-normal-ancient ((t (,@fmt-none :foreground ,blue))))
     '(gnus-summary-normal-read ((t (,@fmt-none :foreground ,solarized-comment))))
     '(gnus-summary-normal-ticked ((t (,@fmt-none :foreground ,red))))
     '(gnus-summary-normal-unread ((t (,@fmt-none :foreground ,blue))))
     '(gnus-summary-selected ((t (,@fmt-none :foreground ,solarized-bg :background ,yellow))))
     '(gnus-cite-1 ((t (,@fmt-none :foreground ,blue))))
     '(gnus-cite-2 ((t (,@fmt-none :foreground ,cyan))))
     '(gnus-cite-3 ((t (,@fmt-none :foreground ,yellow))))
     '(gnus-cite-4 ((t (,@fmt-none :foreground ,red))))
     '(gnus-cite-5 ((t (,@fmt-none :foreground ,orange))))
     '(gnus-cite-6 ((t (,@fmt-none :foreground ,violet))))
     '(gnus-cite-7 ((t (,@fmt-none :foreground ,green))))
     '(gnus-cite-8 ((t (,@fmt-none :foreground ,magenta))))
     '(gnus-cite-9 ((t (,@fmt-none :foreground ,base00))))
     '(gnus-cite-10 ((t (,@fmt-none :foreground ,solarized-comment))))
     '(gnus-cite-11 ((t (,@fmt-none :foreground ,solarized-hl))))
     '(gnus-signature ((t (,@fmt-none :foreground ,solarized-comment))))

     ;; grep
     '(grep-context-face ((t (:foreground ,solarized-fg))))
     '(grep-error-face ((t (:foreground ,red :weight bold :underline t))))
     '(grep-hit-face ((t (:foreground ,blue))))
     '(grep-match-face ((t (:foreground ,orange :weight bold))))

     ;; guide-key
     '(guide-key/highlight-command-face ((t (:foreground ,blue))))
     '(guide-key/key-face ((t (:foreground ,base01))))
     '(guide-key/prefix-command-face ((t (:foreground ,green))))

     ;; helm
     '(helm-apt-deinstalled ((t (:foreground ,solarized-comment))))
     '(helm-apt-installed ((t (:foreground ,green))))
     '(helm-bookmark-directory ((t (:inherit helm-ff-directory))))
     '(helm-bookmark-file ((t (:foreground ,solarized-fg))))
     '(helm-bookmark-gnus ((t (:foreground ,cyan))))
     '(helm-bookmark-info ((t (:foreground ,green))))
     '(helm-bookmark-man ((t (:foreground ,violet))))
     '(helm-bookmark-w3m ((t (:foreground ,yellow))))
     '(helm-bookmarks-su ((t (:foreground ,orange))))
     '(helm-buffer-not-saved ((t (:foreground ,orange))))
     '(helm-buffer-saved-out ((t (:foreground ,red :background ,solarized-bg :inverse-video t))))
     '(helm-buffer-size ((t (:foreground ,solarized-comment))))
     '(helm-candidate-number ((t (:foreground ,solarized-emph :background ,solarized-hl :bold t))))
     '(helm-ff-directory ((t (:foreground ,blue :background ,solarized-bg))))
     '(helm-ff-executable ((t (:foreground ,green))))
     '(helm-ff-file ((t (:foreground ,solarized-fg :background ,solarized-bg))))
     '(helm-ff-invalid-symlink ((t (:foreground ,orange :background ,solarized-bg :slant italic))))
     '(helm-ff-prefix ((t (:foreground ,solarized-bg :background ,yellow))))
     '(helm-ff-symlink ((t (:foreground ,cyan))))
     '(helm-grep-file ((t (:foreground ,cyan :underline t))))
     '(helm-grep-finish ((t (:foreground ,green))))
     '(helm-grep-lineno ((t (:foreground ,orange))))
     '(helm-grep-match ((t (:inherit match))))
     '(helm-grep-running ((t (:foreground ,red))))
     '(helm-header ((t (:inherit header-line))))
     '(helm-lisp-completion-info ((t (:foreground ,solarized-fg))))
     '(helm-lisp-show-completion ((t (:foreground ,yellow :background ,solarized-hl :bold t))))
     '(helm-M-x-key ((t (:foreground ,orange :underline t))))
     '(helm-moccur-buffer ((t (:foreground ,cyan :underline t))))
     '(helm-match ((t (:inherit match))))
     '(helm-selection ((t (:background ,solarized-hl ,@fmt-undr))))
     '(helm-selection-line ((t (:foreground ,solarized-emph :background ,solarized-hl :underline nil))))
     '(helm-separator ((t (:foreground ,red))))
     '(helm-source-header ((t (:foreground ,solarized-bg :background ,blue-lc :underline nil))))
     '(helm-time-zone-current ((t (:foreground ,green))))
     '(helm-time-zone-home ((t (:foreground ,red))))
     '(helm-visible-mark ((t (:foreground ,magenta :background ,solarized-bg :bold t))))

     ;; helm-swoop
     '(helm-swoop-target-line-face ((t (:foreground unspecified :background ,base02))))
     '(helm-swoop-target-line-block-face ((t (:foreground unspecified :background ,base02))))
     '(helm-swoop-target-word-face ((t (:foreground ,magenta :background unspecified))))

     ;; hl-line
     '(hl-line ((t (:underline ,opt-under :background ,solarized-hl))))

     ;; ido mode
     '(ido-first-match ((t (:foreground ,yellow :weight normal))))
     '(ido-only-match ((t (:foreground ,solarized-bg :background ,yellow :weight normal))))
     '(ido-subdir ((t (:foreground ,blue))))
     '(ido-incomplete-regexp ((t (:foreground ,red :weight bold))))
     '(ido-indicator ((t (:foreground ,solarized-bg :background ,red :width condensed))))
     '(ido-virtual ((t (:foreground ,cyan))))

     ;; info+
     '(info-file
       ((t (:foreground ,yellow-lc :background ,(if (eq solarized-background 'light) base02 base2)))))
     '(info-menu
       ((t (:foreground ,yellow-lc :background ,(if (eq solarized-background 'light) base02 base2)))))
     '(info-quoted-name ((t (:foreground ,green-lc :inherit font-lock-string-face))))
     '(info-string ((t (:foreground ,orange-lc :inherit font-lock-string-face))))
     '(info-single-quote ((t (:foreground ,red-lc :inherit font-lock-string-face))))
     '(info-quoted-name ((t (:foreground ,violet-lc :inherit font-lock-string-face))))
     '(info-string ((t (:foreground ,orange-lc :inherit font-lock-string-face))))
     '(info-title-1 ((t (:foreground ,red-hc :weight bold))))
     '(info-title-2 ((t (:foreground ,blue-lc :weight bold))))
     '(info-title-3 ((t (:weight bold))))
     '(info-title-4 ((t (:weight bold))))
     '(info-command-ref-item
       ((t (:foreground ,green-lc :background ,(if (eq solarized-background 'light) base02 base2)))))
     '(info-constant-ref-item
       ((t (:foreground ,red-hc :background ,(if (eq solarized-background 'light) base02 base2)))))
     '(info-function-ref-item
       ((t (:foreground ,cyan-lc :background ,(if (eq solarized-background 'light) base02 base2)))))
     '(info-macro-ref-item
       ((t (:foreground ,green-hc :background ,(if (eq solarized-background 'light) base02 base2)))))
     '(info-reference-item ((t (:background ,(if (eq solarized-background 'light) base02 base2)))))
     '(info-special-form-ref-item
       ((t (:foreground ,magenta-hc :background ,(if (eq solarized-background 'light) base02 base2)))))
     '(info-syntax-class-item
       ((t (:foreground ,magenta-lc :background ,solarized-hl))))
     '(info-user-option-ref-item
       ((t (:foreground ,red-hc :background ,(if (eq solarized-background 'light) base02 base2)))))
     '(info-user-option-ref-item
       ((t (:foreground ,orange-hc :background ,(if (eq solarized-background 'light) base02 base2)))))

     ;; isearch
     '(isearch ((t (,@fmt-stnd :foreground ,orange :background ,solarized-bg))))
     '(isearch-fail ((t (,@fmt-stnd :foreground ,orange :background ,solarized-bg))))

     ;; jabber
     '(jabber-roster-user-away ((t (,@fmt-ital :foreground ,green))))
     '(jabber-roster-user-online ((t (,@fmt-bold :foreground ,blue))))
     '(jabber-roster-user-dnd ((t (,@fmt-ital :foreground ,red))))
     '(jabber-chat-prompt-local ((t (,@fmt-bold :foreground ,blue))))
     '(jabber-chat-prompt-foreign ((t (,@fmt-bold :foreground ,red))))
     '(jabber-activity-face ((t (,@fmt-bold :foreground ,red))))
     '(jabber-activity-personal-face ((t (,@fmt-bold :foreground ,blue))))

     ;; jedi
     '(jedi:highlight-function-argument ((t (:inherit bold))))

     ;; js3 mode
     '(js3-warning-face ((t (:underline ,orange))))
     '(js3-error-face ((t (:foreground ,red))))
     '(js3-external-variable-face ((t (:foreground ,orange))))
     '(js3-function-param-face ((t (:foreground ,green))))
     '(js3-instance-member-face ((t (:foreground ,magenta))))
     '(js3-jsdoc-html-tag-delimiter-face ((t (:foreground ,cyan))))
     '(js3-jsdoc-html-tag-name-face ((t (:foreground ,orange))))
     '(js3-jsdoc-tag-face ((t (:foreground ,cyan))))
     '(js3-jsdoc-type-face ((t (:foreground ,blue))))
     '(js3-jsdoc-value-face ((t (:foreground ,violet))))
     '(js3-magic-paren-face ((t (:underline t))))
     '(js3-private-function-call-face ((t (:foreground ,yellow))))
     '(js3-private-member-face ((t (:foreground ,blue))))

     ;; linum
     '(linum ((t (:foreground ,solarized-comment :background ,solarized-hl))))

     ;; magit
     '(magit-section-title ((t (:foreground ,yellow :weight bold))))
     '(magit-branch ((t (:foreground ,orange :weight bold))))
     '(magit-item-highlight ((t (:background ,solarized-hl :weight unspecified))))
     '(magit-log-author ((t (:foreground ,cyan))))
     '(magit-log-graph ((t (:foreground ,solarized-comment))))
     '(magit-log-head-label-bisect-bad ((t (:foreground ,red-lc :background ,red-hc :box 1))))
     '(magit-log-head-label-bisect-good ((t (:foreground ,green-lc :background ,green-hc :box 1))))
     '(magit-log-head-label-default ((t (:background ,solarized-hl :box 1))))
     '(magit-log-head-label-local ((t (:foreground ,blue-hc :background ,blue-lc :box 1))))
     '(magit-log-head-label-patches ((t (:foreground ,red-hc :background ,red-lc :box 1))))
     '(magit-log-head-label-remote ((t (:foreground ,green-hc :background ,green-lc :box 1))))
     '(magit-log-head-label-tags ((t (:foreground ,yellow-hc :background ,yellow-lc :box 1))))
     '(magit-log-sha1 ((t (:foreground ,yellow))))
     '(magit-header ((t (:inherit default))))
     '(magit-log-sha1 ((t (:foreground ,yellow))))
     '(magit-cherry-equivalent ((t (:foreground ,magenta))))
     '(magit-cherry-unmatched ((t (:foreground ,cyan))))
     '(magit-process-ng ((t (:inherit magit-header :foreground ,red))))
     '(magit-process-ok ((t (:inherit magit-header :foreground ,green))))
     '(magit-signature-bad ((t (:foreground ,red))))
     '(magit-signature-good ((t (:foreground ,green))))
     '(magit-signature-none ((t (:inherit magit-log-message))))
     '(magit-signature-untrusted ((t (:foreground ,cyan))))
     '(magit-whitespace-warning-face ((t (:inherit trailing-whitespace))))

     ;; man
     '(Man-overstrike ((t (:foreground ,blue :weight bold))))
     '(Man-reverse ((t (:foreground ,orange))))
     '(Man-underline ((t (:foreground ,green :underline t))))

     ;; message
     '(message-cited-text ((t (:foreground ,base2))))
     '(message-header-name ((t (:foreground ,cyan))))
     '(message-header-other ((t (:foreground ,red))))
     '(message-header-to ((t (,@fmt-bold :foreground ,solarized-emph))))
     '(message-header-cc ((t (,@fmt-bold :foreground ,green))))
     '(message-header-newsgroups ((t (,@fmt-bldi :foreground ,yellow))))
     '(message-header-subject ((t (:foreground ,base00))))
     '(message-header-xheader ((t (:foreground ,violet))))
     '(message-mml ((t (:foreground ,blue))))
     '(message-separator ((t (:foreground ,base3))))

     ;; org
     '(org-agenda-structure
       ((t (:foreground ,base1 :background ,base02
                        :weight bold :slant normal :inverse-video nil
                        :height ,solarized-height-plus-1
                        :underline nil
                        :box (:line-width 2 :color ,base03)))))
     '(org-agenda-calendar-event ((t (:foreground ,base1))))
     '(org-agenda-calendar-sexp ((t (:foreground ,base0 :slant italic))))
     '(org-agenda-date
       ((t (:foreground ,base01 :background ,base03 :weight normal
                        :box (:line-width 2 :color ,base03)
                        :inverse-video nil :overline nil :slant normal
                        :height 1.0))))
     '(org-agenda-date-weekend
       ((t (:inherit org-agenda-date :inverse-video nil :background
                     unspecified
                     :foreground ,base01 :weight unspecified
                     :underline t :overline nil :box unspecified))))
     '(org-agenda-date-today
       ((t (:inherit org-agenda-date :inverse-video t :weight bold
                     :underline unspecified :overline nil :box unspecified
                     :foreground ,blue :background ,base03))))
     '(org-agenda-done ((t (:foreground ,base01 :slant italic))))
     '(org-archived ((t (:foreground ,base01 :weight normal))))
     '(org-block ((t (:foreground ,base01))))
     '(org-block-begin-line ((t (:foreground ,base01 :slant italic))))
     '(org-checkbox ((t (:background ,base03 :foreground ,base0
                                     :box (:line-width 1 :style released-button)))))
     '(org-code ((t (:foreground ,base01))))
     '(org-date ((t (:foreground ,blue :underline t))))
     '(org-done ((t (,@fmt-bold :foreground ,green))))
     '(org-ellipsis ((t (:foreground ,base01))))
     '(org-formula ((t (:foreground ,yellow))))
     '(org-headline-done ((t (:foreground ,green))))
     '(org-hide ((t (:foreground ,base03))))
     '(org-level-1 ((t (:inherit ,s-variable-pitch :foreground ,orange :height ,solarized-height-plus-4))))
     '(org-level-2 ((t (:inherit ,s-variable-pitch :foreground ,green :height ,solarized-height-plus-3))))
     '(org-level-3 ((t (:inherit ,s-variable-pitch :foreground ,blue :height ,solarized-height-plus-2))))
     '(org-level-4 ((t (:inherit ,s-variable-pitch :foreground ,yellow :height ,solarized-height-plus-1))))
     '(org-level-5 ((t (:inherit ,s-variable-pitch :foreground ,cyan))))
     '(org-level-6 ((t (:inherit ,s-variable-pitch :foreground ,green))))
     '(org-level-7 ((t (:inherit ,s-variable-pitch :foreground ,red))))
     '(org-level-8 ((t (:inherit ,s-variable-pitch :foreground ,blue))))
     '(org-link ((t (:foreground ,yellow :underline t))))
     '(org-sexp-date ((t (:foreground ,violet))))
     '(org-scheduled ((t (:foreground ,green))))
     '(org-scheduled-previously ((t (:foreground ,cyan))))
     '(org-scheduled-today ((t (:foreground ,blue :weight normal))))
     '(org-special-keyword ((t (:foreground ,base01 :weight bold))))
     '(org-table ((t (:foreground ,green))))
     '(org-tag ((t (:weight bold))))
     '(org-time-grid ((t (:foreground ,base01))))
     '(org-todo ((t (:foreground ,red :weight bold))))
     '(org-upcoming-deadline ((t (:foreground ,yellow  :weight normal :underline nil))))
     '(org-warning ((t (:foreground ,orange :weight normal :underline nil))))
     ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
     '(org-habit-clear-face ((t (:background ,blue-lc :foreground ,blue-hc))))
     '(org-habit-clear-future-face ((t (:background ,blue-lc))))
     '(org-habit-ready-face ((t (:background ,green-lc :foreground ,green))))
     '(org-habit-ready-future-face ((t (:background ,green-lc))))
     '(org-habit-alert-face ((t (:background ,yellow :foreground ,yellow-lc))))
     '(org-habit-alert-future-face ((t (:background ,yellow-lc))))
     '(org-habit-overdue-face ((t (:background ,red :foreground ,red-lc))))
     '(org-habit-overdue-future-face ((t (:background ,red-lc))))
     ;; latest additions
     '(org-agenda-dimmed-todo-face ((t (:foreground ,base01))))
     '(org-agenda-restriction-lock ((t (:background ,yellow))))
     '(org-clock-overlay ((t (:background ,yellow))))
     '(org-column ((t (:background ,base02 :strike-through nil
                                   :underline nil :slant normal
                                   :weight normal :inherit default))))
     '(org-column-title ((t (:background ,base02 :underline t :weight bold))))
     '(org-date-selected ((t (:foreground ,red :inverse-video t))))
     '(org-document-info ((t (:foreground ,base0))))
     '(org-document-title ((t (:foreground ,base1  :weight bold :height ,solarized-height-plus-4))))
     '(org-drawer ((t (:foreground ,cyan))))
     '(org-footnote ((t (:foreground ,magenta :underline t))))
     '(org-latex-and-export-specials ((t (:foreground ,orange))))
     '(org-mode-line-clock-overrun ((t (:inherit mode-line :background ,red))))

     ;; outline
     '(outline-1 ((t (:inherit org-level-1))))
     '(outline-2 ((t (:inherit org-level-2))))
     '(outline-3 ((t (:inherit org-level-3))))
     '(outline-4 ((t (:inherit org-level-4))))
     '(outline-5 ((t (:inherit org-level-5))))
     '(outline-6 ((t (:inherit org-level-6))))
     '(outline-7 ((t (:inherit org-level-7))))
     '(outline-8 ((t (:inherit org-level-8))))

     ;; popup
     '(popup-face ((t (:foreground ,solarized-fg :background ,solarized-hl))))
     '(popup-isearch-match ((t (:foreground ,solarized-bg :background ,yellow))))
     '(popup-menu-face ((t (:foreground ,solarized-fg :background ,solarized-hl))))
     '(popup-menu-mouse-face ((t (:foreground ,solarized-fg :background ,blue))))
     '(popup-menu-selection-face ((t (:foreground ,solarized-bg :background ,magenta))))
     '(popup-scroll-bar-background-face ((t (:background ,solarized-comment))))
     '(popup-scroll-bar-foreground-face ((t (:background ,solarized-emph))))
     '(popup-tip-face ((t (:foreground ,solarized-fg :background ,solarized-hl))))

     ;; pretty mode plus
     '(pretty-mode-symbol-face  ((t (:foreground ,green))))

     ;; rainbow-delimiters
     '(rainbow-delimiters-depth-1-face ((t (:foreground ,cyan))))
     '(rainbow-delimiters-depth-2-face ((t (:foreground ,yellow))))
     '(rainbow-delimiters-depth-3-face ((t (:foreground ,blue))))
     '(rainbow-delimiters-depth-4-face ((t (:foreground ,orange))))
     '(rainbow-delimiters-depth-5-face ((t (:foreground ,green))))
     '(rainbow-delimiters-depth-6-face ((t (:foreground ,yellow))))
     '(rainbow-delimiters-depth-7-face ((t (:foreground ,blue))))
     '(rainbow-delimiters-depth-8-face ((t (:foreground ,orange))))
     '(rainbow-delimiters-depth-9-face ((t (:foreground ,green))))
     '(rainbow-delimiters-depth-10-face ((t (:foreground ,yellow))))
     '(rainbow-delimiters-depth-11-face ((t (:foreground ,blue))))
     '(rainbow-delimiters-depth-12-face ((t (:foreground ,orange))))
     '(rainbow-delimiters-unmatched-face
       ((t (:foreground ,solarized-fg :background ,solarized-bg
                        :inverse-video t))))

     ;; rcirc
     '(rcirc-my-nick ((t (:foreground ,blue))))
     '(rcirc-other-nick ((t (:foreground ,green))))
     '(rcirc-bright-nick ((t (:foreground ,magenta))))
     '(rcirc-server ((t (:foreground ,solarized-emph))))
     '(rcirc-timestamp ((t (:foreground ,solarized-comment))))
     '(rcirc-nick-in-message ((t (:foreground ,orange))))
     '(rcirc-prompt ((t (:foreground ,yellow))))

     ;; rst mode
     '(rst-level-1-face ((t (:foreground ,solarized-bg :background ,yellow))))
     '(rst-level-2-face ((t (:foreground ,solarized-bg :background ,cyan))))
     '(rst-level-3-face ((t (:foreground ,solarized-bg :background ,blue))))
     '(rst-level-4-face ((t (:foreground ,solarized-bg :background ,violet))))
     '(rst-level-5-face ((t (:foreground ,solarized-bg :background ,magenta))))
     '(rst-level-6-face ((t (:foreground ,solarized-bg :background ,red))))

     ;; show-paren
     '(show-paren-mismatch ((t (,@fmt-bold :foreground ,red :background ,solarized-comment))))
     '(show-paren-match ((t (,@fmt-bold :foreground ,cyan :background ,solarized-hl))))

     ;; slime
     '(slime-repl-inputted-output-face ((t (:foreground ,red))))

     ;; smart mode line
     '(sml/global ((t (,@fmt-none
                       :foreground ,s-mode-line-fg
                       :box (:line-width 1 :color ,s-mode-line-bg :style unspecified)))))
     '(sml/modes ((t (:inherit sml/global :foreground ,solarized-comment))))
     '(sml/line-number ((t (:inherit sml/modes ,@fmt-bold))))
     '(sml/position-percentage ((t (:inherit sml/modes))))
     '(sml/col-number ((t (:inherit sml/modes))))
     '(sml/numbers-separator ((t (:inherit sml/modes))))
     '(sml/mule-info ((t (:inherit sml/modes))))
     '(sml/client ((t (:inherit sml/modes))))
     '(sml/prefix ((t (:inherit sml/modes))))
     '(sml/folder ((t (:inherit sml/modes))))
     '(sml/filename ((t (:inherit sml/global :foreground ,solarized-emph ,@fmt-bold))))
     '(sml/not-modified ((t (:inherit sml/modes))))
     '(sml/modified ((t (:inherit sml/global :foreground ,yellow))))
     '(sml/outside-modified ((t (:inherit sml/modified :foreground ,orange))))
     '(sml/read-only ((t (:inherit sml/modified :foreground ,red))))
     '(sml/vc ((t (:inherit sml/global :foreground ,yellow))))
     '(sml/vc-edited ((t (:inherit sml/vc :foreground ,orange))))
     '(sml/git ((t (:inherit sml/vc))))
     '(sml/charging ((t (:inherit sml/global :foreground ,green))))
     '(sml/discharging ((t (:inherit sml/global :foreground ,red))))
     '(sml/time ((t (:inherit sml/modes))))

     ;; syslog mode
     '(syslog-ip-face ((t (:foreground ,yellow :background unspecified))))
     '(syslog-hour-face ((t (:foreground ,green :background unspecified))))
     '(syslog-error-face ((t (:foreground ,red :background unspecified :weight bold))))
     '(syslog-warn-face ((t (:foreground ,orange :background unspecified :weight bold))))
     '(syslog-info-face ((t (:foreground ,blue :background unspecified :weight bold))))
     '(syslog-debug-face ((t (:foreground ,cyan :background unspecified :weight bold))))
     '(syslog-su-face ((t (:foreground ,magenta :background unspecified))))

     ;; term
     '(term-color-black ((t ( :foreground ,solarized-hl))))
     '(term-color-red ((t ( :foreground ,red))))
     '(term-color-green ((t ( :foreground ,green))))
     '(term-color-yellow ((t ( :foreground ,yellow))))
     '(term-color-blue ((t ( :foreground ,blue))))
     '(term-color-magenta ((t ( :foreground ,magenta))))
     '(term-color-cyan ((t ( :foreground ,cyan))))
     '(term-color-white ((t ( :foreground ,base00))))

     ;; undo tree
     '(undo-tree-visualizer-default-face ((t (:foreground ,base01 :background ,base03))))
     '(undo-tree-visualizer-current-face ((t (:foreground ,blue :inverse-video t))))
     '(undo-tree-visualizer-active-branch-face
       ((t (:foreground ,base1 :background ,base03 :weight bold))))
     '(undo-tree-visualizer-register-face ((t (:foreground ,yellow))))
     '(undo-tree-visualizer-unmodified-face ((t (:foreground ,green))))

     ;; w3m
     '(w3m-anchor ((t (:inherit link))))
     '(w3m-arrived-anchor ((t (:inherit link-visited))))
     '(w3m-form ((t (:background ,base03 :foreground ,base0))))
     '(w3m-header-line-location-title ((t (:background ,base02 :foreground ,yellow))))
     '(w3m-header-line-location-content ((t (:background ,base02 :foreground ,base0))))
     '(w3m-bold ((t (:foreground ,base1 :weight bold))))
     '(w3m-image-anchor ((t (:background ,base03 :foreground ,cyan :inherit link))))
     '(w3m-image ((t (:background ,base03 :foreground ,cyan))))
     '(w3m-lnum-minibuffer-prompt ((t (:foreground ,base1))))
     '(w3m-lnum-match ((t (:background ,base02))))
     '(w3m-lnum ((t (:underline nil :bold nil :foreground ,red))))
     '(w3m-session-select ((t (:foreground ,base0))))
     '(w3m-session-selected ((t (:foreground ,base1 :bold t :underline t))))
     '(w3m-tab-background ((t (:background ,base03 :foreground ,base0))))
     '(w3m-tab-selected-background ((t (:background ,base03 :foreground ,base0))))
     '(w3m-tab-mouse ((t (:background ,base02 :foreground ,yellow))))
     '(w3m-tab-selected ((t (:background ,base02 :foreground ,base1 :bold t))))
     '(w3m-tab-unselected ((t (:background ,base02 :foreground ,base0))))
     '(w3m-tab-selected-retrieving ((t (:background ,base02 :foreground ,red))))
     '(w3m-tab-unselected-retrieving ((t (:background ,base02 :foreground ,orange))))
     '(w3m-tab-unselected-unseen ((t (:background ,base02 :foreground ,violet))))

     ;; wanderlust
     '(wl-highlight-folder-few-face ((t (:foreground ,red))))
     '(wl-highlight-folder-many-face ((t (:foreground ,red))))
     '(wl-highlight-folder-path-face ((t (:foreground ,orange))))
     '(wl-highlight-folder-unread-face ((t (:foreground ,blue))))
     '(wl-highlight-folder-zero-face ((t (:foreground ,solarized-fg))))
     '(wl-highlight-folder-unknown-face ((t (:foreground ,blue))))
     '(wl-highlight-message-citation-header ((t (:foreground ,red))))
     '(wl-highlight-message-cited-text-1 ((t (:foreground ,red))))
     '(wl-highlight-message-cited-text-2 ((t (:foreground ,green))))
     '(wl-highlight-message-cited-text-3 ((t (:foreground ,blue))))
     '(wl-highlight-message-cited-text-4 ((t (:foreground ,blue))))
     '(wl-highlight-message-header-contents-face ((t (:foreground ,green))))
     '(wl-highlight-message-headers-face ((t (:foreground ,red))))
     '(wl-highlight-message-important-header-contents ((t (:foreground ,green))))
     '(wl-highlight-message-header-contents ((t (:foreground ,green))))
     '(wl-highlight-message-important-header-contents2 ((t (:foreground ,green))))
     '(wl-highlight-message-signature ((t (:foreground ,green))))
     '(wl-highlight-message-unimportant-header-contents ((t (:foreground ,solarized-fg))))
     '(wl-highlight-summary-answered-face ((t (:foreground ,blue))))
     '(wl-highlight-summary-disposed-face ((t (:foreground ,solarized-fg
                                                           :slant italic))))
     '(wl-highlight-summary-new-face ((t (:foreground ,blue))))
     '(wl-highlight-summary-normal-face ((t (:foreground ,solarized-fg))))
     '(wl-highlight-summary-thread-top-face ((t (:foreground ,yellow))))
     '(wl-highlight-thread-indent-face ((t (:foreground ,magenta))))
     '(wl-highlight-summary-refiled-face ((t (:foreground ,solarized-fg))))
     '(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))

     ;; web mode
     '(web-mode-builtin-face ((t (:foreground ,red))))
     '(web-mode-comment-face ((t (:foreground ,base01))))
     '(web-mode-constant-face ((t (:foreground ,blue :weight bold))))
     '(web-mode-current-element-highlight-face
       ((t (:underline unspecified :weight unspecified :background ,base02))))
     '(web-mode-css-at-rule-face ((t (:foreground ,violet :slant italic))))
     '(web-mode-css-pseudo-class-face ((t (:foreground ,green :slant italic))))
     '(web-mode-doctype-face ((t (:foreground ,base01 :slant italic :weight bold))))
     '(web-mode-folded-face ((t (:underline t))))
     '(web-mode-function-name-face ((t (:foreground ,blue))))
     '(web-mode-html-attr-name-face ((t (:foreground ,blue :slant normal))))
     '(web-mode-html-attr-value-face ((t (:foreground ,cyan :slant italic))))
     '(web-mode-html-tag-face ((t (:foreground ,green))))
     '(web-mode-keyword-face ((t (:foreground ,yellow :weight normal))))
     '(web-mode-preprocessor-face ((t (:foreground ,yellow :slant normal :weight unspecified))))
     '(web-mode-string-face ((t (:foreground ,cyan))))
     '(web-mode-type-face ((t (:foreground ,yellow))))
     '(web-mode-variable-name-face ((t (:foreground ,blue))))
     '(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
     '(web-mode-block-attr-name-face ((t (:inherit web-mode-html-attr-name-face))))
     '(web-mode-block-attr-value-face ((t (:inherit web-mode-html-attr-value-face))))
     '(web-mode-block-comment-face ((t (:inherit web-mode-comment-face))))
     '(web-mode-block-control-face ((t (:inherit font-lock-preprocessor-face))))
     '(web-mode-block-face ((t (:background unspecified))))
     '(web-mode-block-string-face ((t (:inherit web-mode-string-face))))
     '(web-mode-comment-keyword-face ((t (:box 1 :weight bold))))
     '(web-mode-css-color-face ((t (:inherit font-lock-builtin-face))))
     '(web-mode-css-function-face ((t (:inherit font-lock-builtin-face))))
     '(web-mode-css-priority-face ((t (:inherit font-lock-builtin-face))))
     '(web-mode-css-property-name-face ((t (:inherit font-lock-variable-name-face))))
     '(web-mode-css-selector-face ((t (:inherit font-lock-keyword-face))))
     '(web-mode-css-string-face ((t (:inherit web-mode-string-face))))
     '(web-mode-javascript-string-face ((t (:inherit web-mode-string-face))))
     '(web-mode-json-context-face ((t (:foreground ,violet))))
     '(web-mode-json-key-face ((t (:foreground ,violet))))
     '(web-mode-json-string-face ((t (:inherit web-mode-string-face))))
     '(web-mode-param-name-face ((t (:foreground ,base0))))
     '(web-mode-part-comment-face ((t (:inherit web-mode-comment-face))))
     '(web-mode-part-face ((t (:inherit web-mode-block-face))))
     '(web-mode-part-string-face ((t (:inherit web-mode-string-face))))
     '(web-mode-symbol-face ((t (:foreground ,yellow))))
     '(web-mode-whitespace-face ((t (:background ,red))))
     '(web-mode-html-tag-bracket-face ((t (:foreground ,base01))))

     ;; whitespace-mode
     '(whitespace-space
       ((t (:foreground ,solarized-comment :background unspecified :inverse-video unspecified :slant italic))))
     '(whitespace-hspace
       ((t (:foreground ,solarized-emph :background unspecified :inverse-video unspecified))))
     '(whitespace-tab
       ((t (:foreground ,red :background unspecified :inverse-video unspecified :weight bold))))
     '(whitespace-newline
       ((t (:foreground ,solarized-comment :background unspecified :inverse-video unspecified))))
     '(whitespace-trailing
       ((t (:foreground ,orange-lc :background unspecified :inverse-video t))))
     '(whitespace-line
       ((t (:foreground ,magenta :background unspecified :inverse-video unspecified))))
     '(whitespace-space-before-tab
       ((t (:foreground unspecified :background ,red-lc :inverse-video unspecified))))
     '(whitespace-indentation
       ((t (:foreground ,yellow :background unspecified :inverse-video unspecified :weight bold))))
     '(whitespace-empty
       ((t (:foreground ,red-lc :background unspecified :inverse-video t))))
     '(whitespace-space-after-tab
       ((t (:foreground ,orange :background unspecified :inverse-video t :weight bold)))))))


(setq-default
 ;; fill column indicator
 fci-rule-color (solarized-find-color
                 (if (eq 'light solarized-background) 'base2 'base02)
                 solarized-dark-palette)

 ;; smart mode line
 sml/active-foreground-color (face-attribute 'mode-line :foreground)
 sml/active-background-color (face-attribute 'mode-line :background)
 sml/inactive-foreground-color (face-attribute 'mode-line-inactive :foreground)
 sml/inactive-background-color (face-attribute 'mode-line-inactive :background))

;;;###autoload
(when (and load-file-name
           (boundp 'custom-theme-load-path))
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'solarized)

;;; solarized-theme.el ends here
