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
            (s-fringe-fg solarized-comment)
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

     ;; annotate.el
     '(annotate-highlight ((t (:underlibe ,base02))))
     '(annotate-annotation ((t (:foreground ,base1 :background ,base02))))

     ;; anzu
     '(anzu-mode-line ((t (:foreground ,solarized-emph :weight bold))))

     ;; artbollocks-mode
     '(artbollocks-face ((t (:foreground ,solarized-emph :background unspecified :inverse-video unspecified))))
     '(artbollocks-lexical-illusions-face
       ((t (:foreground ,orange-lc :background unspecified :inverse-video t))))
     '(artbollocks-passive-voice-face
       ((t (:foreground ,solarized-comment :background unspecified :inverse-video unspecified :slant italic))))
     '(artbollocks-weasel-words-face
       ((t (:foreground ,red :background unspecified :inverse-video unspecified :weight bold))))

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
     '(ac-selection-face ((t (:foreground ,base2 :background ,orange))))
     '(ac-candidate-mouse-face ((t (:foreground ,base3 :background ,orange))))
     '(ac-completion-face ((t (:foreground ,solarized-emph :underline t))))
     '(ac-gtags-candidate-face ((t (:foreground ,blue :background ,solarized-hl))))
     '(ac-gtags-selection-face ((t (:foreground ,blue-hc :background ,blue-lc))))
     '(ac-yasnippet-candidate-face ((t (:foreground ,yellow :background ,solarized-hl))))
     '(ac-yasnippet-selection-face ((t (:foreground ,yellow-hc :background ,yellow-lc))))

     ;; basic coloring
     '(button ((t (:underline t))))
     '(link ((t (,@fmt-undb :foreground ,yellow))))
     '(link-visited ((t (,@fmt-undr :foreground ,yellow))))
     '(default ((t (:foreground ,solarized-fg :background ,solarized-bg))))
     '(shadow ((t (:foreground ,base01))))
     '(cursor ((t (:foreground ,solarized-bg :background ,solarized-fg
                               :inverse-video t))))
     '(escape-glyph ((t (:foreground ,violet))))
     '(fringe ((t (:foreground ,s-fringe-fg :background ,s-fringe-bg))))
     '(success ((t (:foreground ,green))))
     '(warning ((t (:foreground ,yellow))))
     '(error ((t (:foreground ,orange))))
     '(widget-field ((t (:background ,base02))))
     '(highlight ((t (:background ,solarized-hl))))
     '(lazy-highlight ((t (:foreground ,base03 :background ,yellow
                                       :weight normal))))
     '(match ((t (:background ,base02 :foreground ,base1 :weight bold))))

     ;; bookmarks
     '(bm-fringe-face ((t (:background ,orange :foreground ,solarized-bg))))
     '(bm-fringe-persistent-face ((t (:background ,blue :foreground ,solarized-bg))))

     ;; calendar
     '(calendar-today ((t (:weight bold :background ,solarized-hl :foreground nil))))
     '(holiday ((t (:background nil :foreground ,red-lc :weight bold))))
     '(diary ((t (:foreground ,yellow))))
     '(calendar-weekday-header ((t (:inherit default))))
     '(calendar-weekend-header ((t (:inherit font-lock-comment-face))))
     '(calendar-month-header ((t (:foreground ,yellow :weight bold :height 1.1))))

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
     '(cfw:face-title ((t (:inherit ,s-variable-pitch :foreground ,yellow
                                    :weight bold :height 2.0))))
     '(cfw:face-today ((t (:weight bold :background ,solarized-hl :foreground nil))))
     '(cfw:face-today-title ((t (:foreground ,yellow-hc :background ,yellow-lc
                                             :weight bold))))
     '(cfw:face-toolbar ((t (:foreground ,solarized-fg :background ,solarized-hl))))
     '(cfw:face-toolbar-button-off ((t (:foreground ,yellow-hc :background ,yellow-lc
                                                    :weight bold))))
     '(cfw:face-toolbar-button-on ((t (:foreground ,yellow-lc :background ,yellow-hc
                                                   :weight bold))))

     ;; cogre
     '(cogre-box-face ((t (:foreground ,solarized-emph :background ,solarized-hl))))
     '(cogre-box-first-face ((t (:inherit cogre-box-face :overline t))))
     '(cogre-box-last-face ((t (:inherit cogre-box-face :underline t))))

     ;; comint
     '(comint-highlight-input ((t (:foreground ,yellow))))
     '(comint-highlight-prompt ((t (:foreground ,orange :background ,base03
                                                :weight bold))))

     ;; company-mode
     '(company-template-field ((t (:background ,yellow :foreground ,base02))))
     '(company-echo-common ((t (:inherit company-echo :underline t))))
     '(company-tooltip ((t (:background ,base02 :foreground ,cyan))))
     '(company-tooltip-selection ((t (:background ,cyan-lc :foreground ,cyan-hc))))
     '(company-tooltip-mouse ((t (:background ,cyan-hc :foreground ,cyan-lc))))
     '(company-tooltip-common ((t (:foreground ,base1 :underline t))))
     '(company-tooltip-common-selection ((t (:foreground ,base1 :underline t))))
     '(company-tooltip-annotation ((t (:foreground ,base1 :background ,base02))))
     '(company-scrollbar-fg ((t (:foreground ,base03 :background ,base0))))
     '(company-scrollbar-bg ((t (:background ,base02 :foreground ,cyan))))
     '(company-preview ((t (:background ,base02 :foreground ,cyan))))
     '(company-preview-common ((t (:foreground ,base1 :underline t))))
     '(company-preview-search ((t (:inherit company-preview :slant italic))))

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

     ;; cperl
     '(cperl-nonoverridable-face ((t (:foreground nil :inherit font-lock-keyword-face))))
     '(cperl-array-face ((t (:foregorund nil :inherit font-lock-variable-name-face))))
     '(cperl-hash-face ((t (:foregorund nil :inherit font-lock-variable-name-face))))

     ;; cscope
     '(cscope-file-face ((t (:foreground ,green :weight bold))))
     '(cscope-function-face ((t (:foreground ,blue))))
     '(cscope-line-number-face ((t (:foreground ,solarized-comment))))
     '(cscope-mouse-face ((t (:background ,blue :foreground ,solarized-fg))))
     '(cscope-line-face ((t (:foreground ,solarized-fg))))
     '(cscope-separator-face ((t (:foreground ,solarized-emph))))

     ;; ctable
     '(ctbl:face-cell-select ((t (:foreground ,solarized-emph :background ,solarized-hl
                                              :underline ,solarized-emph :weight bold))))
     '(ctbl:face-continue-bar ((t (:foreground ,yellow :background ,solarized-hl))))
     '(ctbl:face-row-select ((t (:foreground ,solarized-fg :background ,solarized-hl
                                             :underline t))))

     ;; csv
     '(csv-separator-face ((t (,@fmt-ital :foreground ,solarized-comment))))

     ;; cua
     '(cua-global-mark ((t (:background ,yellow :foreground ,base03))))
     '(cua-rectangle ((t (:inherit region))))
     '(cua-rectangle-noselect ((t (:inherit region :background ,base02 :foreground ,base01))))

     ;; dired
     '(dired-directory ((t (:foreground ,blue :weight normal))))
     '(dired-flagged ((t (:foreground ,red))))
     '(dired-header ((t (:inherit header-line))))
     '(dired-ignored ((t (:inherit shadow))))
     '(dired-mark ((t (:foreground ,yellow :weight bold))))
     '(dired-marked ((t (:foreground ,magenta :weight bold))))
     '(dired-perm-write ((t (:foreground ,solarized-fg :underline t))))
     '(dired-symlink ((t (:foreground ,cyan :slant italic))))
     '(dired-warning ((t (:foreground ,orange :underline t))))
     ;; dired-async
     '(dired-async-message ((t (:background ,(if (eq solarized-background 'light) yellow-lc yellow)))))
     '(dired-async-mode-message ((t (:background ,(if (eq solarized-background 'light) red-lc red)))))
     ;; dired-efap
     '(dired-efap-face ((t (:box nil :background ,base02 :foreground ,base1
                                 :underline ,solarized-fg :weight bold))))
     ;; dired-k
     '(dired-k-added ((t (:inherit diff-added))))
     '(dired-k-commited ((t (:foreground ,green))))
     '(dired-k-directory ((t (:inherit dired-directory))))
     '(dired-k-ignored ((t (:inherit dired-ignored))))
     '(dired-k-modified ((t (:inherit diff-changed))))
     '(dired-k-untracked ((t (:foreground ,base01))))
     '(direx-k-ignored ((t (:inherit dired-k-ignored))))
     '(direx-k-modified ((t (:inherit dired-k-modified))))
     '(direx-k-untracked ((t (:inherit dired-k-untracked))))

     ;; diff
     '(diff-added ((t (:foreground ,green :background ,solarized-bg))))
     '(diff-changed ((t (:foreground ,blue :background ,solarized-bg))))
     '(diff-removed ((t (:foreground ,red :background ,solarized-bg))))
     '(diff-header ((t (:background ,solarized-bg))))
     '(diff-file-header ((t (:foreground ,solarized-fg :background ,solarized-bg :weight bold))))
     '(diff-refine-added ((t :foreground ,solarized-bg :background ,green)))
     '(diff-refine-changed ((t :foreground ,solarized-bg :background ,blue)))
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

     ;; edts
     '(edts-face-error-line
       ((t (:foreground ,red-hc :background ,red-lc :weight bold :underline (:style line :color ,red)))))
     '(edts-face-warning-line
       ((t (:foreground ,yellow-hc :background ,yellow-lc :weight bold :underline (:style line :color ,yellow)))))
     '(edts-face-error-fringe-bitmap
       ((t (:foreground ,red :background unspecified :weight bold))))
     '(edts-face-warning-fringe-bitmap
       ((t (:foreground ,yellow :background unspecified :weight bold))))
     '(edts-face-error-mode-line
       ((t (:background ,red :foreground unspecified))))
     '(edts-face-warning-mode-line
       ((t (:background ,yellow :foreground unspecified))))

     ;; ee
     '(ee-bookmarked ((t (:foreground ,base1))))
     '(ee-category ((t (:foreground ,blue))))
     '(ee-link ((t (:inherit link))))
     '(ee-link-visited ((t (:inherit link-visited))))
     '(ee-marked ((t (:foreground ,magenta :weight bold))))
     '(ee-omitted ((t (:foreground ,base01))))
     '(ee-shadow ((t (:inherit shadow))))

     ;; elfeed
     '(elfeed-search-date-face ((t (:foreground ,base01))))
     '(elfeed-search-feed-face ((t (:foreground ,base01))))
     '(elfeed-search-tag-face ((t (:foreground ,base0))))
     '(elfeed-search-title-face ((t (:foreground ,base0))))

     ;; elscreen
     '(elscreen-tab-background-face ((t (:background ,base03))))
     '(elscreen-tab-current-screen-face ((t (:background ,base02 :foreground ,base1)) (t (:underline t))))
     '(elscreen-tab-other-screen-face ((t (:background ,base03 :foreground ,base01))))
     '(elscreen-tab-control-face ((t (:background ,base03 :foreground ,base0))))

     ;; epa
     '(epa-mark ((t (:foreground ,magenta :weight bold))))
     '(epa-string ((t (:foreground ,violet))))
     '(epa-validity-disabled ((t (:inverse-video t :slant italic))))
     '(epa-validity-high ((t (:weight bold))))
     '(epa-validity-low ((t (:slant italic))))
     '(epa-validity-medium ((t (:slant italic))))

     ;; epc
     '(epc:face-title ((t (:foreground ,blue :background ,solarized-bg
                                       :weight normal :underline nil))))

     ;; erc
     '(erc-action-face ((t (:inherit erc-default-face))))
     '(erc-bold-face ((t (:weight bold))))
     '(erc-button ((t (:inherit button))))
     '(erc-command-indicator-face ((t (:inherit erc-default-face :weight bold))))
     '(erc-current-nick-face ((t (:foreground ,blue :weight bold))))
     '(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
     '(erc-default-face ((t (:foreground ,solarized-fg :background ,solarized-bg))))
     '(erc-direct-msg-face ((t (:inherit erc-default-face))))
     '(erc-error-face ((t (:inherit font-lock-warning-face))))
     '(erc-fool-face ((t (:foreground ,solarized-fg))))
     '(erc-header-line ((t (:foreground nil :background nil :inherit header-line))))
     '(erc-input-face ((t (:foreground ,solarized-emph))))
     '(erc-inverse-face ((t (:foreground ,solarized-bg :background ,solarized-fg))))
     '(erc-keyword-face ((t (:foreground ,yellow :weight bold))))
     '(erc-my-nick-face ((t (:foreground ,orange))))
     '(erc-nick-default-face ((t (:foreground ,cyan ,@fmt-none))))
     '(erc-nick-msg-face ((t (:inherit erc-nick-default-face :weight bold))))
     '(erc-notice-face ((t (:foreground ,solarized-comment ,@fmt-none))))
     '(erc-pal-face ((t (:inherit erc-my-nick-face :weight bold))))
     '(erc-prompt-face ((t (:foreground ,solarized-emph :background ,solarized-bg
                                        :weight bold))))
     '(erc-timestamp-face ((t (:foreground ,solarized-comment))))
     '(erc-underline-face ((t (:underline t))))

     ;; erc-colorize
     '(erc-distinct-1-face ((t (:foreground ,magenta))))
     '(erc-distinct-2-face ((t (:foreground ,blue))))
     '(erc-distinct-3-face ((t (:foreground ,cyan))))
     '(erc-distinct-4-face ((t (:foreground ,green))))
     '(erc-distinct-5-face ((t (:foreground ,yellow))))
     '(erc-distinct-6-face ((t (:foreground ,violet))))
     '(erc-distinct-7-face ((t (:foreground ,red))))

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

     ;; fic
     '(fic-author-face ((t (:background ,base03 :foreground ,orange
                                        :underline t :slant italic))))
     '(fic-face ((t (:background ,base03 :foreground ,orange
                                 :weight normal :slant italic))))
     '(font-lock-fic-face ((t (:background ,base03 :foreground ,orange
                                           :weight normal :slant italic))))

     ;; fixmee
     '(fixmee-notice-face ((t (:background nil :foreground ,base1
                                           :underline nil :slant italic
                                           :weight bold))))

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
     '(flyspell-duplicate-face
       ((,'((supports :underline (:style wave)))
         (:underline (:style wave :color ,yellow) :inherit unspecified))
        (t (:foreground ,yellow :weight bold :underline t))))
     '(flyspell-incorrect-face
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

     ;; function args
     '(fa-face-hint ((t (:inherit popup-face))))
     '(fa-face-hint-bold ((t (:inherit fa-face-hint :weight bold))))
     '(fa-face-type ((t (:inherit fa-face-hint :foreground ,yellow))))
     '(fa-face-type-bold ((t (:inherit fa-face-type :weight bold))))
     '(fa-face-semi ((t (:inherit fa-face-hint :foreground ,solarized-comment
                                  :slant italic))))
     '(fa-face-type-definition ((t (:inherit font-lock-type-face
                                             :slant italic))))
     '(fa-face-type-compound ((t (:inherit font-lock-type-face))))

     ;; git gutter
     '(git-gutter:added ((t (:foreground ,solarized-bg :background ,green :weight bold))))
     '(git-gutter:deleted ((t (:foreground ,solarized-bg :background ,red :weight bold))))
     '(git-gutter:modified ((t (:foreground ,solarized-bg :background ,red :weight bold))))
     '(git-gutter:unchanged ((t (:foreground ,solarized-bg :background ,solarized-hl :weight bold))))

     ;; git gutter fringe
     '(git-gutter-fr:added ((t (:foreground ,green :weight bold))))
     '(git-gutter-fr:deleted ((t (:foreground ,red :weight bold))))
     '(git-gutter-fr:modified ((t (:foreground ,blue :weight bold))))

     ;; git timemachine
     '(git-timemachine-minibuffer-author-face ((t (:foreground ,orange))))
     '(git-timemachine-minibuffer-detail-face ((t (:foreground ,yellow))))

     ;; gnus
     '(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))
     '(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
     '(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))
     '(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
     '(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))
     '(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
     '(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))
     '(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
     '(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))
     '(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))
     '(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))
     '(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))
     '(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))
     '(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))
     '(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))
     '(gnus-header-content ((t (:inherit message-header-other))))
     '(gnus-header-from ((t (:inherit message-header-other))))
     '(gnus-header-name ((t (:inherit message-header-name))))
     '(gnus-header-newsgroups ((t (:inherit message-header-other))))
     '(gnus-header-subject ((t (:inherit message-header-subject))))
     '(gnus-summary-cancelled ((t (:foreground ,solarized-comment :slant italic))))
     '(gnus-summary-high-ancient ((t (:foreground ,solarized-fg :weight bold))))
     '(gnus-summary-high-read ((t (:foreground ,solarized-fg :weight bold))))
     '(gnus-summary-high-ticked ((t (:foreground ,solarized-emph :weight bold))))
     '(gnus-summary-high-unread ((t (:foreground ,orange-hc :weight bold))))
     '(gnus-summary-low-ancient ((t (:foreground ,solarized-fg :slant italic))))
     '(gnus-summary-low-read ((t (:foreground ,solarized-fg :slant italic))))
     '(gnus-summary-low-ticked ((t (:foreground ,solarized-emph :slant italic))))
     '(gnus-summary-low-unread ((t (:foreground ,orange-lc :slant italic))))
     '(gnus-summary-normal-ancient ((t (:foreground ,solarized-fg))))
     '(gnus-summary-normal-read ((t (:foreground ,solarized-fg))))
     '(gnus-summary-normal-ticked ((t (:foreground ,solarized-emph))))
     '(gnus-summary-normal-unread ((t (:foreground ,orange))))
     '(gnus-summary-selected ((t (:foreground ,yellow :weight bold))))
     '(gnus-cite-1 ((t (:foreground ,blue))))
     '(gnus-cite-2 ((t (:foreground ,blue))))
     '(gnus-cite-3 ((t (:foreground ,blue))))
     '(gnus-cite-4 ((t (:foreground ,green))))
     '(gnus-cite-5 ((t (:foreground ,green))))
     '(gnus-cite-6 ((t (:foreground ,green))))
     '(gnus-cite-7 ((t (:foreground ,red))))
     '(gnus-cite-8 ((t (:foreground ,red))))
     '(gnus-cite-9 ((t (:foreground ,red))))
     '(gnus-cite-10 ((t (:foreground ,yellow))))
     '(gnus-cite-11 ((t (:foreground ,yellow))))
     '(gnus-group-news-1-empty ((t (:foreground ,yellow))))
     '(gnus-group-news-2-empty ((t (:foreground ,green))))
     '(gnus-group-news-3-empty ((t (:foreground ,green))))
     '(gnus-group-news-4-empty ((t (:foreground ,blue))))
     '(gnus-group-news-5-empty ((t (:foreground ,blue))))
     '(gnus-group-news-6-empty ((t (:foreground ,blue-lc))))
     '(gnus-group-news-low-empty ((t (:foreground ,base01))))
     '(gnus-signature ((t (:foreground ,yellow))))
     '(gnus-x-face ((t (:background ,base0 :foreground ,base03))))

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
     '(helm-bookmark-addressbook ((t (:foreground ,blue))))
     '(helm-bookmark-directory ((t (:inherit helm-buffer-directory))))
     '(helm-bookmark-file ((t (:foreground ,solarized-fg))))
     '(helm-bookmark-gnus ((t (:foreground ,cyan))))
     '(helm-bookmark-info ((t (:foreground ,green))))
     '(helm-bookmark-man ((t (:foreground ,violet))))
     '(helm-bookmark-w3m ((t (:foreground ,yellow))))
     '(helm-buffer-directory ((t (:foreground ,blue :background ,solarized-bg))))
     '(helm-buffer-not-saved ((t (:foreground ,orange))))
     '(helm-buffer-process ((t (:foreground ,orange :slant italic))))
     '(helm-buffer-saved-out ((t (:foreground ,red :background ,solarized-bg :inverse-video t))))
     '(helm-buffer-size ((t (:foreground ,solarized-comment))))
     '(helm-candidate-number ((t (:foreground ,solarized-emph :background ,solarized-hl :bold t))))
     '(helm-etags-file ((t (:foreground ,blue))))
     '(helm-ff-directory ((t (:foreground ,blue :background ,solarized-bg))))
     '(helm-ff-dotted-directory ((t (:foreground ,blue-lc :background ,solarized-bg))))
     '(helm-ff-dotted-symlink-directory ((t (:foreground ,cyan-lc :background ,solarized-bg))))
     '(helm-ff-executable ((t (:foreground ,green))))
     '(helm-ff-file ((t (:foreground ,solarized-fg :background ,solarized-bg))))
     '(helm-ff-invalid-symlink ((t (:foreground ,orange :background ,solarized-bg :slant italic))))
     '(helm-ff-prefix ((t (:foreground ,solarized-bg :background ,yellow))))
     '(helm-ff-symlink ((t (:foreground ,cyan))))
     '(helm-grep-file ((t (:foreground ,cyan :underline t))))
     '(helm-grep-finish ((t (:foreground ,green))))
     '(helm-grep-lineno ((t (:foreground ,orange))))
     '(helm-grep-match ((t (:foreground ,yellow :inherit match)))) ;:inherit match doesn't work?
     '(helm-header ((t (:inherit header-line))))
     '(helm-header-line-left-margin ((t (:inherit helm-header :foreground ,yellow))))
     '(helm-history-remote ((t (:foreground ,yellow))))
     '(helm-lisp-completion-info ((t (:foreground ,solarized-fg))))
     '(helm-lisp-show-completion ((t (:foreground ,yellow :background ,solarized-hl :bold t))))
     '(helm-locate-finish ((t (:foreground ,green))))
     '(helm-M-x-key ((t (:foreground ,orange :underline t))))
     '(helm-moccur-buffer ((t (:foreground ,cyan :underline t))))
     '(helm-match ((t (:foreground ,yellow :inherit match)))) ;:inherit match doesn't work?
     '(helm-prefarg ((t (:foreground ,green))))
     '(helm-resume-need-update ((t (:foreground ,red))))
     '(helm-selection ((t (:background ,solarized-hl ,@fmt-undr))))
     '(helm-selection-line ((t (:foreground ,solarized-emph :background ,solarized-hl :underline nil))))
     '(helm-separator ((t (:foreground ,red))))
     '(helm-source-header ((t (:foreground ,solarized-bg :background ,blue-lc :underline nil))))
     '(helm-time-zone-current ((t (:foreground ,green))))
     '(helm-time-zone-home ((t (:foreground ,red))))
     '(helm-visible-mark ((t (:foreground ,magenta :background ,solarized-bg :bold t))))

     ;; helm-css-scss
     '(helm-css-scss-selector-depth-face-1 ((t (:foreground ,base0))))
     '(helm-css-scss-selector-depth-face-2 ((t (:foreground ,blue))))
     '(helm-css-scss-selector-depth-face-3 ((t (:foreground ,cyan))))
     '(helm-css-scss-selector-depth-face-4 ((t (:foreground ,green))))
     '(helm-css-scss-selector-depth-face-5 ((t (:foreground ,yellow))))
     '(helm-css-scss-selector-depth-face-6 ((t (:foreground ,violet))))
     '(helm-css-scss-target-line-face ((t (:background unspecified :foreground ,magenta))))

     ;; helm-go-package
     '(helm-source-go-package-godoc-description ((t (:foreground ,base01))))

     ;; helm-swoop
     '(helm-swoop-target-line-face ((t (:foreground unspecified :background ,base02))))
     '(helm-swoop-target-line-block-face ((t (:foreground unspecified :background ,base02))))
     '(helm-swoop-target-word-face ((t (:foreground ,magenta :background unspecified))))

     ;; hi-lock-mode
     '(hi-yellow ((t (:foreground ,yellow-hc :background ,yellow-lc))))
     '(hi-pink ((t (:foreground ,magenta-hc :background ,magenta-lc))))
     '(hi-green ((t (:foreground ,green-hc :background ,green-lc))))
     '(hi-blue ((t (:foreground ,blue-hc :background ,blue-lc))))
     '(hi-black-b ((t (:foreground ,base1 :background ,base03 :weight bold))))
     '(hi-blue-b ((t (:weight bold :foreground ,cyan-hc :background ,cyan-lc))))
     '(hi-green-b ((t (:weight bold :foreground ,green-hc :background ,green-lc))))
     '(hi-red-b ((t (:weight bold :foreground ,red-hc :background ,red-lc))))
     '(hi-black-hb ((t (:weight bold :foreground ,base1 :background ,base02))))

     ;; hl-line
     '(hl-line ((t (:underline ,opt-under :background ,solarized-hl))))

     ;; hydra
     '(hydra-face-red ((t (:foreground ,red))))
     '(hydra-face-blue ((t (:foreground ,blue))))
     '(hydra-face-amaranth ((t (:foreground ,orange))))
     '(hydra-face-pink ((t (:foreground ,magenta))))
     '(hydra-face-teal ((t (:foreground ,cyan))))

     ;; ido mode
     '(ido-first-match ((t (:foreground ,yellow :weight normal))))
     '(ido-only-match ((t (:foreground ,solarized-bg :background ,yellow :weight normal))))
     '(ido-subdir ((t (:foreground ,blue))))
     '(ido-incomplete-regexp ((t (:foreground ,red :weight bold))))
     '(ido-indicator ((t (:foreground ,solarized-bg :background ,red :width condensed))))
     '(ido-virtual ((t (:foreground ,cyan))))

     ;; info+
     '(info-file
       ((t (:foreground ,yellow-lc :background ,base02))))
     '(info-menu
       ((t (:foreground ,yellow-lc :background ,base02))))
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
       ((t (:foreground ,green-lc :background ,base02))))
     '(info-constant-ref-item
       ((t (:foreground ,red-hc :background ,base02))))
     '(info-function-ref-item
       ((t (:foreground ,cyan-lc :background ,base02))))
     '(info-macro-ref-item
       ((t (:foreground ,green-hc :background ,base02))))
     '(info-reference-item ((t (:background ,base02))))
     '(info-special-form-ref-item
       ((t (:foreground ,magenta-hc :background ,base02))))
     '(info-syntax-class-item
       ((t (:foreground ,magenta-lc :background ,solarized-hl))))
     '(info-user-option-ref-item
       ((t (:foreground ,red-hc :background ,base02))))
     '(info-user-option-ref-item
       ((t (:foreground ,orange-hc :background ,base02))))

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
     ;; headings and diffs
     '(magit-section-highlight           ((t (:background ,base02))))
     '(magit-section-heading             ((t (:foreground ,yellow :weight bold))))
     '(magit-section-heading-selection   ((t (:foreground ,orange :weight bold))))
     '(magit-diff-file-heading           ((t (:weight bold))))
     '(magit-diff-file-heading-highlight ((t (:background ,base02 :weight bold))))
     '(magit-diff-file-heading-selection ((t (:background ,base02
                                              :foreground ,orange :weight bold))))
     '(magit-diff-hunk-heading           ((t (:foreground ,yellow-lc))))
     '(magit-diff-hunk-heading-highlight ((t (:foreground ,yellow))))
     '(magit-diff-hunk-heading-selection ((t (:foreground ,yellow-hc :weight bold))))
     '(magit-diff-lines-heading          ((t (:background ,orange
                                              :foreground ,base3))))
     '(magit-diff-context-highlight      ((t (:background ,base02))))
     '(magit-diffstat-added              ((t (:foreground ,green))))
     '(magit-diffstat-removed            ((t (:foreground ,red))))
     ;; popup
     '(magit-popup-heading             ((t (:foreground ,yellow :weight bold))))
     '(magit-popup-key                 ((t (:foreground ,base1 :weight bold))))
     '(magit-popup-argument            ((t (:foreground ,cyan :weight bold))))
     '(magit-popup-disabled-argument   ((t (:foreground ,base01 :weight normal))))
     '(magit-popup-option-value        ((t (:foreground ,cyan :weight bold))))
     ;; process
     '(magit-process-ok    ((t (:foreground ,green :weight bold))))
     '(magit-process-ng    ((t (:foreground ,red   :weight bold))))
     ;; log
     '(magit-log-author    ((t (:foreground ,base01 :weight bold))))
     '(magit-log-date      ((t (:foreground ,base01))))
     '(magit-log-graph     ((t (:foreground ,base1))))
     ;; sequence
     '(magit-sequence-pick ((t (:foreground ,yellow-lc))))
     '(magit-sequence-stop ((t (:foreground ,green))))
     '(magit-sequence-part ((t (:foreground ,yellow))))
     '(magit-sequence-head ((t (:foreground ,blue))))
     '(magit-sequence-drop ((t (:foreground ,red))))
     '(magit-sequence-done ((t (:foreground ,base01))))
     '(magit-sequence-onto ((t (:foreground ,base01))))
     ;; bisect
     '(magit-bisect-good ((t (:foreground ,green))))
     '(magit-bisect-skip ((t (:foreground ,yellow))))
     '(magit-bisect-bad  ((t (:foreground ,red))))
     ;; blame
     '(magit-blame-heading ((t (:background ,base02 :foreground ,violet
                                            :weight bold :slant normal :box (:color ,base02 :line-width 2)))))
     '(magit-blame-hash    ((t (:background ,base02 :foreground ,violet
                                            :weight normal :slant normal :box (:color ,base02 :line-width 2)))))
     '(magit-blame-name    ((t (:background ,base02 :foreground ,violet
                                            :weight normal :slant normal :box (:color ,base02 :line-width 2)))))
     '(magit-blame-date    ((t (:background ,base02 :foreground ,violet
                                            :weight bold :slant normal :box (:color ,base02 :line-width 2)))))
     '(magit-blame-summary ((t (:background ,base02 :foreground ,base0
                                            :weight bold :slant normal :box (:color ,base02 :line-width 2)))))
     ;; references etc.
     '(magit-dimmed         ((t (:foreground ,base01))))
     '(magit-hash           ((t (:foreground ,base01))))
     '(magit-tag            ((t (:foreground ,cyan   :weight bold))))
     '(magit-branch-remote  ((t (:foreground ,green  :weight bold))))
     '(magit-branch-local   ((t (:foreground ,blue   :weight bold))))
     '(magit-branch-current ((t (:foreground ,blue   :weight bold :box t))))
     '(magit-head           ((t (:foreground ,blue   :weight bold))))
     '(magit-refname        ((t (:background ,base02 :foreground ,base01 :weight bold))))
     '(magit-refname-stash  ((t (:background ,base02 :foreground ,base01 :weight bold))))
     '(magit-refname-wip    ((t (:background ,base02 :foreground ,base01 :weight bold))))
     '(magit-signature-good      ((t (:foreground ,green))))
     '(magit-signature-bad       ((t (:foreground ,red))))
     '(magit-signature-untrusted ((t (:foreground ,yellow))))
     '(magit-cherry-unmatched    ((t (:foreground ,cyan))))
     '(magit-cherry-equivalent   ((t (:foreground ,magenta))))
     '(magit-reflog-commit       ((t (:foreground ,green))))
     '(magit-reflog-amend        ((t (:foreground ,magenta))))
     '(magit-reflog-merge        ((t (:foreground ,green))))
     '(magit-reflog-checkout     ((t (:foreground ,blue))))
     '(magit-reflog-reset        ((t (:foreground ,red))))
     '(magit-reflog-rebase       ((t (:foreground ,magenta))))
     '(magit-reflog-cherry-pick  ((t (:foreground ,green))))
     '(magit-reflog-remote       ((t (:foreground ,cyan))))
     '(magit-reflog-other        ((t (:foreground ,cyan))))

     ;; makefile
     '(makefile-space ((t (:inherit font-lock-warning-face))))
     '(makefile-shell ((t (:inherit font-lock-preprocessor-face))))
     '(makefile-targets ((t (:inherit font-lock-function-name-face))))
     '(makefile-makepp-perl ((t (:inherit font-lock-string-face))))

     ;; man
     '(Man-overstrike ((t (:foreground ,blue :weight bold))))
     '(Man-reverse ((t (:foreground ,orange))))
     '(Man-underline ((t (:foreground ,green :underline t))))

     ;; markup-faces
     '(markup-gen-face ((t (:foreground ,base01))))
     '(markup-title-0-face ((t (:inherit ,s-variable-pitch :foreground ,orange :height ,solarized-height-plus-4))))
     '(markup-title-1-face ((t (:inherit ,s-variable-pitch :foreground ,green :height ,solarized-height-plus-3))))
     '(markup-title-2-face ((t (:inherit ,s-variable-pitch :foreground ,blue :height ,solarized-height-plus-2))))
     '(markup-title-3-face ((t (:inherit ,s-variable-pitch :foreground ,yellow :height ,solarized-height-plus-1))))
     '(markup-title-4-face ((t (:inherit ,s-variable-pitch :foreground ,cyan))))
     '(markup-title-5-face ((t (:inherit ,s-variable-pitch :foreground ,green))))
     '(markup-emphasis-face ((t (:inherit markup-gen-face :slant italic))))
     '(markup-strong-face ((t (:inherit markup-gen-face :weight bold))))
     '(markup-code-face ((t (:inherit (fixed-pitch markup-gen-face)))))
     '(markup-verbatim-face ((t (:inherit fixed-pitch :foreground ,base01))))
     '(markup-superscript-face ((t (:inherit markup-gen-face :height 0.8))))
     '(markup-subscript-face ((t (:inherit markup-gen-face :height 0.8))))
     '(markup-reference-face ((t (:inherit markup-gen-face :underline t))))
     '(markup-secondary-text-face ((t (:inherit markup-gen-face :foreground ,magenta :height 0.8))))
     '(markup-italic-face ((t (:inherit markup-gen-face :slant italic))))
     '(markup-bold-face ((t (:inherit markup-gen-face :weight bold))))
     '(markup-underline-face ((t (:inherit markup-gen-face :underline t))))
     '(markup-typewriter-face ((t (:inherit (fixed-pitch markup-gen-face)))))
     '(markup-small-face ((t (:inherit markup-gen-face :height 0.8))))
     '(markup-big-face ((t (:inherit markup-gen-face :height 1.3))))
     '(markup-meta-face ((t (:family "Monospace" :height 90 :stipple nil
                                     :inverse-video nil :box nil
                                     :strike-through nil :overline nil
                                     :underline nil :slant normal :weight normal
                                     :width normal :foundry "unknown"
                                     :foreground ,base01))))
     '(markup-meta-hide-face ((t (:inherit markup-meta-face :height 0.8))))
     '(markup-command-face ((t (:inherit markup-meta-face :weight bold))))
     '(markup-attribute-face ((t (:inherit markup-meta-face :slant italic))))
     '(markup-value-face ((t (:inherit markup-meta-face))))
     '(markup-complex-replacement-face ((t (:background ,base03 :foreground ,base0
                                                        :box (:line-width 1 :style released-button)))))
     '(markup-list-face ((t (:inherit markup-meta-face))))
     '(markup-table-face ((t (:inherit markup-meta-face :foreground ,green))))
     '(markup-table-row-face ((t (:inherit markup-table-face))))
     '(markup-table-cell-face ((t (:inherit markup-meta-face))))
     '(markup-anchor-face ((t (:inherit markup-meta-face :overline t))))
     '(markup-internal-reference-face ((t (:inherit markup-meta-face :underline t))))
     '(markup-comment-face ((t (:inherit (font-lock-comment-face markup-meta-face)))))
     '(markup-preprocessor-face ((t (:inherit (font-lock-preprocessor-face markup-meta-face)))))
     '(markup-replacement-face ((t (:family "Monospace" :foreground ,base01 :weight bold))))
     '(markup-passthrough-face ((t (:inherit (fixed-pitch markup-gen-face)))))
     '(markup-error-face ((t (:inherit (font-lock-warning-face)))))

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

     ;; mic-paren
     '(paren-face-match
       ((t (:foreground ,green :background unspecified :weight bold))))
     '(paren-face-mismatch
       ((t (:foreground ,base02 :background ,red :weight bold))))
     '(paren-face-no-match
       ((t (:foreground ,base02 :background ,red :weight bold))))

     ;; misc
     '(menu ((t (:foreground ,solarized-fg :background ,solarized-hl))))
     '(minibuffer-prompt ((t (,@fmt-bold :foreground ,cyan))))
     '(mode-line ((t (:foreground ,s-mode-line-fg :background ,s-mode-line-bg
                                  :box (:line-width 1 :color ,s-mode-line-bg :style unspecified)))))
     '(mode-line-buffer-id ((t (:foreground ,s-mode-line-buffer-id-fg :weight bold))))
     '(mode-line-inactive ((t (:foreground ,s-mode-line-inactive-fg :background ,s-mode-line-inactive-bg
                                           :box (:line-width 1 :color ,s-mode-line-inactive-bc :style unspecified)))))

     '(mode-line-emphasis ((t (:inherit mode-line :foreground ,solarized-emph))))
     '(mode-line-highlight ((t (:inherit mode-line :foreground ,magenta :box nil ,@fmt-bold))))
     '(header-line ((t (:foreground ,solarized-emph :background ,solarized-hl
                                    :inverse-video unspecified :underline unspecified
                                    :box (:line-width 1 :color ,solarized-hl :style unspecified)))))
     '(region ((t (:foreground ,base1 :background ,base03 ,@fmt-revbb))))
     '(secondary-selection ((t (:background ,base02))))
     '(trailing-whitespace ((t (,@fmt-revr :foreground ,red))))
     '(vertical-border ((t (:foreground ,solarized-fg))))

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

     ;; perspective
     '(persp-selected-face ((t (:foreground ,yellow))))

     ;; prodigy
     '(prodigy-green-face ((t (:foreground ,green))))
     '(prodigy-red-face ((t (:foreground ,orange))))
     '(prodigy-yellow-face ((t (:foreground ,yellow))))
     '(prodigy-line-face ((t (:foreground ,base02))))

     ;; popup
     '(popup-face ((t (:foreground ,solarized-fg :background ,solarized-hl))))
     '(popup-isearch-match ((t (:foreground ,solarized-bg :background ,yellow))))
     '(popup-menu-face ((t (:inherit ac-candidate-face))))
     '(popup-menu-mouse-face ((t (:inherit ac-candidate-mouse-face))))
     '(popup-menu-selection-face ((t (:inherit ac-selection-face))))
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

     ;; realgud
     '(debugger-running ((t (:foreground ,green :weight bold))))
     '(realgud-backtrace-number ((t (:inherit fringe))))
     '(realgud-overlay-arrow1 ((t (:foreground ,green :weight bold))))
     '(realgud-overlay-arrow2 ((t (:foreground ,orange :weight bold))))
     '(realgud-overlay-arrow3 ((t (:foreground ,blue :weight bold))))

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

     ;; smartparens
     '(sp-pair-overlay-face ((t (:background ,base02))))
     '(sp-wrap-overlay-face ((t (:background ,base02))))
     '(sp-wrap-overlay-opening-pair
       ((t (:inherit sp-wrap-overlay-face :foreground ,green))))
     '(sp-wrap-overlay-closing-pair
       ((t (:inherit sp-wrap-overlay-face :foreground ,orange))))
     '(sp-wrap-tag-overlay-face ((t (:background ,base02))))
     '(sp-show-pair-enclosing ((t (:inherit highlight))))
     '(sp-show-pair-match-face
       ((t (:background unspecified :foreground ,cyan :weight bold))))
     '(sp-show-pair-mismatch-face
       ((t (:foreground ,base02 :background ,red :weight bold))))

     ;; smerge
     '(smerge-upper ((t (:background ,orange-lc :foreground ,base03))))
     '(smerge-lower ((t (:background ,green-lc :foreground ,base03))))
     '(smerge-base ((t (:background ,orange-lc :foreground ,base03))))
     '(smerge-markers ((t (:background ,base02))))
     '(smerge-refined-changed ((t (:inherit diff-refine-changed))))
     '(smerge-refined-removed ((t (:inherit diff-refine-removed))))
     '(smerge-refined-added ((t (:inherit diff-refine-added))))

     ;; structured-haskell
     '(shm-current-face ((t (:background ,base02))))
     '(shm-quarantine-face ((t (:background ,base01))))

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

     ;; tooltip
     '(tooltip ((t (:background ,yellow-lc :foreground ,yellow-hc
                                :inherit ,s-variable-pitch))))

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
     '(wl-highlight-action-argument-face ((t (:foreground ,base0 :weight bold))))
     '(wl-highlight-demo-face ((t (:inherit default))))
     '(wl-highlight-folder-closed-face ((t (:foreground ,solarized-comment))))
     '(wl-highlight-folder-few-face ((t (:foreground ,orange))))
     '(wl-highlight-folder-killed-face ((t (:foreground ,solarized-comment :slant italic))))
     '(wl-highlight-folder-many-face ((t (:foreground ,orange-hc :weight bold))))
     '(wl-highlight-folder-opened-face ((t (:foreground ,solarized-fg))))
     '(wl-highlight-folder-path-face ((t (:inherit wl-highlight-folder-opened-face))))
     '(wl-highlight-folder-unknown-face ((t (:foreground ,blue :slant italic))))
     '(wl-highlight-folder-unread-face ((t (:foreground ,orange))))
     '(wl-highlight-folder-zero-face ((t (:foreground ,solarized-fg))))
     '(wl-highlight-header-separator-face ((t (:inherit header-line))))
     '(wl-highlight-logo-face ((t (:inherit default))))
     '(wl-highlight-message-citation-header ((t (:foreground ,red))))
     '(wl-highlight-message-cited-text-1 ((t (:foreground ,blue))))
     '(wl-highlight-message-cited-text-10 ((t (:foreground ,yellow))))
     '(wl-highlight-message-cited-text-2 ((t (:foreground ,blue))))
     '(wl-highlight-message-cited-text-3 ((t (:foreground ,blue))))
     '(wl-highlight-message-cited-text-4 ((t (:foreground ,green))))
     '(wl-highlight-message-cited-text-5 ((t (:foreground ,green))))
     '(wl-highlight-message-cited-text-6 ((t (:foreground ,green))))
     '(wl-highlight-message-cited-text-7 ((t (:foreground ,red))))
     '(wl-highlight-message-cited-text-8 ((t (:foreground ,red))))
     '(wl-highlight-message-cited-text-9 ((t (:foreground ,red))))
     '(wl-highlight-message-header-contents ((t (:inherit message-header-other))))
     '(wl-highlight-message-headers ((t (:inherit message-header-name))))
     '(wl-highlight-message-important-header-contents ((t (:foreground ,green))))
     '(wl-highlight-message-important-header-contents2 ((t (:foreground ,green))))
     '(wl-highlight-message-signature ((t (:foreground ,green))))
     '(wl-highlight-message-unimportant-header-contents ((t (:foreground ,solarized-fg))))
     '(wl-highlight-summary-answered-face ((t (:foreground ,solarized-fg :weight bold))))
     '(wl-highlight-summary-copied-face ((t (:foreground ,solarized-emph :slant italic))))
     '(wl-highlight-summary-deleted-face ((t (:foreground ,solarized-comment :slant italic))))
     '(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
     '(wl-highlight-summary-disposed-face ((t (:foreground ,solarized-fg :slant italic))))
     '(wl-highlight-summary-flagged-face ((t (:foreground ,solarized-emph))))
     '(wl-highlight-summary-forwarded-face ((t (:foreground ,solarized-fg :slant italic))))
     '(wl-highlight-summary-high-read-face ((t (:foreground ,solarized-fg :weight bold))))
     '(wl-highlight-summary-high-unread-face ((t (:foreground ,orange-hc :weight bold))))
     '(wl-highlight-summary-killed-face ((t (:inherit wl-highlight-summary-deleted-face))))
     '(wl-highlight-summary-low-read-face ((t (:foreground ,solarized-fg :slant italic))))
     '(wl-highlight-summary-low-unread-face ((t (:foreground ,orange-lc :slant italic))))
     '(wl-highlight-summary-new-face ((t (:foreground ,orange-hc :weight bold))))
     '(wl-highlight-summary-normal-face ((t (:foreground ,solarized-fg))))
     '(wl-highlight-summary-prefetch-face ((t (:inherit wl-highlight-summary-normal-face :slant italic))))
     '(wl-highlight-summary-refiled-face ((t (:foreground ,solarized-fg))))
     '(wl-highlight-summary-resend-face ((t (:foreground ,red-lc :weight bold))))
     '(wl-highlight-summary-spam-face ((t (:foreground ,solarized-comment :slant italic))))
     '(wl-highlight-summary-target-face ((t (:foreground ,solarized-emph :weight bold))))
     '(wl-highlight-summary-temp-face ((t (:foreground ,yellow :slant italic))))
     '(wl-highlight-summary-thread-top-face ((t (:inherit wl-highlight-summary-normal-face))))
     '(wl-highlight-summary-unread-face ((t (:foreground ,orange))))
     '(wl-highlight-thread-indent-face ((t (:foreground ,magenta))))
     '(wl-message-header-narrowing-face ((t (:foreground ,base3 :invisible t))))
     '(wl-summary-persistent-mark-face ((t (:inherit dired-mark))))

     ;; web mode
     '(web-mode-builtin-face ((t (:inherit font-lock-builtin-face))))
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
     '(web-mode-block-delimiter-face ((t (:inherit font-lock-preprocessor-face))))
     '(web-mode-css-comment-face ((t (:inherit web-mode-comment-face))))
     '(web-mode-css-variable-face ((t (:inherit web-mode-variable-name-face :slant italic))))
     '(web-mode-error-face ((t (:background ,red))))
     '(web-mode-function-call-face ((t (:inherit font-lock-function-name-face))))
     '(web-mode-html-attr-custom-face ((t (:inherit web-mode-html-attr-name-face))))
     '(web-mode-html-attr-engine-face ((t (:inherit web-mode-html-attr-custom-face))))
     '(web-mode-html-attr-equal-face ((t (:inherit web-mode-html-attr-name-face))))
     '(web-mode-html-tag-custom-face ((t (:inherit web-mode-html-tag-face))))
     '(web-mode-javascript-comment-face ((t (:inherit web-mode-comment-face))))
     '(web-mode-json-comment-face ((t (:inherit web-mode-comment-face))))

     ;; which-func-mode
     '(which-func ((t (:foreground ,green))))

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
       ((t (:foreground ,orange :background unspecified :inverse-video t :weight bold))))

     ;; widget
     '(widget-documentation ((t (:foreground ,green :background unspecified))))
     '(widget-button ((t (:inherit org-checkbox))))
     '(widget-button-pressed ((t (:background ,base03 :foreground ,base0
                                              :box (:line-width 1 :style pressed-button)))))
     '(widget-field ((t (:inherit secondary-selection))))
     '(widget-single-line-field ((t (:inherit widget-field))))
     '(widget-inactive ((t (:inherit modeline-inactive))))

     ;; woman
     '(woman-italic ((t (:inherit italic :foreground ,green))))
     '(woman-bold ((t (:inherit bold :foreground ,blue))))

     ;; ztree
     '(ztreep-arrow-face ((t (:foreground ,base01))))
     '(ztreep-diff-header-face ((t (:foreground ,base01 :weight bold :height 1.2))))
     '(ztreep-diff-header-small-face ((t (:foreground ,base01 :weight bold))))
     '(ztreep-diff-model-add-face ((t (:foreground ,blue))))
     '(ztreep-diff-model-diff-face ((t (:foreground ,red))))
     '(ztreep-diff-model-normal-face ((t (:foreground ,base0))))
     '(ztreep-expand-sign-face ((t (:foreground ,base01))))
     '(ztreep-header-face ((t (:foreground ,base01 :weight bold :height 1.2))))
     '(ztreep-leaf-face ((t (:foreground ,base0))))
     '(ztreep-node-face ((t (:foreground ,blue)))))

   (custom-theme-set-variables
    'solarized

    ;; ansi-colors
    `(ansi-color-names-vector
      [,base02 ,red ,green ,yellow ,blue ,magenta ,cyan ,base00])

    ;; compilation
    `(compilation-message-face 'default)

    ;; cua
    `(cua-normal-cursor-color ,base0)
    `(cua-read-only-cursor-color ,green)
    `(cua-global-mark-cursor-color ,cyan)
    `(cua-overwrite-cursor-color ,yellow)

    ;; fill column indicator
    `(fci-rule-color ,(solarized-find-color
                       (if (eq 'light solarized-background) 'base2 'base02)
                       solarized-dark-palette))

    ;; highlight-changes
    `(highlight-changes-colors '(,magenta ,violet))

    ;; pos-tip
    `(pos-tip-foreground-color ,base1)
    `(pos-tip-background-color ,base02)

    ;; smart mode line
    `(sml/active-foreground-color ,(face-attribute 'mode-line :foreground))
    `(sml/active-background-color ,(face-attribute 'mode-line :background))
    `(sml/inactive-foreground-color ,(face-attribute 'mode-line-inactive :foreground))
    `(sml/inactive-background-color ,(face-attribute 'mode-line-inactive :background))

    ;; xterm-color
    `(xterm-color-names [,base02 ,red ,green ,yellow
                                 ,blue ,magenta ,cyan ,base2])
    `(xterm-color-names-bright [,base03 ,orange ,base01 ,base00
                                        ,base0 ,violet ,base1 ,base3]))
   ))


;;;###autoload
(when (and load-file-name
           (boundp 'custom-theme-load-path))
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'solarized)

;;; solarized-theme.el ends here
