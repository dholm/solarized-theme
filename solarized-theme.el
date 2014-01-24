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

(defconst solarized-contrast-components
  ;; name       sRGB      256       16              8
  '((yellow  (("#7B6000" "#875f00" "yellow"        "yellow")
              ("#DEB542" "#d7a55f" "brightyellow"  "white")))
    (orange  (("#8B2C02" "#d75f00" "red"           "red")
              ("#F2804F" "#ff875f" "brightyellow"  "white")))
    (red     (("#990A1B" "#870000" "red"           "red")
              ("#FF6E64" "#ff5f5f" "brightred"     "white")))
    (magenta (("#93115C" "#87005f" "magenta"       "magenta")
              ("#F771AC" "#ff87af" "brightmagenta" "white")))
    (violet  (("#3F4D91" "#5f5fd7" "blue"          "blue")
              ("#9EA0E5" "#87afff" "brightmagenta" "white")))
    (blue    (("#00629D" "#005faf" "blue"          "blue")
              ("#69B7F0" "#5fafff" "brightblue"    "white")))
    (cyan    (("#00736F" "#00875f" "cyan"          "cyan")
              ("#69CABF" "#5fd7d7" "brightcyan"    "white")))
    (green   (("#546E00" "#5f8700" "green"         "green")
              ("#B4C342" "#d7ff5f" "brightgreen"   "white")))))

(defun solarized-column-index ()
  "Return the palette column to use based on available features."
  (if window-system
      1
    (case (display-color-cells)
      (16 3)
      (8 4)
      (otherwise 2))))

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
     '(header-line ((t (:foreground ,solarized-fg :background ,solarized-hl ,@fmt-revbb))))
     '(highlight ((t (:background ,solarized-hl))))
     '(lazy-highlight ((t (,@fmt-revr :foreground ,yellow :background ,solarized-bg))))
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

     ;; compilation
     '(compilation-info ((t (,@fmt-bold :foreground ,green))))
     '(compilation-warning ((t (,@fmt-bold :foreground ,orange))))

     ;; ctable
     '(ctbl:face-cell-select ((t (:foreground ,solarized-emph :background ,solarized-hl
                                              :underline ,solarized-emph :weight bold))))
     '(ctbl:face-continue-bar ((t (:foreground ,yellow :background ,solarized-hl))))
     '(ctbl:face-row-select ((t (:foreground ,solarized-fg :background ,solarized-hl
                                             :underline t))))

     ;; diff
     '(diff-added ((t (,@fmt-revr :foreground ,green))))
     '(diff-changed ((t (,@fmt-revr :foreground ,yellow))))
     '(diff-removed ((t (,@fmt-revr :foreground ,red))))
     '(diff-refine-change ((t (,@fmt-revr :foreground ,blue :background ,solarized-bg))))
     '(diff-file-header ((t (:background ,solarized-bg))))
     '(diff-header ((t (:foreground ,solarized-emph :background ,solarized-bg))))

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
     '(git-gutter:added ((t (:foreground ,solarized-bg :background ,green
                                         :weight bold))))
     '(git-gutter:deleted ((t (:foreground ,solarized-bg :background ,red
                                           :weight bold))))
     '(git-gutter:modified ((t (:foreground ,solarized-bg :background ,red
                                            :weight bold))))
     '(git-gutter:unchanged ((t (:foreground ,solarized-bg :background ,solarized-hl
                                             :weight bold))))

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
     '(helm-buffer-saved-out ((t (:foreground ,red :background ,solarized-bg
                                              :inverse-video t))))
     '(helm-buffer-size ((t (:foreground ,solarized-comment))))
     '(helm-candidate-number ((t (:foreground ,solarized-emph :background ,solarized-hl
                                              :bold t))))
     '(helm-ff-directory ((t (:foreground ,blue :background ,solarized-bg))))
     '(helm-ff-executable ((t (:foreground ,green))))
     '(helm-ff-file ((t (:foreground ,solarized-fg :background ,solarized-bg))))
     '(helm-ff-invalid-symlink ((t (:foreground ,orange :background ,solarized-bg
                                                :slant italic))))
     '(helm-ff-prefix ((t (:foreground ,solarized-bg :background ,yellow))))
     '(helm-ff-symlink ((t (:foreground ,cyan))))
     '(helm-grep-file ((t (:foreground ,cyan :underline t))))
     '(helm-grep-finish ((t (:foreground ,green))))
     '(helm-grep-lineno ((t (:foreground ,orange))))
     '(helm-grep-match ((t (:inherit match))))
     '(helm-grep-running ((t (:foreground ,red))))
     '(helm-header ((t (:inherit header-line))))
     '(helm-lisp-completion-info ((t (:foreground ,solarized-fg))))
     '(helm-lisp-show-completion ((t (:foreground ,yellow :background ,solarized-hl
                                                  :bold t))))
     '(helm-M-x-key ((t (:foreground ,orange :underline t))))
     '(helm-moccur-buffer ((t (:foreground ,cyan :underline t))))
     '(helm-match ((t (:inherit match))))
     '(helm-selection ((t (:background ,solarized-hl ,@fmt-undr))))
     '(helm-selection-line ((t (:foreground ,solarized-emph :background ,solarized-hl
                                            :underline nil))))
     '(helm-separator ((t (:foreground ,red))))
     '(helm-source-header ((t (:foreground ,solarized-bg :background ,blue-lc
                                           :underline nil))))
     '(helm-time-zone-current ((t (:foreground ,green))))
     '(helm-time-zone-home ((t (:foreground ,red))))
     '(helm-visible-mark ((t (:foreground ,magenta :background ,solarized-bg
                                          :bold t))))

     ;; hl-line
     '(hl-line ((t (:underline ,opt-under :background ,solarized-hl))))

     ;; ido
     '(ido-first-match ((t (,@fmt-bold :foreground ,green))))
     '(ido-only-match ((t (:foreground ,green))))
     '(ido-subdir ((t (:foreground ,blue))))

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
     '(org-done ((t (,@fmt-bold :foreground ,green))))
     '(org-hide ((t (:foreground ,solarized-bg))))
     '(org-level-1 ((t (:inherit ,s-variable-pitch :foreground ,orange :height ,solarized-height-plus-4))))
     '(org-level-2 ((t (:inherit ,s-variable-pitch :foreground ,green :height ,solarized-height-plus-3))))
     '(org-level-3 ((t (:inherit ,s-variable-pitch :foreground ,blue :height ,solarized-height-plus-2))))
     '(org-level-4 ((t (:inherit ,s-variable-pitch :foreground ,yellow :height ,solarized-height-plus-1))))
     '(org-level-5 ((t (:inherit ,s-variable-pitch :foreground ,cyan))))
     '(org-level-6 ((t (:inherit ,s-variable-pitch :foreground ,green))))
     '(org-level-7 ((t (:inherit ,s-variable-pitch :foreground ,red))))
     '(org-level-8 ((t (:inherit ,s-variable-pitch :foreground ,blue))))
     '(org-todo ((t (,@fmt-bold :foreground ,solarized-bg :background ,red))))

     ;; outline
     '(outline-1 ((t (:inherit org-level-1))))
     '(outline-2 ((t (:inherit org-level-2))))
     '(outline-3 ((t (:inherit org-level-3))))
     '(outline-4 ((t (:inherit org-level-4))))
     '(outline-5 ((t (:inherit org-level-5))))
     '(outline-6 ((t (:inherit org-level-6))))
     '(outline-7 ((t (:inherit org-level-7))))
     '(outline-8 ((t (:inherit org-level-8))))

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

     ;; show-paren
     '(show-paren-mismatch ((t (,@fmt-bold :foreground ,red :background ,solarized-comment))))
     '(show-paren-match ((t (,@fmt-bold :foreground ,cyan :background ,solarized-hl))))

     ;; slime
     '(slime-repl-inputted-output-face ((t (:foreground ,red))))

     ;; term
     '(term-color-black ((t ( :foreground ,solarized-hl))))
     '(term-color-red ((t ( :foreground ,red))))
     '(term-color-green ((t ( :foreground ,green))))
     '(term-color-yellow ((t ( :foreground ,yellow))))
     '(term-color-blue ((t ( :foreground ,blue))))
     '(term-color-magenta ((t ( :foreground ,magenta))))
     '(term-color-cyan ((t ( :foreground ,cyan))))
     '(term-color-white ((t ( :foreground ,base00))))

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

     ;; whitespace
     '(whitespace-space ((t (:foreground ,solarized-hl))))
     '(whitespace-hspace ((t (:foreground ,orange))))
     '(whitespace-tab ((t (:foreground ,solarized-hl))))
     '(whitespace-trailing ((t (,@fmt-bold :foreground ,red :background ,solarized-hl))))
     '(whitespace-line ((t (:foreground ,magenta :background ,solarized-bg))))
     '(whitespace-space-before-tab ((t (,@fmt-bold :foreground ,red))))
     '(whitespace-indentation ((t (:foreground ,solarized-hl))))
     '(whitespace-empty ((t (:foreground ,red))))
     '(whitespace-space-after-tab ((t (:foreground ,cyan)))))))

;;;###autoload
(when (and load-file-name
           (boundp 'custom-theme-load-path))
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'solarized)

;;; solarized-theme.el ends here
