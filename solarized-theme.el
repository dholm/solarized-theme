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
     '(mode-line ((t (:foreground ,solarized-emph :background ,solarized-hl ,@fmt-revbb :box nil))))
     '(mode-line-inactive ((t (:foreground ,base00 :background ,solarized-hl ,@fmt-revbb :box nil))))
     '(region ((t (:foreground ,solarized-comment :background ,solarized-bg ,@fmt-revbb))))
     '(secondary-selection ((t (:background ,solarized-hl))))
     '(trailing-whitespace ((t (,@fmt-revr :foreground ,red))))
     '(vertical-border ((t (:foreground ,solarized-fg))))

     ;; compilation
     '(compilation-info ((t (,@fmt-bold :foreground ,green))))
     '(compilation-warning ((t (,@fmt-bold :foreground ,orange))))

     ;; isearch
     '(isearch ((t (,@fmt-stnd :foreground ,orange :background ,solarized-bg))))
     '(isearch-fail ((t (,@fmt-stnd :foreground ,orange :background ,solarized-bg))))

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

     ;; auctex
     '(font-latex-warning ((t (:foreground ,red))))
     '(font-latex-sectioning-5 ((t (:foreground ,violet))))

     ;; bookmarks
     '(bm-fringe-face ((t (:background ,orange :foreground ,solarized-bg))))
     '(bm-fringe-persistent-face ((t (:background ,blue :foreground ,solarized-bg))))

     ;; diff
     '(diff-added ((t (,@fmt-revr :foreground ,green))))
     '(diff-changed ((t (,@fmt-revr :foreground ,yellow))))
     '(diff-removed ((t (,@fmt-revr :foreground ,red))))
     '(diff-refine-change ((t (,@fmt-revr :foreground ,blue :background ,solarized-bg))))
     '(diff-file-header ((t (:background ,solarized-bg))))
     '(diff-header ((t (:foreground ,solarized-emph :background ,solarized-bg))))

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

     ;; flyspell
     '(flyspell-incorrect ((t (:inherit flymake-errline))))
     '(flyspell-duplicate ((t (:inherit flymake-warnline))))

     ;; erc
     '(erc-input-face ((t (:foreground ,solarized-comment))))
     '(erc-keyword-face ((t (,@fmt-bldi :foreground ,yellow))))
     '(erc-nick-default-face ((t (,@fmt-none :foreground ,cyan))))
     '(erc-my-nick-face ((t (:foreground ,blue))))
     '(erc-notice-face ((t (,@fmt-none :foreground ,blue))))
     '(erc-timestamp-face ((t (:foreground ,solarized-comment))))

     ;; git-gutter
     '(git-gutter:modified ((t (:foreground ,violet))))
     '(git-gutter:added ((t (:foreground ,green))))
     '(git-gutter:deleted ((t (:foreground ,red))))

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

     ;; hl-line
     '(hl-line ((t (:underline ,opt-under :background ,solarized-hl))))

     ;; ido
     '(ido-first-match ((t (,@fmt-bold :foreground ,green))))
     '(ido-only-match ((t (:foreground ,green))))
     '(ido-subdir ((t (:foreground ,blue))))

     ;; jabber
     '(jabber-roster-user-away ((t (,@fmt-ital :foreground ,green))))
     '(jabber-roster-user-online ((t (,@fmt-bold :foreground ,blue))))
     '(jabber-roster-user-dnd ((t (,@fmt-ital :foreground ,red))))
     '(jabber-chat-prompt-local ((t (,@fmt-bold :foreground ,blue))))
     '(jabber-chat-prompt-foreign ((t (,@fmt-bold :foreground ,red))))
     '(jabber-activity-face ((t (,@fmt-bold :foreground ,red))))
     '(jabber-activity-personal-face ((t (,@fmt-bold :foreground ,blue))))

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
     '(org-todo ((t (,@fmt-bold :foreground ,solarized-bg :background ,red))))

     ;; outline
     '(outline-1 ((t (,@fmt-none :foreground ,blue))))
     '(outline-2 ((t (,@fmt-none :foreground ,cyan))))
     '(outline-3 ((t (,@fmt-none :foreground ,yellow))))
     '(outline-4 ((t (,@fmt-none :foreground ,red))))
     '(outline-5 ((t (,@fmt-none :foreground ,solarized-fg))))
     '(outline-6 ((t (,@fmt-none :foreground ,solarized-comment))))
     '(outline-7 ((t (,@fmt-none :foreground ,orange))))
     '(outline-8 ((t (,@fmt-none :foreground ,violet))))

     ;; rainbow-delimiters
     '(rainbow-delimiters-depth-1-face ((t (:foreground ,cyan))))
     '(rainbow-delimiters-depth-2-face ((t (:foreground ,yellow))))
     '(rainbow-delimiters-depth-3-face ((t (:foreground ,blue))))
     '(rainbow-delimiters-depth-4-face ((t (:foreground ,red))))
     '(rainbow-delimiters-depth-5-face ((t (:foreground ,green))))
     '(rainbow-delimiters-depth-6-face ((t (:foreground ,blue))))
     '(rainbow-delimiters-depth-7-face ((t (:foreground ,orange))))
     '(rainbow-delimiters-depth-8-face ((t (:foreground ,magenta))))
     '(rainbow-delimiters-depth-9-face ((t (:foreground ,solarized-fg))))

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
