;;; noctilux-theme.el --- Dark theme inspired by LightTable
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'org))

(defconst noctilux-description
  "A Light Table inspired color theme based on Solarized's definitions.")

(defcustom noctilux-degrade nil
  "For test purposes only; when in GUI mode, forces Noctilux to use the 256
degraded color mode to test the approximate color values for accuracy."
  :type 'boolean
  :group 'lt)

(defcustom noctilux-diff-mode 'normal
  "Sets the level of highlighting to use in diff-like modes."
  :type 'symbol
  :options '(high normal low)
  :group 'lt)

(defcustom noctilux-bold t
  "Stops Noctilux from displaying bold when nil."
  :type 'boolean
  :group 'lt)

(defcustom noctilux-underline t
  "Stops Noctilux from displaying underlines when nil."
  :type 'boolean
  :group 'lt)

(defcustom noctilux-italic t
  "Stops Noctilux from displaying italics when nil."
  :type 'boolean
  :group 'lt)

(defcustom noctilux-contrast 'normal
  "Stick with normal! It's been carefully tested. Setting this option to high or
low does use the same Noctilux palette but simply shifts some values up or
down in order to expand or compress the tonal range displayed."
  :type 'symbol
  :options '(high normal low)
  :group 'lt)

(defcustom noctilux-broken-srgb (and (eq system-type 'darwin)
                                     (eq window-system 'ns)
                                     (not (when (boundp 'ns-use-srgb-colorspace)
                                            ns-use-srgb-colorspace)))
  "Emacs bug #8402 results in incorrect color handling on Macs. If this is t
\(the default on Macs), Noctilux works around it with alternative colors.
However, these colors are not totally portable, so you may be able to edit
the \"Gen RGB\" column in noctilux-definitions.el to improve them further."
  :type 'boolean
  :group 'lt)

(defvar noctilux-colors           ; ANSI(Noctilux terminal)
  ;; name       sRGB      Gen RGB   256       16              8
  '((base03    "#282a36" "#282a36" "#282a36" "brightblack"   "black")
    (base02    "#292929" "#292929" "#292929" "black"         "black")
    (base01    "#5f5f5f" "#5f5f5f" "#5f5f5f" "brightgreen"   "green")
    (base00    "#999999" "#999999" "#999999" "brightyellow"  "yellow")
    (base0     "#cccccc" "#cccccc" "#cccccc" "brightblue"    "blue")
    (base1     "#aaaaaa" "#aaaaaa" "#aaaaaa" "brightcyan"    "cyan")
    (base2     "#e9e2cb" "#e9e2cb" "#e9e2cb" "white"         "white")
    (base3     "#fcf4dc" "#fcf4dc" "#fcf4dc" "brightwhite"   "white")
    (yellow    "#f1fa8c" "#f1fa8c" "#f1fa8c" "yellow"        "yellow")
    (orange    "#ff7518" "#ff7518" "#ff7518" "brightred"     "red")
    (red       "#ff5555" "#ff5555" "#ff5555" "red"           "red")
    (magenta   "#FF1F69" "#FF1F69" "#FF1F69" "magenta"       "magenta")
    (purple    "#bd93f9" "#bd93f9" "#bd93f9" "brightmagenta" "magenta")
    (darkblue  "#6272a4" "#6272a4" "#6272a4" "blue"          "blue")
    (blue      "#0198E1" "#0198E1" "#0198E1" "blue"          "blue")
    (pink      "#ff79c6" "#ff79c6" "#ff79c6" "pink"          "pink")
    (cyan      "#7590db" "#7590db" "#7590db" "cyan"          "cyan")
    (oldcyan   "#aadddd" "#aadddd" "#aadddd" "cyan"          "cyan")
    (white     "#ffffff" "#ffffff" "#ffffff" "white"         "white")
    (green     "#50fa7b" "#50fa7b" "#50fa7b" "green"         "green")
    (gray      "#838484" "#838484" "#838484" "gray"          "gray")
    (darkgreen "#008000" "#008000" "#008000" "green"         "green")))


(setq org-todo-keyword-faces '(
    ("PROJECT" . (:inherit default :height 1.0 :foreground "#ffffff" :weight bold :underline nil))
    ("STRT"    . (:inherit default :height 1.0 :foreground "#0198e1" :weight bold :underline nil))
    ("SOMEDAY" . (:inherit default :height 1.0 :foreground "#ff8888" :weight bold :underline nil))
    ("REVIEW"  . (:inherit default :height 1.0 :foreground "#f1fa8c" :weight bold :underline nil))
    ("WAIT"    . (:inherit default :height 1.0 :foreground "#f1fa8c" :weight bold :underline nil))))

(setq org-modern-todo-faces '(
    ("PROJECT" . (:inherit default :height 1.0 :foreground "#ffffff" :weight bold :underline nil))
    ("STRT"    . (:inherit default :height 1.0 :foreground "#0198e1" :weight bold :underline nil))
    ("SOMEDAY" . (:inherit default :height 1.0 :foreground "#ff8888" :weight bold :underline nil))
    ("REVIEW"  . (:inherit default :height 1.0 :foreground "#f1fa8c" :weight bold :underline nil))
    ("WAIT"    . (:inherit default :height 1.0 :foreground "#f1fa8c" :weight bold :underline nil))))

(defmacro noct-flet (specs &rest body)
  (let ((flet (if (fboundp 'cl-flet) 'cl-flet 'flet)))
    `(,flet ,specs ,@body)))

(defun noctilux-color-definitions (mode)
  (noct-flet ((find-color (name)
           (let* ((index (if window-system
                             (if noctilux-degrade
                                 3
                               (if noctilux-broken-srgb 2 1))
                           (cl-case (display-color-cells)
                             (16 4)
                             (8  5)
                             (otherwise 3)))))
             (nth index (assoc name noctilux-colors)))))

    (let ((base03      (find-color 'base03))
          (base02      (find-color 'base02))
          (base01      (find-color 'base01))
          (base00      (find-color 'base00))
          (base0       (find-color 'base0))
          (base1       (find-color 'base1))
          (base2       (find-color 'base2))
          (base3       (find-color 'base3))
          (yellow      (find-color 'yellow))
          (orange      (find-color 'orange))
          (red         (find-color 'red))
          (magenta     (find-color 'magenta))
          (purple      (find-color 'purple))
          (darkblue    (find-color 'darkblue))
          (blue        (find-color 'blue))
          (pink        (find-color 'pink))
          (oldcyan     (find-color 'oldcyan))
          (cyan        (find-color 'cyan))
          (white       (find-color 'white))
          (green       (find-color 'green))
	  (gray        (find-color 'gray))
	  (darkgreen   (find-color 'darkgreen))
          (bold        (if noctilux-bold 'bold 'normal))
          (bright-bold (if noctilux-bold 'normal 'bold))
          (underline   (if noctilux-underline t nil))
          (opt-under   nil)
          (italic      (if noctilux-italic 'italic 'normal)))
      (let ((back base03))
        (cond ((< (display-color-cells) 16)
               (setf back nil))
              ((eq 'high noctilux-contrast)
               (let ((orig-base3 base3))
                 (cl-rotatef base01 base00 base0 base1 base2 base3)
                 (setf base3 orig-base3)))
              ((eq 'low noctilux-contrast)
               (setf back      base02
                     opt-under t)))
        ;; NOTE: We try to turn an 8-color term into a 10-color term by not
        ;;       using default background and foreground colors, expecting the
        ;;       user to have the right colors set for them.
        (let ((bg-back   `(:background ,back))
              (bg-base03 `(:background ,base03))
              (bg-base02 `(:background ,base02))
              (bg-base01 `(:background ,base01))
              (bg-base00 `(:background ,base00))
              (bg-base0 `(:background ,base0))
              (bg-base1 `(:background ,base1))
              (bg-base2 `(:background ,base2))
              (bg-base3 `(:background ,base3))
              (bg-green `(:background ,green))
              (bg-yellow `(:background ,yellow))
              (bg-orange `(:background ,orange))
              (bg-red `(:background ,red))
              (bg-magenta `(:background ,magenta))
              (bg-purple `(:background ,purple))
              (bg-blue `(:background ,blue))
              (bg-cyan `(:background ,cyan))
              (bg-white `(:background ,white))

              (fg-base03 `(:foreground ,base03))
              (fg-base02 `(:foreground ,base02))
              (fg-base01 `(:foreground ,base01))
              (fg-base00 `(:foreground ,base00))
              (fg-base0  `(:foreground ,(when (<= 16 (display-color-cells))
                                        base0)))
              (fg-base1  `(:foreground ,(when (<= 16 (display-color-cells))
                                        base1)))
              (fg-base2 `(:foreground ,base2))
              (fg-base3 `(:foreground ,base3))
              (fg-green `(:foreground ,green))
              (fg-yellow `(:foreground ,yellow))
              (fg-orange `(:foreground ,orange))
              (fg-red `(:foreground ,red))
              (fg-magenta `(:foreground ,magenta))
              (fg-purple `(:foreground ,purple))
              (fg-darkblue `(:foreground ,darkblue))
              (fg-blue `(:foreground ,blue))
              (fg-pink `(:foreground ,pink))
              (fg-cyan `(:foreground ,cyan))
              (fg-oldcyan `(:foreground ,oldcyan))
              (fg-gray `(:foreground ,gray))
              (fg-darkgreen `(:foreground ,darkgreen))
              (fg-white `(:foreground ,white))

              (fmt-none `(:weight normal :slant normal  :underline nil        :inverse-video nil))
              (fmt-big `(:weight normal :slant normal  :underline nil        :inverse-video nil :height 1.2))
              (fmt-sbig `(:weight normal :slant normal  :underline nil        :inverse-video nil :height 1.1))
              (fmt-bigger  `(:weight normal :slant normal  :underline nil        :inverse-video nil :height 1.5))
              (fmt-small `(:weight normal :slant normal :underline nil        :inverse-video nil :height 0.8))
              (fmt-bold `(:weight ,bold  :slant normal  :underline nil        :inverse-video nil))
              (fmt-bldi `(:weight ,bold                 :underline nil        :inverse-video nil))
              (fmt-undr `(:weight normal :slant normal  :underline ,underline :inverse-video nil))
              (fmt-strk `(:weight normal :slant normal  :underline nil        :inverse-video nil :strike-through t))
              (fmt-undb `(:weight ,bold  :slant normal  :underline ,underline :inverse-video nil))
              (fmt-undi `(:weight normal                :underline ,underline :inverse-video nil))
              (fmt-uopt `(:weight normal :slant normal  :underline ,opt-under :inverse-video nil))
              (fmt-bb  `(:weight ,bright-bold :slant normal :underline nil    :inverse-video nil))
              ;; FIXME: not quite the same
              (fmt-curl `(:weight normal :slant normal  :underline t          :inverse-video nil))
              (fmt-ital `(:weight normal :slant ,italic :underline nil        :inverse-video nil))
              ;; FIXME: not quite the same
              (fmt-stnd `(:weight normal :slant normal  :underline nil        :inverse-video t))
              (fmt-revr `(:weight normal :slant normal  :underline nil        :inverse-video t))
              (fmt-revb `(:weight ,bold  :slant normal  :underline nil        :inverse-video t))
              (fmt-revbb `(:weight ,bright-bold :slant normal :underline nil  :inverse-video t))
              (fmt-revbbu `(:weight ,bright-bold :slant normal  :underline ,underline :inverse-video t))
              (fmt-redwave `(:underline (:color ,red :style wave) :inherit default))
              (fmt-orangewave `(:underline (:color ,orange :style wave) :inherit default)))
          `((;; basic
             (default ((t (,@fg-base0 ,@bg-back)))) ; Normal
	     (italic ((t (:underline nil :italic t))))
             (cursor ((t (,@fg-base03 ,@bg-orange)))) ; Cursor
             (error ((t (,@fmt-bold ,@fg-red)))) ; Error
             (escape-glyph-face ((t (,@fg-red))))
             ;; (fringe ((t (,@fg-base01 ,@bg-base02))))
             (fringe ((t (,@fg-base01 ,@bg-base03))))
             (linum ((t (,@fg-base01 ,@bg-base03))))
             (header-line ((t (,@fg-base0 ,@bg-base03)))) ; Pmenu
             (highlight ((t (:background "#222222" :underline ,opt-under))))
             (hl-line ((t (:underline ,opt-under ,@bg-base02)))) ; CursorLine
             (treemacs-hl-line-face ((t (:underline ,opt-under ,@bg-base02)))) ; CursorLine
             (isearch ((t (,@fmt-stnd ,@fg-orange ,@bg-back)))) ; IncSearch
             (isearch-fail ((t (,@fmt-stnd ,@fg-orange ,@bg-back)))) ; IncSearch
             (lazy-highlight ((t (,@fmt-revr ,@fg-yellow ,@bg-back)))) ; Search
             (link ((t (,@fmt-undr ,@fg-purple))))
             (link-visited ((t (,@fmt-undr ,@fg-magenta))))
             (menu ((t (,@fg-base0 ,@bg-base02))))
             (minibuffer-prompt ((t (,@fmt-bold ,@fg-cyan)))) ; Question
             (mode-line  ; StatusLine
              ((t (,@fg-base0,@bg-base02 ,@fmt-bb :box nil))))
             (mode-line-inactive ; StatusLineNC
              ((t (,@fg-base01 ,@bg-base03 ,@fmt-bb :box nil))))
             (region ((t (,@fg-base02 ,@bg-base00 ,@fmt-revbb)))) ; Visual
             (secondary-selection ((t (,@bg-base02))))
             (shadow ((t (,@fg-base01))))
             (trailing-whitespace ((t (,@fmt-revr ,@fg-red))))
             (vertical-border ((t (,@fg-base0))))
             ;; comint
             (comint-highlight-prompt ((t (,@fg-blue))))
             ;; compilation
             (compilation-info ((t (,@fmt-bold ,@fg-green))))
             (compilation-warning ((t (,@fmt-bold ,@fg-orange))))
             ;; custom
             (custom-button
              ((t (,@fg-base1 ,@bg-base02
                              :box (:line-width 2 :style released-button)))))
             (custom-button-mouse
              ((t (,@fmt-revr ,@fg-base1 ,@bg-base02 :inherit custom-button))))
             (custom-button-pressed
              ((t (,@fmt-revr ,@fg-base1 ,@bg-base02
                              :box (:line-width 2 :style pressed-button)
                              :inherit custom-button-mouse))))
             (custom-changed ((t (,@fmt-revr ,@fg-blue ,@bg-base3))))
             (custom-comment ((t (,@fg-base1 ,@bg-base02))))
             (custom-comment-tag ((t (,@fg-base1 ,@bg-base02))))
             (custom-documentation ((t (:inherit default))))
             (custom-group-tag ((t (,@fg-base1))))
             (custom-group-tag-1 ((t (,fmt-bold ,@fg-base1))))
             (custom-invalid ((t (,@fmt-revr ,@fg-red ,@bg-back))))
             (custom-link ((t (,@fg-purple))))
             (custom-state ((t (,@fg-green))))
             (custom-variable-tag ((t (,@fg-base1))))
             ;; diff - DiffAdd, DiffChange, DiffDelete, and DiffText
             ,@(cl-case noctilux-diff-mode
                 (high
                  `((diff-added ((t (,@fmt-revr ,@fg-green))))
                    (diff-changed ((t (,@fmt-revr ,@fg-yellow))))
                    (diff-removed ((t (,@fmt-revr ,@fg-red))))
                    (diff-refine-change
                     ((t (,@fmt-revr ,@fg-blue ,@bg-back))))))
                 (low
                  `((diff-added ((t (,@fmt-undr ,@fg-green))))
                    (diff-changed ((t (,@fmt-undr ,@fg-yellow))))
                    (diff-removed ((t (,@fmt-bold ,@fg-red))))
                    (diff-refine-change
                     ((t (,@fmt-undr ,@fg-blue ,@bg-back))))))
                 (normal
                  (if window-system
                      `((diff-added ((t (,@fmt-bold ,@fg-green))))
                        (diff-changed ((t (,@fmt-bold ,@fg-yellow))))
                        (diff-removed ((t (,@fmt-bold ,@fg-red))))
                        (diff-refine-change
                         ((t (,@fmt-bold ,@fg-blue ,@bg-back)))))
                    `((diff-added ((t (,@fg-green))))
                      (diff-changed ((t (,@fg-yellow))))
                      (diff-removed ((t (,@fg-red))))
                      (diff-refine-change ((t (,@fg-blue ,@bg-back))))))))
             (diff-file-header ((t (,@bg-back))))
             (diff-header ((t (,@fg-base1 ,@bg-back))))
             ;; IDO
             (ido-only-match ((t (,@fg-green))))
             (ido-subdir ((t (,@fg-blue))))
             (ido-first-match ((t (,@fmt-bold ,@fg-green))))
             ;; emacs-wiki
             (emacs-wiki-bad-link-face ((t (,@fmt-undr ,@fg-red))))
             (emacs-wiki-link-face ((t (,@fmt-undr ,@fg-blue))))
             (emacs-wiki-verbatim-face ((t (,@fmt-undr ,@fg-base00))))
             ;; eshell
             (eshell-ls-archive ((t (,@fg-magenta))))
             (eshell-ls-backup ((t (,@fg-yellow))))
             (eshell-ls-clutter ((t (,@fg-orange))))
             (eshell-ls-directory ((t (,@fg-blue)))) ; Directory
             (eshell-ls-executable ((t (,@fg-green))))
             (eshell-ls-missing ((t (,@fg-red))))
             (eshell-ls-product ((t (,@fg-yellow))))
             (eshell-ls-readonly ((t (,@fg-base1))))
             (eshell-ls-special ((t (,@fg-purple))))
             (eshell-ls-symlink ((t (,@fg-cyan))))
             (eshell-ls-unreadable ((t (,@fg-base00))))
             (eshell-prompt ((t (,@fmt-bold ,@fg-green))))
             ;; font-lock
             (font-lock-builtin-face ((t (,@fmt-none ,@fg-green)))) ; Statement
             (font-lock-comment-face ((t (,@fmt-ital ,@fg-base01)))) ; Comment
             (font-lock-constant-face ((t (,@fmt-none ,@fg-purple)))) ; Constant
             (font-lock-function-name-face ; Identifier
              ((t (,@fmt-none ,@fg-blue))))
             (font-lock-keyword-face ((t (,@fmt-none ,@fg-green)))) ; Statement
             (font-lock-string-face ((t (,@fmt-none ,@fg-cyan)))) ; Constant
             (font-lock-type-face ((t (,@fmt-none ,@fg-yellow)))) ; Type
             (font-lock-variable-name-face ; Identifier
              ((t (,@fmt-none ,@fg-blue))))
             (font-lock-warning-face ((t (,@fmt-bold ,@fg-red)))) ; Error
             (font-lock-doc-face ((t (,@fmt-ital ,@fg-base01)))) ; Comment
             (font-lock-doc-string-face  ; Comment (XEmacs-only)
              ((t (,@fmt-ital ,@fg-base01))))
             (font-lock-color-constant-face ((t (,@fmt-none ,@fg-green))))
             (font-lock-comment-delimiter-face ; Comment
              ((t (,@fmt-ital ,@fg-base01))))
             (font-lock-preprocessor-face ; PreProc
              ((t (,@fmt-none ,@fg-orange))))
             (font-lock-reference-face ((t (,@fmt-none ,@fg-cyan))))
             (font-lock-negation-char-face ((t (,@fmt-none ,@fg-red))))
             (font-lock-other-type-face ((t (,@fmt-ital ,@fg-blue))))
             (font-lock-regexp-grouping-construct
              ((t (,@fmt-none ,@fg-orange))))
             (font-lock-special-keyword-face ; Special
              ((t (,@fmt-none ,@fg-red))))
             (font-lock-exit-face ((t (,@fmt-none ,@fg-red))))
             (font-lock-other-emphasized-face ((t (,@fmt-bldi ,@fg-purple))))
             (font-lock-regexp-grouping-backslash
              ((t (,@fmt-none ,@fg-yellow))))
             ;; helm
             (helm-M-x-key ((t (,@fg-purple))))
             (helm-buffer-directory ((t (,@fmt-bold ,@fg-yellow))))
             (helm-buffer-file ((t (,@fmt-none ,@fg-base0))))
             (helm-buffer-not-saved ((t (,@fmt-undr ,@fg-red))))
             (helm-buffer-process ((t (,@fg-blue))))
             (helm-buffer-saved-out ((t (,@fg-cyan))))
             (helm-buffer-size ((t (,@fg-magenta))))
             (helm-candidate-number ((t (,@fmt-bold ,@fg-red ,@bg-base02))))
             (helm-header-line-left-margin ((t ,@fg-red)))
             (helm-ff-directory ((t (,@fmt-bold ,@fg-yellow))))
             (helm-ff-dotted-directory ((t (,@fg-red))))
             (helm-ff-executable ((t (,@fmt-bold ,@fg-base0))))
             (helm-ff-file ((t (,@fmt-none ,@fg-base0))))
             (helm-ff-invalid-symlink ((t (,@fmt-undr ,@fg-red ,@bg-base02))))
             (helm-ff-prefix ((t (,@fmt-bold ,@fg-yellow))))
             (helm-ff-symlink ((t (,@fmt-bold ,@fg-blue ,@bg-base02))))
             (helm-match ((t (,@fmt-undr ,@fg-red))))
             (helm-selection ((t (,@fg-green ,@bg-base01))))
             (helm-source-header ((t (,@fmt-bold ,@fg-cyan))))
             (helm-visible-mark ((t (,@fg-blue ,@bg-base02))))

             ;; info
             (info-xref ((t (,@fmt-undr ,@fg-blue))))
             (info-xref-visited ((t (,@fg-magenta :inherit info-xref))))

	     ;; eros
             (eros-result-overlay-face ((t (,@bg-base03))))

	     ;; selectrum
	     ;; ---------

	     ;; (selectrum-current-candidate ((t (:foreground ,base03 :underline ,gray :overline ,gray))))
	     (selectrum-current-candidate ((t (:foreground ,white :background ,base02))))

	     ;; (selectrum-prescient-primary-highlight ((t (,@fmt-none ,@bg-blue))))     
	     ;; (selectrum-prescient-secondary-highlight ((t (,@fmt-none ,@bg-green))))

	     ;; selectrum-mouse-highlight                 
	     ;; selectrum-completion-docsig               
	     ;; selectrum-completion-annotation           
	     ;; selectrum-group-separator                 
	     ;; selectrum-group-title                     
	     ;; selectrum-quick-keys-match                
	     ;; selectrum-quick-keys-highlight            

	     ;; consult
	     ;; -------
             (consult-file ((t (,@fg-green))))
	     (consult-preview-match ((t (,@fmt-none))))     

             ;; consult-preview-match
             ;; consult-preview-cursor

             ;; consult-buffer
             ;; consult-key
             ;; consult-help
             ;; consult-bookmark
             ;; consult-separator
             ;; consult-line-number
             ;; consult-async-split
             ;; consult-preview-line
             ;; consult-async-failed
             ;; consult-imenu-prefix
             ;; consult-preview-error
             ;; consult-async-running
             ;; consult-async-finished
             ;; consult-narrow-indicator
             ;; consult-preview-insertion
             ;; consult-line-number-prefix

	     ;; bookmark
             (bookmark-face     ((t (,@bg-base03 ,@fg-green))))

             ;; org
             (org-hide                  ((t (,@fg-base03))))
             (org-todo                  ((t (:inherit default ,@fmt-bold ,@fg-red  ,@bg-base03))))
             (org-modern-date-active    ((t (:inherit default ,@fmt-small ,@fg-gray ,@bg-base03))))
             (org-modern-date-inactive  ((t (:inherit default ,@fmt-small ,@fg-gray ,@bg-base03))))
             (org-modern-time-active    ((t (:inherit default ,@fmt-small ,@fg-gray ,@bg-base03))))
             (org-modern-time-inactive  ((t (:inherit default ,@fmt-small ,@fg-gray ,@bg-base03))))
             (org-modern-done           ((t (:inherit default ,@fmt-bold ,@fg-gray ,@bg-base03))))
             (org-modern-todo           ((t (:inherit default ,@fmt-bold ,@fg-red  ,@bg-base03))))
             (org-modern-tag            ((t (:inherit default ,@fmt-ital ,@fg-gray,@bg-base02))))
             (org-done                  ((t (:inherit default ,@fmt-bold ,@fg-gray ,@bg-base03))))
	     (org-headline-done         ((t (,@fmt-strk ,@fg-gray ,@bg-base03))))
             (org-date                  ((t (:inherit default ,@fmt-none ,@fg-yellow ,@bg-base03))))

	     (org-checkbox-statistics-todo  ((t (,@fmt-none ,@fg-red ,@bg-base03))))
	     (org-checkbox-statistics-done  ((t (,@fmt-none ,@fg-gray ,@bg-base03))))

	     (org-ellipsis      ((t (,@fmt-none ,@fg-gray))))

	     (org-document-title        ((t (,@fmt-bigger,@fg-gray ,@bg-base03))))
	     (org-document-info-keyword ((t (:inherit default ,@fmt-small ,@fg-gray  ,@bg-base03))))
             (org-code                  ((t (:inherit default ,@fmt-none  ,@fg-green ,@bg-base03))))
             (org-verbatim              ((t (:inherit default ,@fmt-none  ,@fg-yellow ,@bg-base03))))
	     (org-quote                 ((t (:inherit default ,@fmt-ital  ,@fg-gray  ,@bg-base03))))
	     (org-block                 ((t (:inherit default ,@fmt-none  ,@fg-white ,@bg-base03))))
	     (org-block-begin-line      ((t (:inherit default ,@fmt-small ,@fg-gray  ,@bg-base03))))
	     (org-block-end-line        ((t (:inherit default ,@fmt-small ,@fg-gray  ,@bg-base03))))
	     (org-drawer                ((t (:inherit default ,@fmt-small ,@fg-gray  ,@bg-base03))))
	     (org-meta-line             ((t (:inherit default ,@fmt-small ,@fg-gray  ,@bg-base03))))
	     (org-special-keyword       ((t (:inherit default ,@fg-green))))

	     ;; (org-table                 ((t (:inherit default ,@fmt-none  ,@fg-blue ,@bg-base03))))
	     ;; (org-table-header          ((t (:inherit default ,@fmt-none  ,@fg-blue ,@bg-base03))))

             (org-link ((t (,@fmt-undr ,@fg-darkblue ,@bg-base03))))
             (org-tag ((t (:inherit default ,@fmt-ital ,@fg-green ,@bg-base03))))

	     (org-level-1 ((t (,@fmt-big ,@fg-purple ,@bg-base03))))
	     (org-level-2 ((t (,@fmt-sbig ,@fg-pink ,@bg-base03))))
	     (org-level-3 ((t (,@fg-green ,@bg-base03))))

             (org-project-kwd-face ((t (,@fmt-big ,@fg-purple ,@bg-base03))))
             (org-waiting-kwd-face ((t (,@fmt-big ,@fg-orange ,@bg-base03))))
             (org-someday-kwd-face ((t (,@fmt-big ,@fg-blue ,@bg-base03))))
             (org-started-kwd-face ((t (,@fmt-big ,@fg-yellow ,@bg-base03))))

             (org-cancelled-kwd-face ((t (,@fg-green ,@bg-base03))))
             (org-delegated-kwd-face ((t (,@fg-cyan ,@bg-base03))))

	     ;; org-agenda
	     (org-agenda-date-today ((t (,@fmt-none ,@fg-purple))))
	     (org-super-agenda-header ((t (,@fmt-big ,@fg-purple))))
	     (org-scheduled-previously ((t (,@fg-white))))
	     (org-scheduled-today ((t (,@fg-white))))
	     (org-upcoming-deadline ((t (,@fg-white))))
	     (org-upcoming-distant-deadline ((t (,@fg-white))))
	     (org-time-grid ((t (,@fg-white))))

	     ;; cfw calendar
	     (cfw:face-toolbar ((t (,@bg-base03))))

	     ;; embark
	     (embark-collect-zebra-highlight ((t (:background "#848484"))))
	     ;; dired
	     (dired-subtree-depth-1-face ((t (,@fmt-none))))
	     (dired-subtree-depth-2-face ((t (,@fmt-none))))
	     (dired-subtree-depth-3-face ((t (,@fmt-none))))
	     (dired-subtree-depth-4-face ((t (,@fmt-none))))
	     (dired-subtree-depth-5-face ((t (,@fmt-none))))
	     (dired-subtree-depth-6-face ((t (,@fmt-none))))
	     (dired-subtree-depth-7-face ((t (,@fmt-none))))
	     (dired-subtree-depth-8-face ((t (,@fmt-none))))
	     (dired-subtree-depth-9-face ((t (,@fmt-none))))
	     

             ;; shr
             (shr-h1 ((t (:height 1.4 ,@fg-blue))))
             (shr-h2 ((t (:height 1.3 ,@fg-darkgreen))))
             (shr-h3 ((t (:height 1.2 ,@fg-darkblue))))
	     (shr-code ((t (,@fmt-ital))))

	     ;; `(shr-abbreviation ((,class :inherit modus-themes-lang-note)))
	     ;; `(shr-code ((,class :inherit modus-themes-markup-verbatim)))
	     ;; `(shr-h1 ((,class :inherit modus-themes-heading-1)))
	     ;; `(shr-h2 ((,class :inherit modus-themes-heading-2)))
	     ;; `(shr-h3 ((,class :inherit modus-themes-heading-3)))
	     ;; `(shr-h4 ((,class :inherit modus-themes-heading-4)))
	     ;; `(shr-h5 ((,class :inherit modus-themes-heading-5)))
	     ;; `(shr-h6 ((,class :inherit modus-themes-heading-6)))
	     ;; `(shr-selected-link ((,class :inherit modus-themes-subtle-red)))

             ;; table
             (table-cell ((t (,@fmt-none ,@fg-base0 ,@bg-back))))

             ;; outline - pandocBlockQuoteLeader*
             (outline-1 ((t (,@fmt-none ,@fg-blue))))
             (outline-2 ((t (,@fmt-none ,@fg-cyan))))
             (outline-3 ((t (,@fmt-none ,@fg-yellow))))
             (outline-4 ((t (,@fmt-none ,@fg-red))))
             (outline-5 ((t (,@fmt-none ,@fg-base0))))
             (outline-6 ((t (,@fmt-none ,@fg-base01))))
             (outline-7 ((t (,@fmt-none ,@fg-orange))))
             (outline-8 ((t (,@fmt-none ,@fg-purple))))
             ;; speedbar
             (speedbar-button-face ((t (,@fmt-none ,@fg-base1))))
             (speedbar-directory-face ((t (,@fmt-none ,@fg-orange))))
             (speedbar-file-face ((t (,@fmt-none ,@fg-green))))
             (speedbar-highlight-face ((t (,@bg-base02))))
             (speedbar-selected-face ((t (,@fmt-undr ,@fg-yellow))))
             (speedbar-separator-face ((t (,@fmt-stnd))))
             (speedbar-tag-face ((t (,@fmt-none ,@fg-blue))))
             ;; show-paren - MatchParen
             (show-paren-match ((t (,@fmt-bold ,@fg-cyan ,@bg-base02))))
             (show-paren-mismatch ((t (,@fmt-bold ,@fg-red ,@bg-base01))))
             ;; widgets
             (widget-field
              ((t (,@fg-base1 ,@bg-base02 :box (:line-width 1)
                              :inherit default))))
             (widget-single-line-field ((t (:inherit widget-field))))
             ;; extra modules
             ;; -------------
	     ;; bm visual bookmarks
	     (bm-fringe-face ((t (,@bg-orange ,@fg-base03))))
	     (bm-fringe-persistent-face ((t (,@bg-blue ,@fg-base03))))
             ;; Flymake
             (flymake-errline ((t (,@fmt-redwave)))) ; ErrorMsg
             (flymake-warnline ((t (,@fmt-orangewave)))) ; WarningMsg
             ;; column-marker
             (column-marker-1 ((t (,@bg-base01))))
             (column-marker-2 ((t (,@bg-cyan))))
             (column-marker-3 ((t (,@bg-purple))))
             ;; jabber
             (jabber-activity-face ((t (,@fmt-bold ,@fg-red))))
             (jabber-activity-personal-face ((t (,@fmt-bold ,@fg-blue))))
             (jabber-chat-error ((t (,@fmt-bold ,@fg-red))))
             (jabber-chat-prompt-foreign ((t (,@fmt-bold ,@fg-red))))
             (jabber-chat-prompt-local ((t (,@fmt-bold ,@fg-blue))))
             (jabber-chat-prompt-system ((t (,@fmt-bold ,@fg-green))))
             (jabber-chat-text-foreign ((t (,@fg-base1))))
             (jabber-chat-text-local ((t (,@fg-base0))))
             (jabber-chat-rare-time-face ((t (,@fmt-undr ,@fg-green))))
             (jabber-roster-user-away ((t (,@fmt-ital ,@fg-green))))
             (jabber-roster-user-chatty ((t (,@fmt-bold ,@fg-orange))))
             (jabber-roster-user-dnd ((t (,@fmt-ital ,@fg-red))))
             (jabber-roster-user-error ((t (:weight light :slant italic ,@fg-red))))
             (jabber-roster-user-offline ((t (,@fg-base01))))
             (jabber-roster-user-online ((t (,@fmt-bold ,@fg-blue))))
             (jabber-roster-user-xa ((t (,@fmt-ital ,@fg-magenta))))
	     ;; git-gutter
	     (git-gutter:modified ((t (,@fg-purple))))
	     (git-gutter:added ((t (,@fg-green))))
	     (git-gutter:deleted ((t (,@fg-red))))
             ;; gnus - these are taken from mutt, not VIM
             (gnus-cite-1 ((t (,@fmt-none ,@fg-blue)))) ; quoted
             (gnus-cite-2 ((t (,@fmt-none ,@fg-cyan)))) ; quoted1
             (gnus-cite-3 ((t (,@fmt-none ,@fg-yellow)))) ; quoted2
             (gnus-cite-4 ((t (,@fmt-none ,@fg-red)))) ; quoted3
             (gnus-cite-5 ((t (,@fmt-none ,@fg-orange)))) ; quoted4
             (gnus-cite-6 ((t (,@fmt-none ,@fg-purple))))
             (gnus-cite-7 ((t (,@fmt-none ,@fg-green))))
             (gnus-cite-8 ((t (,@fmt-none ,@fg-magenta))))
             (gnus-cite-9 ((t (,@fmt-none ,@fg-base00))))
             (gnus-cite-10 ((t (,@fmt-none ,@fg-base01))))
             (gnus-cite-11 ((t (,@fmt-none ,@fg-base02))))
             (gnus-group-mail-1 ((t (,@fmt-bold ,@fg-base3))))
             (gnus-group-mail-1-empty ((t (,@fg-base3))))
             (gnus-group-mail-2 ((t (,@fmt-bold ,@fg-base2))))
             (gnus-group-mail-2-empty ((t (,@fg-base2))))
             (gnus-group-mail-3 ((t (,@fmt-bold ,@fg-magenta))))
             (gnus-group-mail-3-empty ((t (,@fg-magenta))))
             (gnus-group-mail-low ((t (,@fmt-bold ,@fg-base00))))
             (gnus-group-mail-low-empty ((t (,@fg-base00))))
             (gnus-group-news-1 ((t (,@fmt-bold ,@fg-base1))))
             (gnus-group-news-1-empty ((t (,@fg-base1))))
             (gnus-group-news-2 ((t (,@fmt-bold ,@fg-blue))))
             (gnus-group-news-2-empty ((t (,@fg-blue))))
             (gnus-group-news-low ((t (,@fmt-bold ,@fg-purple))))
             (gnus-group-news-low-empty ((t (,@fg-purple))))
             (gnus-emphasis-highlight-words ; highlight
              ((t (,@fmt-none ,fg-yellow))))
             (gnus-header-content ((t (,@fmt-none ,@fg-base01)))) ; hdrdefault
             (gnus-header-from ((t (,@fmt-none ,@fg-base00)))) ; header ^From
             (gnus-header-name ((t (,@fmt-none ,@fg-base01)))) ; hdrdefault
             (gnus-header-newsgroups ; hdrdefault
              ((t (,@fmt-none ,@fg-base02))))
             (gnus-header-subject ; header ^Subject
              ((t (,@fmt-none ,@fg-blue))))
             (gnus-server-agent ((t (,@fmt-bold ,@fg-base3))))
             (gnus-server-closed ((t (,@fmt-ital ,@fg-base1))))
             (gnus-server-denied ((t (,@fmt-bold ,@fg-base2))))
             (gnus-server-offline ((t (,@fmt-bold ,@fg-green))))
             (gnus-server-opened ((t (,@fmt-bold ,@fg-cyan))))
             (gnus-signature ((t (,@fmt-none ,@fg-base01)))) ; signature
             (gnus-splash ((t (,@fg-base2))))
             (gnus-summary-cancelled ; deleted messages
              ((t (,@fmt-none ,@fg-red))))
             (gnus-summary-high-ancient
              ((t (,@fmt-bold :inherit gnus-summary-normal-ancient))))
             (gnus-summary-high-read
              ((t (,@fmt-bold :inherit gnus-summary-normal-read))))
             (gnus-summary-high-ticked
              ((t (,@fmt-bold :inherit gnus-summary-normal-ticked))))
             (gnus-summary-high-undownloaded
              ((t (,@fmt-bold :inherit gnus-summary-normal-undownloaded))))
             (gnus-summary-high-unread
              ((t (,@fmt-bold :inherit gnus-summary-normal-unread))))
             (gnus-summary-low-ancient
              ((t (,@fmt-ital :inherit gnus-summary-normal-ancient))))
             (gnus-summary-low-read
              ((t (,@fmt-ital :inherit gnus-summary-normal-ancient))))
             (gnus-summary-low-unread
              ((t (,@fmt-ital :inherit gnus-summary-normal-unread))))
             (gnus-summary-low-ticked
              ((t (,@fmt-ital :inherit gnus-summary-normal-ancient))))
             (gnus-summary-low-undownloaded
              ((t (,@fmt-ital :inherit gnus-summary-normal-ancient))))
             (gnus-summary-normal-ancient ; old messages
              ((t (,@fmt-none ,@fg-blue))))
             (gnus-summary-normal-read ; read messages
              ((t (,@fmt-none ,@fg-base01))))
             (gnus-summary-normal-ticked ; flagged
              ((t (,@fmt-none ,@fg-red))))
             (gnus-summary-normal-undownloaded ((t (,@fmt-none ,@fg-base2))))
             (gnus-summary-normal-unread ; unread messages
              ((t (,@fmt-none ,@fg-blue))))
             (gnus-summary-selected ; indicator
              ((t (,@fmt-none ,@fg-base03 ,@bg-yellow))))
             ;; magit
             (magit-bisect-bad ((t ,@fg-red)))
             (magit-bisect-good ((t ,@fg-green)))
             (magit-biset-skip ((t ,@fg-blue)))
             (magit-branch-current ((t ,@fg-green)))
             (magit-branch-local ((t ,@fg-cyan)))
             (magit-branch-remote ((t ,@fg-yellow)))
             (magit-cherry-equivalent ((t ,@fg-base0)))
             (magit-cherry-unmatched ((t ,@fg-orange)))
             (magit-dimmed ((t ,@fmt-ital ,@fg-base01)))
             (magit-hash ((t ,@fg-base2)))
             (magit-header-line ((t ,@fmt-bold ,@fg-blue)))
             (magit-log-author ((t ,@fg-cyan)))
             (magit-log-date ((t ,@fg-blue)))
             (magit-log-graph ((t ,@fg-base01)))
             (magit-refname ((t ,@fg-magenta)))
             (magit-process-ng ((t ,@fmt-bold ,@fg-red)))
             (magit-process-ok ((t ,@fmt-bold ,@fg-green)))
             (magit-section-heading ((t ,@fmt-bold ,@fg-cyan)))
             (magit-siganture-unmatched ((t ,@fg-magenta)))
             (magit-siganture-untrusted ((t ,@fg-magenta)))
             (magit-signature-bad ((t ,@fg-red)))
             (magit-signature-good ((t ,@fg-green)))
             (magit-tag ((t ,@fg-orange)))
             ;; Message
             (message-mml ((t (,@fg-blue))))
             (message-cited-text ((t (,@fg-base2))))
             (message-separator ((t (,@fg-base3))))
             (message-header-xheader ((t (,@fg-purple))))
             (message-header-name ((t (,@fg-cyan))))
             (message-header-other ((t (,@fg-red))))
             (message-header-newsgroups ((t (,@fmt-bldi ,@fg-yellow))))
             (message-header-subject ((t (,@fg-base00))))
             (message-header-cc ((t (,@fmt-bold ,@fg-green))))
             (message-header-to ((t (,@fmt-bold ,@fg-base1))))
             ;; parenface
             (paren-face ((t (,@fg-base01))))
             ;; rainbow-delimiters
             (rainbow-delimiters-depth-1-face ((t (,@fg-cyan))))
             (rainbow-delimiters-depth-2-face ((t (,@fg-yellow))))
             (rainbow-delimiters-depth-3-face ((t (,@fg-blue))))
             (rainbow-delimiters-depth-4-face ((t (,@fg-red))))
             (rainbow-delimiters-depth-5-face ((t (,@fg-green))))
             (rainbow-delimiters-depth-6-face ((t (,@fg-blue))))
             (rainbow-delimiters-depth-7-face ((t (,@fg-orange))))
             (rainbow-delimiters-depth-8-face ((t (,@fg-magenta))))
             (rainbow-delimiters-depth-9-face ((t (,@fg-base0))))
             ;; slime
             (slime-error-face ((t (,@fmt-revr ,@fg-red)))) ; ErrorMsg
             (slime-note-face ((t (,@fg-yellow))))
             (slime-repl-inputted-output-face ((t (,@fg-red))))
             (slime-repl-output-mouseover-face ((t (:box (:color ,base3)))))
             (slime-style-warning-face ((t (,@fmt-bold ,@fg-orange))))
             (slime-warning-face ((t (,@fmt-bold ,@fg-red)))) ; WarningMsg
             ;; smart-mode-line
             ; use (setq sml/theme nil) for these settings to take effect
             (sml/charging ((t (,@fg-yellow))))
             (sml/client ((t (,@fg-purple))))
             (sml/col-number ((t (,@fmt-bb ,@fg-base2))))
             (sml/discharging ((t (,@fg-magenta))))
             (sml/filename ((t (,@fmt-bold ,@fg-green))))
             (sml/folder ((t (,@fmt-none ,@fg-blue))))
             (sml/git ((t (,@fmt-none ,@fg-orange))))
             (sml/global ((t (,@fg-base1))))
             (sml/line-number ((t (,@fmt-bb ,@fg-base3))))
             (sml/minor-modes ((t (,@fmt-none ,@fg-base01))))
             (sml/modes ((t (,@fmt-bb ,@fg-base3))))
             (sml/modified ((t (,@fg-red))))
             (sml/numbers-separator ((t (,@fmt-none ,@fg-base01))))
             (sml/name-filling ((t (,@fg-cyan))))
             (sml/numbers-separator ((t (,@fg-base00))))
             (sml/outside-modified ((t (,@fmt-none ,@fg-red))))
             (sml/position-percentage ((t (,@fmt-bb ,@fg-purple))))
             (sml/prefix ((t (,@fmt-none ,@fg-cyan))))
             (sml/process ((t (,@fg-cyan))))
             (sml/read-only ((t (,@fmt-none ,@fg-yellow))))
             (sml/sudo ((t (,@fmt-bold ,@fg-red))))
             (sml/time ((t (,@fmt-bold ,@fg-base3))))
             (sml/vc ((t (,@fg-green))))
             (sml/vc-edited ((t (,@fg-orange))))
             ;; whitespace
             (whitespace-empty ((t (,@fg-red))))
             (whitespace-hspace ((t (,@fg-orange))))
             (whitespace-indentation ((t (,@fg-base02))))
             (whitespace-space ((t (,@fg-base02))))
             (whitespace-space-after-tab ((t (,@fg-cyan))))
             (whitespace-space-before-tab ((t (,@fmt-bold ,@fg-red))))
             (whitespace-tab ((t (,@fg-base02))))
             (whitespace-trailing ((t (,@fmt-bold ,@fg-red ,@bg-base02))))
             (whitespace-highlight-face ((t (,@fg-red ,@bg-blue))))
             (whitespace-line ((t (,@fg-magenta ,@bg-base03))))
	     ;; persp
	     (persp-selected-face ((t (:foreground ,white :underline t))))
             ;; rcirc
             (rcirc-my-nick ((t (:foreground ,blue))))
             (rcirc-nick-in-message ((t (:foreground ,orange))))
             (rcirc-other-nick ((t (:foreground ,green))))
             (rcirc-prompt ((t (:foreground ,yellow))))
             (rcirc-bright-nick ((t (:foreground ,magenta))))
             (rcirc-server ((t (:foreground ,base1))))
             (rcirc-timestamp ((t (:foreground ,base01))))
             ;; ERC
             (erc-input-face ((t (:foreground ,base01))))
             (erc-keyword-face ((t (,@fmt-bldi ,@fg-yellow))))
             (erc-my-nick-face ((t (:foreground ,blue))))
             (erc-nick-defaunoctilux-face ((t (,@fmt-none ,@fg-cyan))))
             (erc-notice-face ((t (,@fmt-none ,@fg-blue))))
             (erc-timestamp-face ((t (:foreground ,base01))))

	     ;; avy
             (avy-lead-face   ((t (:inherit default ,@fg-base03 ,@bg-red))))
             (avy-lead-face-0 ((t (:inherit default ,@fg-base03 ,@bg-purple))))
             (avy-lead-face-1 ((t (:inherit default))))
             (avy-lead-face-2 ((t (:inherit default))))

	     ;; tab-line
	     (tab-line ((t (,@bg-base03))))

	     ;; tab-bar
	     (tab-bar ((t (,@bg-base03))))
	     (tab-bar-tab ((t (,@fmt-undr ,@bg-base03))))
	     (tab-bar-tab-inactive ((t (,@bg-base03))))

	     ;; lsp

             ;; evil
             (evil-ex-lazy-highlight ((t :inherit lazy-highlight)))
             (evil-ex-search ((t :inherit isearch)))
             (evil-ex-substitute-matches ((t ,@fmt-strk ,@fg-red)))
             (evil-ex-substitute-replacement ((t ,@fmt-undr ,@fg-darkgreen)))
             ;;font-latex
             (font-latex-warning-face ((t (,@fg-red))))
             (font-latex-sectioning-5-face ((t (,@fg-purple))))
             ;;flyspell
             (flyspell-incorrect ((t (,@fg-red))))
             (flyspell-duplicate ((t (,@fg-yellow))))
             ;; company-mode
             (company-tooltip ((t (,@fg-base0 ,@bg-base02))))
             (company-tooltip-selection ((t (,@fg-base0 ,@bg-base01))))
             (company-tooltip-mouse ((t (,@bg-base02))))
             (company-tooltip-common ((t (,@bg-base02 ,@fg-purple))))
             (company-tooltip-common-selection ((t (,@fg-purple ,@bg-base01))))
             (company-tooltip-annotation ((t (,@fg-base0 ,@bg-base02))))
             (company-scrollbar-fg ((t (,@bg-base01))))
             (company-scrollbar-bg ((t (,@bg-base3))))
             (company-preview ((t (,@fg-base0 ,@bg-base01))))
             (company-preview-common ((t (,@fg-base0 ,@bg-base01))))
             (company-preview-search ((t (,@fg-purple ,@bg-base01))))
             (company-echo ((t nil)))
             (company-echo-common ((t (,@fg-purple))))
	     ;;ansi-term
	     (term-color-black ((t ( ,@fg-base02))))
	     (term-color-red ((t ( ,@fg-red))))
	     (term-color-green ((t ( ,@fg-green))))
	     (term-color-yellow ((t ( ,@fg-yellow))))
	     (term-color-blue ((t ( ,@fg-blue))))
	     (term-color-magenta ((t ( ,@fg-magenta))))
	     (term-color-cyan ((t ( ,@fg-cyan))))
	     (term-color-white ((t ( ,@fg-base00)))))

            ((foreground-color . ,(when (<= 16 (display-color-cells)) base0))
             (background-color . ,back)
             (background-mode . ,mode)
             (cursor-color . ,(when (<= 16 (display-color-cells))
                                base0))
	     (ansi-color-names-vector . [,base02 ,red ,green ,yellow ,blue ,magenta ,cyan ,base00]))))))))

(defun set-additional-faces ()
  (custom-set-faces
   `(ediff-odd-diff-A ((t (:foreground "#ffffff" :background "#848484"))))
   `(ediff-odd-diff-B ((t (:foreground "#ffffff" :background "#848484"))))
   `(ediff-odd-diff-C ((t (:foreground "#ffffff" :background "#848484"))))
   `(ediff-even-diff-A ((t (:foreground "#ffffff" :background "#848484"))))
   `(ediff-even-diff-B ((t (:foreground "#ffffff" :background "#848484"))))
   `(ediff-even-diff-C ((t (:foreground "#ffffff" :background "#848484"))))))


(defmacro create-noctilux-theme ()
  (let* ((theme-name 'noctilux)
         (defs (noctilux-color-definitions 'dark))
         (theme-vars (mapcar (lambda (def) (list (car def) (cdr def)))
                             (cadr defs)))
         (theme-faces (car defs)))
    `(progn
       (deftheme ,theme-name ,noctilux-description)
       (apply 'custom-theme-set-variables ',theme-name ',theme-vars)
       (apply 'custom-theme-set-faces ',theme-name ',theme-faces)
       (set-additional-faces)
       (provide-theme ',theme-name))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(create-noctilux-theme)

(provide-theme 'noctilux)

;;; noctilux-theme.el ends here
