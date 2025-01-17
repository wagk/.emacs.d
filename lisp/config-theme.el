;; Solarized 1.0.0beta2[a] Color Palette[8]
;; | Color   |    |     |     | sRGB    |     |     |     | xterm | Terminal  | Usage                          |
;; |---------+----+-----+-----+---------+-----+-----+-----+-------+-----------+--------------------------------|
;; | Name    | L* | a*  | b*  | Hex     |   R |   G |   B |  Code | Name      |                                |
;; |---------+----+-----+-----+---------+-----+-----+-----+-------+-----------+--------------------------------|
;; | Base03  | 15 | −12 | −12 | #002b36 |   0 |  43 |  54 |   234 | brblack   | background tones (dark theme)  |
;; | Base02  | 20 | −12 | −12 | #073642 |   7 |  54 |  66 |   235 | black     | background tones (dark theme)  |
;; | Base01  | 45 | −07 | −07 | #586e75 |  88 | 110 | 117 |   240 | brgreen   | content tones                  |
;; | Base00  | 50 | −07 | −07 | #657b83 | 101 | 123 | 131 |   241 | bryellow  | content tones                  |
;; | Base0   | 60 | −06 | −03 | #839496 | 131 | 148 | 150 |   244 | brblue    | content tones                  |
;; | Base1   | 65 | −05 | −02 | #93a1a1 | 147 | 161 | 161 |   245 | brcyan    | content tones                  |
;; | Base2   | 92 | −00 | 10  | #eee8d5 | 238 | 232 | 213 |   254 | white     | background tones (light theme) |
;; | Base3   | 97 | 00  | 10  | #fdf6e3 | 253 | 246 | 227 |   230 | brwhite   | background tones (light theme) |
;; | Yellow  | 60 | 10  | 65  | #b58900 | 181 | 137 |   0 |   136 | yellow    | accent tones                   |
;; | Orange  | 50 | 50  | 55  | #cb4b16 | 203 |  75 |  22 |   166 | brred     | accent tones                   |
;; | Red     | 50 | 65  | 45  | #dc322f | 220 |  50 |  47 |   160 | red       | accent tones                   |
;; | Magenta | 50 | 65  | −05 | #d33682 | 211 |  54 | 130 |   125 | magenta   | accent tones                   |
;; | Violet  | 50 | 15  | −45 | #6c71c4 | 108 | 113 | 196 |    61 | brmagenta | accent tones                   |
;; | Blue    | 55 | −10 | −45 | #268bd2 |  38 | 139 | 210 |    33 | blue      | accent tones                   |
;; | Cyan    | 60 | −35 | −05 | #2aa198 |  42 | 161 | 152 |    37 | cyan      | accent tones                   |
;; | Green   | 60 | −20 | 65  | #859900 | 133 | 153 |   0 |    64 | green     | accent tones                   |

;; | SOLARIZED | HEX     | 16/8 | TERMCOL   | XTERM/HEX   | L*A*B      | RGB         | HSB         |
;; |-----------|---------|------|-----------|-------------|------------|-------------|-------------|
;; | base03    | #002b36 | 8/4  | brblack   | 234 #1c1c1c | 15 -12 -12 |   0  43  54 | 193 100  21 |
;; | base02    | #073642 | 0/4  | black     | 235 #262626 | 20 -12 -12 |   7  54  66 | 192  90  26 |
;; | base01    | #586e75 | 10/7 | brgreen   | 240 #585858 | 45 -07 -07 |  88 110 117 | 194  25  46 |
;; | base00    | #657b83 | 11/7 | bryello   | 241 #626262 | 50 -07 -07 | 101 123 131 | 195  23  51 |
;; | base0     | #839496 | 12/6 | brblue    | 244 #808080 | 60 -06 -03 | 131 148 150 | 186  13  59 |
;; | base1     | #93a1a1 | 14/4 | brcyan    | 245 #8a8a8a | 65 -05 -02 | 147 161 161 | 180   9  63 |
;; | base2     | #eee8d5 | 7/7  | white     | 254 #e4e4e4 | 92 -00  10 | 238 232 213 |  44  11  93 |
;; | base3     | #fdf6e3 | 15/7 | brwhite   | 230 #ffffd7 | 97  00  10 | 253 246 227 |  44  10  99 |
;; | yellow    | #b58900 | 3/3  | yellow    | 136 #af8700 | 60  10  65 | 181 137   0 |  45 100  71 |
;; | orange    | #cb4b16 | 9/3  | brred     | 166 #d75f00 | 50  50  55 | 203  75  22 |  18  89  80 |
;; | red       | #dc322f | 1/1  | red       | 160 #d70000 | 50  65  45 | 220  50  47 |   1  79  86 |
;; | magenta   | #d33682 | 5/5  | magenta   | 125 #af005f | 50  65 -05 | 211  54 130 | 331  74  83 |
;; | violet    | #6c71c4 | 13/5 | brmagenta | 61  #5f5faf | 50  15 -45 | 108 113 196 | 237  45  77 |
;; | blue      | #268bd2 | 4/4  | blue      | 33  #0087ff | 55 -10 -45 |  38 139 210 | 205  82  82 |
;; | cyan      | #2aa198 | 6/6  | cyan      | 37  #00afaf | 60 -35 -05 |  42 161 152 | 175  74  63 |
;; | green     | #859900 | 2/2  | green     | 64  #5f8700 | 60 -20  65 | 133 153   0 |  68 100  60 |

(dolist (col '((sol-base03  . "#002b36")
               (sol-base02  . "#073642")
               (sol-base01  . "#586e75")
               (sol-base00  . "#657b83")
               (sol-base0   . "#839496")
               (sol-base1   . "#93a1a1")
               (sol-base2   . "#eee8d5")
               (sol-base3   . "#fdf6e3")
               (sol-yellow  . "#b58900")
               (sol-orange  . "#cb4b16")
               (sol-red     . "#dc322f")
               (sol-magenta . "#d33682")
               (sol-violet  . "#6c71c4")
               (sol-blue    . "#268bd2")
               (sol-cyan    . "#2aa198")
               (sol-green   . "#859900")))
  ;; TODO: set documentation string
  ;; configure faces like this if and only if it's not already defined
  ;; in `local.el'
  (unless (boundp (car col))
    (set (car col) (cdr col))))

(with-eval-after-load 'evil
  (evil-ex-define-cmd "faces" 'list-faces-display))

;; Fonts

(cl-defun --find-and-set-fonts ()
  (interactive)
  (let ((sarasa-mono (font-spec :family "Sarasa Mono J" :size --default-font-size))
        (iosevka (font-spec :family "Iosevka" :size --default-font-size))
        (iosevka-etoile (font-spec :family "Iosevka Etoile" :size --default-font-size))
        (iosevka-aile (font-spec :family "Iosevka Aile" :size --default-font-size))
        (courier (font-spec :family "Courier" :size --default-font-size)))
    (cond
     ((find-font sarasa-mono)
      (set-frame-font sarasa-mono nil t)
      (custom-set-faces `(fixed-pitch ((default . (:font ,sarasa-mono))))
                        `(variable-pitch ((default . (:font ,sarasa-mono))))
                        `(fixed-pitch-serif ((default . (:font ,sarasa-mono))))))
     ((find-font iosevka)
      (set-frame-font iosevka nil t)
      (custom-set-faces `(fixed-pitch ((default . (:font ,iosevka))))
                        `(variable-pitch ((default . (:font ,iosevka))))
                        `(fixed-pitch-serif ((default . (:font ,iosevka)))))))
    ;; If iosevka proportional fonts are also found, use that.
    (when (find-font iosevka-etoile)
      (custom-set-faces `(variable-pitch ((default . (:font ,iosevka-etoile))))))
    (when (find-font iosevka-aile)
      (custom-set-faces `(fixed-pitch-serif ((default . (:font ,iosevka-aile))))))))

(defcustom --default-font-size 20
  "Default frame font size."
  :group 'personal
  :type 'natnum
  :set
  #'(lambda (obj size)
      (set-default-toplevel-value obj size)
      (with-eval-after-load 'config-theme
        (--find-and-set-fonts))))

(add-hook 'after-init-hook #'--find-and-set-fonts)
(add-hook 'server-after-make-frame-hook #'--find-and-set-fonts)

(use-package solarized-definitions
  :ensure (:host github :repo "sellout/emacs-color-theme-solarized" :main nil)
  :config
  (load-theme 'solarized t))

;; Nano theme

(use-package nano-theme
  :disabled t
  :ensure (:host github :repo "emacs-straight/nano-theme" :branch "main")
  :custom
  (nano-window-divider-show t)
  (nano-fonts-use nil)) ;; for now

(customize-set-variable 'nano-light-foreground sol-base00)
(customize-set-variable 'nano-light-background sol-base3)
(customize-set-variable 'nano-light-highlight  sol-base2)
(customize-set-variable 'nano-light-subtle     sol-base2)
(customize-set-variable 'nano-light-faded      sol-base0)
(customize-set-variable 'nano-light-salient    sol-green)
(customize-set-variable 'nano-light-strong     sol-base01)
(customize-set-variable 'nano-light-popout     sol-orange)
(customize-set-variable 'nano-light-critical   sol-red)

(customize-set-variable 'nano-dark-foreground  sol-base0)
(customize-set-variable 'nano-dark-background  sol-base03)
(customize-set-variable 'nano-dark-highlight   sol-base02)
(customize-set-variable 'nano-dark-subtle      sol-base02)
(customize-set-variable 'nano-dark-faded       sol-base00)
(customize-set-variable 'nano-dark-salient     sol-cyan)
(customize-set-variable 'nano-dark-strong      sol-base1)
(customize-set-variable 'nano-dark-popout      sol-orange)
(customize-set-variable 'nano-dark-critical    sol-red)

(with-eval-after-load 'nano-theme
  ;; (load-theme (or (bound-and-true-p --default-background) 'nano-dark) t))
  (load-theme (or (bound-and-true-p --default-background) 'nano-light) t))

(cl-defun --load-config-theme (&optional dark)
  (interactive)
  ;; (if dark
  ;;     (load-theme 'nano-dark t)
  ;;   (load-theme 'nano-light t))
  (load-file (locate-user-emacs-file (file-name-concat "lisp" "config-theme.el"))))

(customize-set-variable 'frame-background-mode 'nil)

(defface sol-background
  `((((background light)) . (:background ,sol-base3))
    (((background dark)) . (:background ,sol-base03)))
  "Default background coloring."
  :group 'personal)

(defface sol-background-i
  `((((background light)) . (:background ,sol-base01))
    (((background dark)) . (:background ,sol-base1)))
  "Inverted default background coloring."
  :group 'personal)

(defface sol-foreground
  `((((background light)) . (:foreground ,sol-base00))
    (((background dark)) . (:foreground ,sol-base0)))
  "Default foreground coloring."
  :group 'personal)

(defface sol-foreground-i
  `((default . (:inherit sol-background-i))
    (((background light)) . (:foreground ,sol-base3))
    (((background dark)) . (:foreground ,sol-base03)))
  "Inverted default foreground coloring."
  :group 'personal)

(defface sol-foreground-box
  `((((background light)) . (:box ,sol-base00))
    (((background dark)) . (:box ,sol-base0)))
  "Default foreground coloring. In a box."
  :group 'personal)

(defface sol-light-foreground
  `((((background light)) . (:foreground ,sol-base1))
    (((background dark)) . (:foreground ,sol-base01)))
  "Somewhat light foreground coloring"
  :group 'personal)

(defface sol-strong-foreground
  `((((background light)) . (:foreground ,sol-base02))
    (((background dark)) . (:foreground ,sol-base2)))
  "Darker foreground coloring"
  :group 'personal)

(defface sol-superstrong-foreground
  `((((background light)) . (:foreground ,sol-base03))
    (((background dark)) . (:foreground ,sol-base3)))
  "Darkest foreground coloring"
  :group 'personal)

(defface sol-light-background
  `((((background light)) . (:background ,sol-base1))
    (((background dark)) . (:background ,sol-base01)))
  "Somewhat light background coloring"
  :group 'personal)

(defface sol-light-background-i
  `((((background light)) . (:background ,sol-base01))
    (((background dark)) . (:background ,sol-base1)))
  "Somewhat light background coloring"
  :group 'personal)

(defface sol-superlight-foreground
  `((((background light)) . (:foreground ,sol-base2))
    (((background dark)) . (:foreground ,sol-base02)))
  "Very light foreground coloring"
  :group 'personal)

(defface sol-superlight-background
  `((((background light)) . (:background ,sol-base2))
    (((background dark)) . (:background ,sol-base02)))
  "Very light background coloring"
  :group 'personal)

(defface sol-superlight-background-i
  `((((background light)) . (:background ,sol-base02))
    (((background dark)) . (:background ,sol-base2)))
  "Very light background coloring"
  :group 'personal)

(defface sol-superlight-underline
  `((((background light)) . (:underline ,sol-base2))
    (((background dark)) . (:underline ,sol-base02)))
  "Very light underline coloring"
  :group 'personal)

(defface sol-light-underline
  `((((background light)) . (:underline ,sol-base1))
    (((background dark)) . (:underline ,sol-base01)))
  "Light underline coloring"
  :group 'personal)

(defface sol-superlight-overline
  `((((background light)) . (:overline ,sol-base2))
    (((background dark)) . (:overline ,sol-base02)))
  "Very light overline coloring"
  :group 'personal)

(defface sol-superlight-box
  `((((background light)) . (:box (:line-width (-1 . -1)
                                   :color ,sol-base2)))
    (((background dark)) . (:box (:line-width (-1 . -1)
                                  :color ,sol-base02))))
  "Very light box coloring"
  :group 'personal)

(defface sol-background-box
  `((((background light)) . (:box (:line-width (-1 . -1)
                                   :color ,sol-base3)))
    (((background dark)) . (:box (:line-width (-1 . -1)
                                  :color ,sol-base03))))
  "Background box coloring"
  :group 'personal)

(defface nano-subtle
  `((default . (:inherit sol-light-foreground)))
  "nano-subtle shim"
  :group 'nano-shim)

(defface nano-subtle-i
  `((default . (:inherit sol-strong-foreground)))
  "nano-subtle-i shim"
  :group 'nano-shim)

(defface nano-faded
  `((default . (:inherit (sol-light-foreground))))
  "nano-faded shim"
  :group 'nano-shim)

(defface nano-faded-i
  `((((background light)) . (:background ,sol-base3
                             :foreground ,sol-base0))
    (((background dark)) . (:background ,sol-base03
                            :foreground ,sol-base00)))
  "nano-faded-i shim"
  :group 'nano-shim)

(defface nano-default
  `((default . (:inherit sol-foreground)))
  "nano-default shim"
  :group 'nano-shim)

(defface nano-default-i
  `((((background light)) . (:foreground ,sol-base3
                             :background ,sol-base00))
    (((background dark)) . (:foreground ,sol-base03
                            :background ,sol-base0)))
  "nano-default-i shim"
  :group 'nano-shim)

(defface nano-salient
  `((((background light)) . (:foreground ,sol-green))
    (((background dark)) . (:foreground ,sol-cyan)))
  "nano-salient shim"
  :group 'nano-shim)

(defface nano-salient-i
  `((((background light)) . (:foreground ,sol-base3
                             :background ,sol-green))
    (((background dark)) . (:foreground ,sol-base03
                            :background ,sol-cyan)))
  "nano-salient-i shim"
  :group 'nano-shim)

(defface nano-strong
  `((default . (:weight bold
                :inherit sol-light-background-i)))
  "nano-strong shim"
  :group 'nano-shim)

(defface nano-strong-i
  `((default . (:weight normal))
    (((background light)) . (:foreground ,sol-base3
                             :background ,sol-base01))
    (((background dark)) . (:foreground ,sol-base03
                            :background ,sol-base1)))
  "nano-strong-i shim"
  :group 'nano-shim)

(defface nano-popout
  `((default . (:foreground ,sol-orange)))
  "nano-popout shim"
  :group 'nano-shim)

(defface nano-popout-i
  `((default . (:background ,sol-orange))
    (((background light)) . (:foreground ,sol-base3))
    (((background dark)) . (:foreground ,sol-base03)))
  "nano-popout-i shim"
  :group 'nano-shim)

(defface nano-critical
  `((default . (:foreground ,sol-red :weight normal)))
  "nano-critical shim"
  :group 'nano-shim)

(defface nano-critical-i
  `((default . (:background ,sol-red :weight normal))
    (((background light)) . (:foreground ,sol-base3))
    (((background dark)) . (:foreground ,sol-base03)))
  "nano-critical-i shim"
  :group 'nano-shim)

(with-eval-after-load 'simple
  (custom-set-faces
   `(separator-line ((default . (:inherit 'sol-superlight-background)))))

  (when (facep 'blink-matching-paren-offscreen)
    (custom-set-faces
     `(blink-matching-paren-offscreen
       ((default . (:inherit sol-strong-background))
        (((supports (:bold))) . (:bold t)))))))

(with-eval-after-load 'minibuffer
  (custom-set-faces
   `(completions-common-part ((default . (:foreground ,sol-cyan))))))

;; faces.el
(custom-set-faces
 `(warning ((default . (:foreground ,sol-orange))))
 `(success ((default . (:foreground ,sol-green))))
 `(error ((default . (:foreground unspecified
                      :bold t
                      :background ,sol-red
                      :inverse-video nil
                      :inherit sol-foreground-i
                      :background ,sol-red))
          (((supports (:bold)))) . (:bold t)))
 `(minibuffer-prompt ((default . (:foreground ,sol-green))))
 `(shadow ((default . (:foreground unspecified
                       :inherit sol-light-foreground))))
 `(highlight ((default . (:background unspecified
                          :inherit sol-superlight-background))))
 `(link-visited ((default . (:foreground ,sol-blue))))
 `(region ((default . (:foreground unspecified
                       :background unspecified
                       :inverse-video t
                       :inherit sol-superlight-background))))
                     ;; :inherit nano-default))))
 `(fringe ((default . (:foreground unspecified
                       :background unspecified
                       :inherit sol-superlight-foreground))))
 `(variable-pitch-text ((((supports (:height))) . (:height 1))))
 `(fill-column-indicator ((default . (:inherit sol-superlight-foreground))))
 `(show-paren-match ((default . (:foreground unspecified
                                 :background unspecified
                                 :inherit sol-superlight-background))
                     (((supports (:bold))) . (:bold t))))
 `(show-paren-mismatch ((default . (:background ,sol-red
                                    :foreground unspecified
                                    :inherit nano-default-i))))
 `(vertical-border ((default . (:foreground unspecified
                                :inherit sol-superlight-foreground))))
 `(mode-line ((default . (:foreground unspecified
                          :background unspecified
                          :inverse-video nil
                          :inherit sol-superlight-box))))
 `(mode-line-active ((default . (:inherit (sol-light-foreground
                                           sol-superlight-box)))))
 `(mode-line-emphasis ((default . (:inherit nano-subtle))))
 `(mode-line-highlight ((t . (:inherit nano-subtle))))
 `(mode-line-buffer-id ((default . (:box unspecified
                                    :inherit sol-light-foreground))))
 `(mode-line-inactive ((default . (:inverse-video nil
                                   :foreground unspecified
                                   :background unspecified
                                   :overline unspecified
                                   :underline unspecified
                                   :box unspecified
                                   :inherit (sol-superlight-box
                                             sol-superlight-foreground)))))
 `(header-line ((default . (:underline nil
                            :inverse-video nil
                            :foreground unspecified
                            :background unspecified))))
 `(header-line-active ((default . (:inherit header-line))))
 `(help-key-binding ((default . (:foreground ,sol-yellow
                                 :box nil
                                 :background unspecified))))
 `(link ((default . (:foreground ,sol-yellow
                     :underline t
                     :bold nil))))
 `(line-number ((default . (:inherit sol-superlight-foreground))))
 `(line-number-current-line
   ((default . (:inherit sol-light-foreground))))
   ;; ((default . (:inherit (sol-superlight-background
   ;;                      sol-foreground-i)))
   ;;  (((background light)) . (:inherit sol-light-foreground))))
 `(window-divider ((default . (:foreground unspecified
                               :background unspecified)))))

(with-eval-after-load 'info
  (custom-set-faces
   `(info-node
     ((default . (:inherit sol-foreground))))))

(with-eval-after-load 'pulse
  (custom-set-faces
   `(pulse-highlight-start-face
     ((default . (:background unspecified
                  :inherit sol-superlight-background))))))

(with-eval-after-load 'eshell
  (custom-set-faces
   `(eshell-prompt ((default . (:foreground unspecified
                                :inherit ,sol-green))))))

(with-eval-after-load 're-builder
  (custom-set-faces
   `(reb-match-0
     ((default . (:foreground ,sol-cyan))))
   `(reb-match-1
     ((default . (:foreground ,sol-blue))))
   `(reb-match-2
     ((default . (:foreground ,sol-violet))))
   `(reb-match-3
     ((default . (:foreground ,sol-magenta))))))

(with-eval-after-load 'sh-script
  (custom-set-faces
   `(sh-quoted-exec
     ((default . (:inherit nano-salient))))))

(with-eval-after-load 'markdown-mode
  (custom-set-faces
   `(markdown-header-face
     ((default . (:inherit sol-superstrong-foreground))))
   `(markdown-header-face-1
     ((default . (:inherit markdown-header-face))))
   `(markdown-header-face-2
     ((default . (:inherit markdown-header-face))))
   `(markdown-header-face-3
     ((default . (:inherit markdown-header-face))))
   `(markdown-header-face-4
     ((default . (:inherit markdown-header-face))))
   `(markdown-header-face-5
     ((default . (:inherit markdown-header-face))))
   `(markdown-header-face-6
     ((default . (:inherit markdown-header-face))))
   `(markdown-header-delimiter-face
     ((default . (:inherit markdown-header-face))))
   `(markdown-metadata-key-face
     ((default . (:inherit sol-strong-foreground
                  :weight semi-light))))
   `(markdown-metadata-value-face
     ((default . (:inherit sol-foreground
                  :weight semi-light))))
   `(markdown-highlighting-face
     ((default . (:box t))))
   `(markdown-strike-through-face
     ((default . (:strike-through t
                  :inherit nano-faded))))
   `(markdown-inline-code-face
     ((default . (:inherit sol-light-foreground))))
   `(markdown-code-face
     ((default . (:inherit markdown-inline-code-face
                  :extend t))))
   `(markdown-table-face
     ((default . (:inherit sol-light-foreground))))
   `(markdown-reference-face
     ((default . (:inherit sol-light-foreground))))
   `(markdown-italic-face
     ((default . (:italic t))))
   `(markdown-bold-face
     ((default . (:bold t))))
   `(markdown-plain-url-face
     ((default . (:inherit markdown-italic-face))))
   `(markdown-url-face
     ((default . (:inherit (markdown-italic-face
                            sol-light-foreground)))))
   `(markdown-link-face
     ((default . (:inherit markdown-italic-face))))
   `(markdown-blockquote-face
     ((default . (:inherit sol-light-foreground)))))

  (defface --markdown-date-timestamp-face
    '((default . (:weight semi-light)))
    "Face used to describe date timestamps. Like years and months and days."
    :group 'personal)
  (defconst --markdown-date-timestamp-regex
    (rx (= 4 (any digit)) "-" (any "0-1") (any digit) "-" (any "0-3") (any digit)))

  (defface --markdown-time-timestamp-face
    '((default . (:weight semi-light)))
    "Face used to describe time timestamps. Like hours and minutes."
    :group 'personal)
  (defconst --markdown-time-timestamp-regex
    (rx (= 2 (any digit)) ":" (= 2 (any digit))
        (* ":" (= 2 (any digit)))
        (* " +" (= 4 (any digit)))))

  (defface --markdown-tag-face
    '((default . (:weight light :inherit nano-faded)))
    "Face used to describe tags (like `#foo'). I like using tags."
    :group 'personal)
  (defconst --markdown-tag-keyword-regex
    (rx (or line-start space punct) "#" (one-or-more (any alnum "_" "-"))))

  (font-lock-add-keywords
   'markdown-mode `((,--markdown-tag-keyword-regex 0 '--markdown-tag-face)
                    (,--markdown-date-timestamp-regex 0 '--markdown-date-timestamp-face)
                    (,--markdown-time-timestamp-regex 0 '--markdown-time-timestamp-face)))
  (font-lock-add-keywords
   'gfm-mode `((,--markdown-tag-keyword-regex 0 '--markdown-tag-face)
               (,--markdown-date-timestamp-regex 0 '--markdown-date-timestamp-face)
               (,--markdown-time-timestamp-regex 0 '--markdown-time-timestamp-face))))

(with-eval-after-load 'replace
  (custom-set-faces
   `(match
     ((default (:background unspecified
                :distant-foreground unspecified
                :inverse-video nil
                :foreground ,sol-orange))
      (((type nil)) (:inverse-video t))))))

(with-eval-after-load 'isearch
  (custom-set-faces
   `(isearch
     ((default . (:background unspecified
                  :foreground ,sol-green
                  :inherit sol-superlight-background))))
   `(lazy-highlight
     ((default . (:distant-foreground unspecified
                  :background unspecified
                  :inherit match))))))

;; font-lock
(custom-set-faces
 `(font-lock-warning-face
   ((default . (:foreground ,sol-blue))
    (((supports (:bold)) (supports (:italic))) .
     (:bold t :italic t))))
 `(font-lock-keyword-face
   ((default . (:foreground unspecified))))
 `(font-lock-constant-face
   ((default . (:foreground unspecified))))
 `(font-lock-function-name-face
   ((default . (:foreground unspecified))))
 `(font-lock-builtin-face
   ((default . (:foreground unspecified))))
 `(font-lock-variable-name-face
   ((default . (:foreground unspecified
                :inherit sol-superstrong-foreground))
    ;; if character terminal, use colors
    (((type nil)) . (:inherit sol-strong-foreground))
    ;; otherwise make it pop by bolding
    (t . (:bold t :inherit sol-foreground))))
 `(font-lock-comment-face
   ((default . (:italic nil
                :inherit (sol-light-foreground sol-superlight-background)))
    (((type nil)) . (:inherit (sol-superlight-background
                               sol-light-foreground)))
    (((supports (:weight))) . (:weight semi-light))))
 `(font-lock-comment-delimiter-face
   ((default . (:inherit font-lock-comment-face))))
 `(font-lock-doc-face
   ((default . (:extend t
                :inherit font-lock-comment-face))))
 `(font-lock-type-face
   ((default . (:foreground unspecified
                :inherit sol-foreground))
    (((supports (:italic))) . (:italic t))))
 `(font-lock-preprocessor-face
   ((default . (:foreground unspecified
                :inherit sol-foreground))))
 `(font-lock-string-face
   ((default . (:foreground unspecified
                :inherit sol-foreground)))))

(with-eval-after-load 'eww
  (custom-set-faces
   `(eww-form-submit
     ((default . (:inherit nano-strong))))
   `(eww-form-textarea
     ((default . (:inherit sol-foreground))))
   `(eww-form-text
     ((default . (:inherit sol-light-foreground))))
   `(eww-form-select
     ((default . (:foreground ,sol-green))))
   `(eww-form-file
     ((default . (:inherit sol-foreground))))
   `(eww-form-checkbox
     ((default . (:inherit sol-light-foreground
                  :box t))))))

(with-eval-after-load 'sh-script
  (custom-set-faces
   `(sh-heredoc
     ((default . (:foreground ,sol-green))))))

;; this is done primarily for vterm, so if we're not using vterm these
;; values might be wrong
(with-eval-after-load 'ansi-color
  (custom-set-faces
   `(ansi-color-bright-black
     ((default . (:foreground ,sol-base03
                  :background ,sol-base03))))
   `(ansi-color-black
     ((default . (:foreground ,sol-base02
                  :background ,sol-base02))))
   `(ansi-color-bright-blue
     ((default . (:foreground ,sol-base0
                  :background ,sol-base0))))
   `(ansi-color-blue
     ((default . (:foreground ,sol-blue
                  :background ,sol-blue))))
   `(ansi-color-bright-cyan
     ((default . (:foreground ,sol-base1
                  :background ,sol-base1))))
   `(ansi-color-cyan
     ((default . (:foreground ,sol-cyan
                  :background ,sol-cyan))))
   `(ansi-color-bright-green
     ((default . (:foreground ,sol-base01
                  :background ,sol-base01))))
   `(ansi-color-green
     ((default . (:foreground ,sol-green
                  :background ,sol-green))))
   `(ansi-color-bright-magenta
     ((default . (:foreground ,sol-violet
                  :background ,sol-violet))))
   `(ansi-color-magenta
     ((default . (:foreground ,sol-magenta
                  :background ,sol-magenta))))
   `(ansi-color-bright-red
     ((default . (:foreground ,sol-orange
                  :background ,sol-orange))))
   `(ansi-color-red
     ((default . (:foreground ,sol-red
                  :background ,sol-red))))
   `(ansi-color-bright-white
     ((default . (:foreground ,sol-base3
                  :background ,sol-base3))))
   `(ansi-color-bright-yellow
     ((default . (:foreground ,sol-base00
                  :background ,sol-base00))))
   `(ansi-color-yellow
     ((default . (:foreground ,sol-yellow
                  :background ,sol-yellow))))))

(with-eval-after-load 'term
  (custom-set-faces
   `(term-color-red
     ((default . (:foreground ,sol-red
                  :background ,sol-red))))
   `(term-color-green
     ((default . (:foreground ,sol-green
                  :background ,sol-green))))
   `(term-color-yellow
     ((default . (:foreground ,sol-yellow
                  :background ,sol-yellow))))
   `(term-color-green
     ((default . (:foreground ,sol-green
                  :background ,sol-green))))
   `(term-color-blue
     ((default . (:foreground ,sol-blue
                  :background ,sol-blue))))
   `(term-color-magenta
     ((default . (:foreground ,sol-magenta
                  :background ,sol-magenta))))
   `(term-color-cyan
     ((default . (:foreground ,sol-cyan
                  :background ,sol-cyan))))
   `(term-color-white
     ((default . (:foreground ,sol-base2
                  :background ,sol-base2))))))

(with-eval-after-load 'avy
  (custom-set-faces
   `(avy-background-face
     ((default . (:inherit sol-foreground-i))))
   `(avy-lead-face
     ((default . (:background ,sol-green
                  :inherit avy-background-face))))
   `(avy-lead-face-0
     ((default . (:background ,sol-blue
                  :inherit avy-background-face))))
   `(avy-lead-face-1
     ((default . (:background ,sol-cyan
                  :inherit avy-background-face))))
   `(avy-lead-face-2
     ((default . (:background ,sol-violet
                  :inherit avy-background-face))))))

(with-eval-after-load 'adoc-mode
  (custom-set-faces
   `(adoc-gen-face
     ((default . (:foreground ,sol-blue))))))

(with-eval-after-load 'table
  (custom-set-faces
   `(table-cell
     ((default . (:inherit sol-foreground))))))

;; Replaces the default arrows you see in the left and right fringe with a
;; nicer-looking bitmap.
(setf (alist-get 'continuation fringe-indicator-alist) 'empty-line)
(setf (alist-get 'truncation fringe-indicator-alist) 'empty-line)

;; Solarized theme

(use-package solarized-theme
  :disabled t
  :ensure (:host github :repo "bbatsov/solarized-emacs")
  ;; :if (display-graphic-p)
  :custom
  (solarized-use-variable-pitch nil)
  (solarized-distinct-fringe-background nil)
  (solarized-high-contrast-mode-line nil)
  (solarized-use-less-bold t)
  (solarized-use-more-italic nil)
  (solarized-scale-org-headlines nil)
  (solarized-height-minus-1 1.0)
  (solarized-height-plus-1 1.0)
  (solarized-height-plus-2 1.0)
  (solarized-height-plus-3 1.0)
  (solarized-height-plus-4 1.0))

(provide 'config-theme)
