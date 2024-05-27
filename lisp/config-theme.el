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
  (set (car col) (cdr col)))

(with-eval-after-load 'evil
  (evil-ex-define-cmd "faces" 'list-faces-display))

;; Fonts

(cl-defun --find-and-set-fonts ()
  (interactive)
  (let ((sarasa-mono (font-spec :family "Sarasa Mono J" :size 14))
        (iosevka (font-spec :family "Iosevka" :size 14))
        (iosevka-etoile (font-spec :family "Iosevka Etoile" :size 14))
        (iosevka-aile (font-spec :family "Iosevka Aile" :size 14))
        (courier (font-spec :family "Courier" :size 14)))
    (cond
     ((find-font sarasa-mono)
      (set-frame-font sarasa-mono nil t)
      (custom-set-faces `(fixed-pitch ((t (:font ,sarasa-mono))))
                        `(variable-pitch ((t (:font ,sarasa-mono))))
                        `(fixed-pitch-serif ((t (:font ,sarasa-mono))))))
     ((find-font iosevka)
      (set-frame-font iosevka nil t)
      (custom-set-faces `(fixed-pitch ((t (:font ,iosevka))))
                        `(variable-pitch ((t (:font ,iosevka))))
                        `(fixed-pitch-serif ((t (:font ,iosevka)))))))
    ;; If iosevka proportional fonts are also found, use that.
    (when (find-font iosevka-etoile)
      (custom-set-faces `(variable-pitch ((t (:font ,iosevka-etoile))))))
    (when (find-font iosevka-aile)
      (custom-set-faces `(fixed-pitch-serif ((t (:font ,iosevka-aile))))))))

(add-hook 'after-init-hook #'--find-and-set-fonts)
(add-hook 'server-after-make-frame-hook #'--find-and-set-fonts)

;; Nano theme

(use-package nano-theme
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
  (load-theme (or (bound-and-true-p --default-background) 'nano-light) t))

(customize-set-variable 'frame-background-mode 'nil)

(defface sol-superlight-foreground
  `((((background light)) (:foreground ,sol-base2))
    (((background dark)) (:foreground ,sol-base02)))
  "Very light foreground coloring"
  :group 'personal)

(defface sol-light-foreground
  `((((background light)) (:foreground ,sol-base1))
    (((background dark)) (:foreground ,sol-base01)))
  "Somewhat light foreground coloring"
  :group 'personal)

(defface sol-superlight-background
  `((((background light)) (:background ,sol-base2))
    (((background dark)) (:background ,sol-base02)))
  "Very light background coloring"
  :group 'personal)

(defface sol-superlight-underline
  `((((background light)) (:underline ,sol-base2))
    (((background dark)) (:underline ,sol-base02)))
  "Very light underline coloring"
  :group 'personal)

(defface sol-light-underline
  `((((background light)) (:underline ,sol-base1))
    (((background dark)) (:underline ,sol-base01)))
  "Light underline coloring"
  :group 'personal)

(defface sol-superlight-overline
  `((((background light)) (:overline ,sol-base2))
    (((background dark)) (:overline ,sol-base02)))
  "Very light overline coloring"
  :group 'personal)

(defface sol-superlight-box
  `((((background light)) (:box (:line-width (-1 . -1)
                                 :color ,sol-base2)))
    (((background dark)) (:box (:line-width (-1 . -1)
                                :color ,sol-base02))))
  "Very light box coloring"
  :group 'personal)

(defface sol-background-box
  `((((background light)) (:box (:line-width (-1 . -1)
                                 :color ,sol-base3)))
    (((background dark)) (:box (:line-width (-1 . -1)
                                :color ,sol-base03))))
  "Background box coloring"
  :group 'personal)

(with-eval-after-load 'simple
  (when (facep 'blink-matching-paren-offscreen)
    (set-face-attribute 'blink-matching-paren-offscreen nil
                        :bold t
                        :foreground 'unspecified
                        :inherit 'nano-critical)))

(with-eval-after-load 'faces
  (set-face-attribute 'link-visited nil
                      :foreground sol-blue)
  (set-face-attribute 'region nil
                      :inverse-video t
                      :inherit '(nano-subtle))
  (set-face-attribute 'fringe nil
                      :inherit 'sol-light-foreground)
  (set-face-attribute 'variable-pitch-text nil
                      :height 1)
  (set-face-attribute 'fill-column-indicator nil
                      :inherit 'sol-superlight-foreground)
  (set-face-attribute 'show-paren-match nil
                      :foreground 'unspecified)
  (set-face-attribute 'italic nil
                      :italic t)
  (set-face-attribute 'vertical-border nil
                      :foreground 'unspecified
                      :inherit 'sol-superlight-foreground)
  (set-face-attribute 'mode-line nil
                      :box '(:line-width 3))
  (set-face-attribute 'mode-line-active nil
                      :inherit '(sol-light-foreground
                                 sol-superlight-box))
  (set-face-attribute 'mode-line-inactive nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :overline 'unspecified
                      :underline 'unspecified
                      :box 'unspecified
                      :inherit '(sol-superlight-box
                                 sol-superlight-foreground
                                 nano-default))
  (set-face-attribute 'mode-line-emphasis nil
                      :inherit 'nano-subtle)
  (set-face-attribute 'mode-line-buffer-id nil
                      :box 'unspecified
                      :inherit '(sol-light-foreground))
  (set-face-attribute 'header-line nil
                      :background 'unspecified
                      :inherit 'nano-default)
  (set-face-attribute 'help-key-binding nil
                      :foreground sol-yellow
                      :box nil
                      :background 'unspecified)
  (set-face-attribute 'link nil
                      :foreground sol-yellow
                      :underline nil
                      :bold t)
  (set-face-attribute 'line-number nil
                      :inherit 'sol-superlight-foreground)
  (set-face-attribute 'line-number-current-line nil
                      :inherit 'sol-light-foreground))

(with-eval-after-load 'flymake
  (custom-set-faces
    `(flymake-error ((((background light))
                      (:box ,sol-base1))
                     (((background dark))
                      (:box ,sol-base01))))
    `(flymake-note ((((background light))
                     (:underline ,sol-base2))
                    (((background dark))
                     (:underline ,sol-base02))))
    `(flymake-warning ((((background light))
                        (:underline ,sol-base1))
                       (((background dark))
                        (:underline ,sol-base01))))))

(with-eval-after-load 'flycheck
  (custom-set-faces
    `(flycheck-error ((((background light))
                       (:box ,sol-base1))
                      (((background dark))
                       (:box ,sol-base01))))
    `(flycheck-info ((((background light))
                      (:underline ,sol-base2))
                     (((background dark))
                      (:underline ,sol-base02))))
    `(flycheck-warning ((((background light))
                         (:underline ,sol-base1))
                        (((background dark))
                         (:underline ,sol-base01))))))

(with-eval-after-load 'pulse
  (set-face-attribute 'pulse-highlight-start-face nil
                      :background 'unspecified
                      :inherit 'sol-superlight-background))

(with-eval-after-load 'evil
  (set-face-attribute 'evil-ex-info nil
                      :foreground sol-red)
  (set-face-attribute 'evil-ex-substitute-replacement nil
                      :foreground sol-red))

(with-eval-after-load 'diff
  (set-face-attribute 'diff-removed nil
                      :foreground sol-red
                      :inherit 'nano-default)
  (set-face-attribute 'diff-refine-removed nil
                      :inherit 'diff-removed))

(with-eval-after-load 'ediff
  (set-face-attribute 'ediff-current-diff-A nil
                      :foreground sol-red
                      :background 'unspecified
                      :inherit 'sol-superlight-background)
  (set-face-attribute 'ediff-fine-diff-A nil
                      :foreground sol-red
                      :bold t
                      :background 'unspecified
                      :inherit 'sol-superlight-background)
  (set-face-attribute 'ediff-current-diff-Ancestor nil
                      :foreground sol-blue
                      :background 'unspecified
                      :inherit 'sol-superlight-background)
  (set-face-attribute 'ediff-fine-diff-Ancestor nil
                      :foreground sol-blue
                      :bold t
                      :background 'unspecified
                      :inherit 'sol-superlight-background)
  (set-face-attribute 'ediff-current-diff-B nil
                      :foreground sol-green
                      :background 'unspecified
                      :inherit 'sol-superlight-background)
  (set-face-attribute 'ediff-fine-diff-B nil
                      :foreground sol-green
                      :bold t
                      :background 'unspecified
                      :inherit 'sol-superlight-background)
  (set-face-attribute 'ediff-current-diff-C nil
                      :foreground sol-yellow
                      :background 'unspecified
                      :inherit 'sol-superlight-background)
  (set-face-attribute 'ediff-fine-diff-C nil
                      :foreground sol-yellow
                      :bold t
                      :background 'unspecified
                      :inherit 'sol-superlight-background)

  (set-face-attribute 'ediff-even-diff-A nil
                      :foreground sol-red
                      :background 'unspecified
                      :inherit nil)
  (set-face-attribute 'ediff-even-diff-Ancestor nil
                      :foreground sol-blue
                      :background 'unspecified
                      :inherit nil)
  (set-face-attribute 'ediff-even-diff-B nil
                      :foreground sol-green
                      :background 'unspecified
                      :inherit nil)
  (set-face-attribute 'ediff-even-diff-C nil
                      :foreground sol-yellow
                      :background 'unspecified
                      :inherit nil)

  (set-face-attribute 'ediff-odd-diff-A nil
                      :background 'unspecified
                      :inherit 'ediff-even-diff-A)
  (set-face-attribute 'ediff-odd-diff-Ancestor nil
                      :background 'unspecified
                      :inherit 'ediff-even-diff-Ancestor)
  (set-face-attribute 'ediff-odd-diff-B nil
                      :background 'unspecified
                      :inherit 'ediff-even-diff-B)
  (set-face-attribute 'ediff-odd-diff-C nil
                      :background 'unspecified
                      :inherit 'ediff-even-diff-C))

(with-eval-after-load 'magit
 (set-face-attribute 'magit-diff-revision-summary nil
                     :bold 'unspecified
                     :inherit 'nano-strong)
 (set-face-attribute 'magit-diff-hunk-heading nil
                     :bold t
                     :inherit 'nano-faded)
 (set-face-attribute 'magit-head nil
                     :bold t
                     :inherit 'nano-salient))

(with-eval-after-load 'magit-sequence
  (set-face-attribute 'magit-sequence-head nil
                      :inherit 'nano-salient)
  (set-face-attribute 'magit-sequence-drop nil
                      :strike-through t))

(with-eval-after-load 're-builder
  (set-face-attribute 'reb-match-0 nil
                      :foreground sol-cyan
                      :background 'unspecified)
  (set-face-attribute 'reb-match-1 nil
                      :foreground sol-blue
                      :background 'unspecified)
  (set-face-attribute 'reb-match-2 nil
                      :foreground sol-violet
                      :background 'unspecified)
  (set-face-attribute 'reb-match-3 nil
                      :foreground sol-magenta
                      :background 'unspecified))

(with-eval-after-load 'transient
  (set-face-attribute 'transient-key-exit nil
                      :foreground sol-red)
  (set-face-attribute 'transient-key-return nil
                      :foreground sol-yellow)
  (set-face-attribute 'transient-key-stay nil
                      :foreground sol-blue))

(with-eval-after-load 'sh-script
  (set-face-attribute 'sh-quoted-exec nil
                      :foreground 'unspecified
                      :inherit 'nano-salient))

(with-eval-after-load 'markdown-mode
  (set-face-attribute 'markdown-highlighting-face nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :box t
                      :inherit 'nano-default)
  (set-face-attribute 'markdown-strike-through-face nil
                      :strike-through t
                      :inherit 'nano-faded)
  (set-face-attribute 'markdown-inline-code-face nil
                      :inherit 'nano-subtle)
  (set-face-attribute 'markdown-code-face nil
                      :extend t
                      :inherit 'nano-subtle)
  (set-face-attribute 'markdown-table-face nil
                      :inherit 'nano-subtle)
  (set-face-attribute 'markdown-reference-face nil
                      :inherit 'nano-default)
  (set-face-attribute 'markdown-italic-face nil
                      :italic t
                      :inherit 'nano-default)
  (set-face-attribute 'markdown-bold-face nil
                      :bold t
                      :inherit 'nano-default)
  (set-face-attribute 'markdown-plain-url-face nil
                      :italic t
                      :inherit 'nano-default)
  (set-face-attribute 'markdown-url-face nil
                      :italic t
                      :inherit 'nano-faded)
  (set-face-attribute 'markdown-link-face nil
                      :italic t
                      :inherit 'nano-default)
  (set-face-attribute 'markdown-blockquote-face nil
                      :inherit 'nano-subtle)

  (defface --markdown-date-timestamp-face
    '((t (:weight semi-light)))
    "Face used to describe date timestamps. Like years and months and days."
    :group 'personal)
  (defconst --markdown-date-timestamp-regex
    (rx (= 4 (any digit)) "-" (any "0-1") (any digit) "-" (any "0-3") (any digit)))

  (defface --markdown-time-timestamp-face
    '((t (:weight semi-light)))
    "Face used to describe time timestamps. Like hours and minutes."
    :group 'personal)
  (defconst --markdown-time-timestamp-regex
    (rx (= 2 (any digit)) ":" (= 2 (any digit))
        (* ":" (= 2 (any digit)))
        (* " +" (= 4 (any digit)))))

  (defface --markdown-tag-face
    '((t (:weight light :inherit nano-faded)))
    "Face used to describe tags (like `#foo'). I like using tags."
    :group 'personal)
  (defconst --markdown-tag-keyword-regex
    (rx (or line-start space) "#" (one-or-more (any alnum "_" "-"))))

  (font-lock-add-keywords
   'markdown-mode `((,--markdown-tag-keyword-regex 0 '--markdown-tag-face)
                    (,--markdown-date-timestamp-regex 0 '--markdown-date-timestamp-face)
                    (,--markdown-time-timestamp-regex 0 '--markdown-time-timestamp-face)))
  (font-lock-add-keywords
   'gfm-mode `((,--markdown-tag-keyword-regex 0 '--markdown-tag-face)
               (,--markdown-date-timestamp-regex 0 '--markdown-date-timestamp-face)
               (,--markdown-time-timestamp-regex 0 '--markdown-time-timestamp-face))))

(with-eval-after-load 'isearch
  (set-face-attribute 'lazy-highlight nil
                      :inherit 'match))

(with-eval-after-load 'highlight-indent-guides
  (set-face-attribute 'highlight-indent-guides-character-face nil
                      :inherit 'sol-superlight-foreground))

;; font-lock
(set-face-attribute 'font-lock-warning-face nil
                    :bold t
                    :italic t
                    :inherit 'nano-default)
(set-face-attribute 'font-lock-keyword-face nil
                    :foreground 'unspecified
                    :inherit 'nano-default)
(set-face-attribute 'font-lock-constant-face nil
                    :foreground 'unspecified
                    :inherit 'nano-default)
(set-face-attribute 'font-lock-function-name-face nil
                    :foreground 'unspecified
                    :inherit 'nano-default)
(set-face-attribute 'font-lock-builtin-face nil
                    :foreground 'unspecified
                    :inherit 'nano-default)
(set-face-attribute 'font-lock-variable-name-face nil
                    :foreground 'unspecified
                    :inherit 'nano-strong)
(set-face-attribute 'font-lock-comment-face nil
                    :foreground 'unspecified
                    :inherit '(nano-faded sol-superlight-background))
(set-face-attribute 'font-lock-doc-face nil
                    :foreground 'unspecified
                    :extend t
                    :inherit '(nano-default sol-superlight-background))
(set-face-attribute 'font-lock-type-face nil
                    :italic t
                    :foreground 'unspecified
                    :inherit 'nano-default)
(set-face-attribute 'font-lock-string-face nil
                    :foreground 'unspecified)

(with-eval-after-load 'blamer
  (set-face-attribute 'blamer-face nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :weight 'extra-light
                      :inherit 'sol-superlight-foreground))

(with-eval-after-load 'eww
  (set-face-attribute 'eww-form-submit nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'nano-strong)
  (set-face-attribute 'eww-form-textarea nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'nano-default)
  (set-face-attribute 'eww-form-text nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'nano-subtle)
  (set-face-attribute 'eww-form-select nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'nano-salient)
  (set-face-attribute 'eww-form-file nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'nano-default)
  (set-face-attribute 'eww-form-checkbox nil
                      :box t
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'nano-subtle))

(with-eval-after-load 'tree-sitter-hl
  (set-face-attribute 'tree-sitter-hl-face:function nil
                      :inherit 'nano-default)
  (set-face-attribute 'tree-sitter-hl-face:function.call nil
                      :inherit 'tree-sitter-hl-face:function)
  (set-face-attribute 'tree-sitter-hl-face:function.builtin nil
                      :inherit 'tree-sitter-hl-face:function)

  (set-face-attribute 'tree-sitter-hl-face:function.macro nil
                      :inherit 'tree-sitter-hl-face:function)

  (set-face-attribute 'tree-sitter-hl-face:method nil
                      :inherit 'tree-sitter-hl-face:function)
  (set-face-attribute 'tree-sitter-hl-face:method.call nil
                      :inherit 'tree-sitter-hl-face:method)

  (set-face-attribute 'tree-sitter-hl-face:variable.builtin nil
                      :inherit 'tree-sitter-hl-face:variable)
  (set-face-attribute 'tree-sitter-hl-face:variable.special nil
                      :inherit 'nano-strong)

  (set-face-attribute 'tree-sitter-hl-face:attribute nil
                      :inherit 'nano-default)

  (set-face-attribute 'tree-sitter-hl-face:type nil
                      :inherit 'font-lock-type-face)
  (set-face-attribute 'tree-sitter-hl-face:type.argument nil
                      :inherit 'tree-sitter-hl-face:type)
  (set-face-attribute 'tree-sitter-hl-face:type.builtin nil
                      :inherit 'tree-sitter-hl-face:type)
  (set-face-attribute 'tree-sitter-hl-face:type.super nil
                      :inherit 'tree-sitter-hl-face:type)

  (set-face-attribute 'tree-sitter-hl-face:property nil
                      :italic nil
                      :inherit 'font-lock-function-name-face))

(with-eval-after-load 'macrostep-expand
  (set-face-attribute 'macrostep-expansion-highlight-face nil
                      :foreground sol-green
                      :background 'unspecified
                      :inherit 'nano-subtle))

(with-eval-after-load 'term
 (set-face-attribute 'term-color-red nil
                     :foreground sol-red
                     :background sol-red)
 (set-face-attribute 'term-color-green nil
                     :foreground sol-green
                     :background sol-green)
 (set-face-attribute 'term-color-yellow nil
                     :foreground sol-yellow
                     :background sol-yellow)
 (set-face-attribute 'term-color-blue nil
                     :foreground sol-blue
                     :background sol-blue)
 (set-face-attribute 'term-color-magenta nil
                     :foreground sol-magenta
                     :background sol-magenta)
 (set-face-attribute 'term-color-cyan nil
                     :foreground sol-cyan
                     :background sol-cyan)
 (set-face-attribute 'term-color-white nil
                     :foreground sol-base2
                     :background sol-base2))

(with-eval-after-load 'smerge-mode
  (set-face-attribute 'smerge-markers nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'nano-subtle)
  (set-face-attribute 'smerge-base nil
                      :foreground sol-blue
                      :background 'unspecified)
  (set-face-attribute 'smerge-lower nil
                      :foreground sol-green
                      :background 'unspecified)
  (set-face-attribute 'smerge-upper nil
                      :foreground sol-red
                      :background 'unspecified)
  (set-face-attribute 'smerge-refined-added nil
                      :background sol-green
                      :inherit 'nano-default-i)
  (set-face-attribute 'smerge-refined-removed nil
                      :background sol-red
                      :inherit 'nano-default-i))

(with-eval-after-load 'wgrep
  (set-face-attribute 'wgrep-face nil
                      :foreground sol-green
                      :background 'unspecified)
  (set-face-attribute 'wgrep-file-face nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'wgrep-face)
  (set-face-attribute 'wgrep-done-face nil
                      :foreground sol-blue
                      :background 'unspecified)
  (set-face-attribute 'wgrep-reject-face nil
                      :foreground sol-red)
  (set-face-attribute 'wgrep-delete-face nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'wgrep-face
                      :strike-through t))

(with-eval-after-load 'compile
  (set-face-attribute 'compilation-mode-line-run nil
                      :background 'unspecified
                      :foreground 'unspecified
                      :inherit 'nano-default)
  (set-face-attribute 'compilation-warning nil
                      :inherit 'nano-salient)
  (set-face-attribute 'compilation-mode-line-exit nil
                      :background 'unspecified
                      :foreground 'unspecified
                      :bold 'unspecified
                      :inherit 'nano-default))

(with-eval-after-load 'undo-tree
  (set-face-attribute 'undo-tree-visualizer-default-face nil
                      :foreground 'unspecified
                      :inherit 'nano-default)
  (set-face-attribute 'undo-tree-visualizer-active-branch-face nil
                      :bold 'unspecified
                      :foreground 'unspecified
                      :inherit 'nano-strong)
  (set-face-attribute 'undo-tree-visualizer-current-face nil
                      :foreground 'unspecified
                      :inherit 'nano-salient)
  (set-face-attribute 'undo-tree-visualizer-register-face nil
                      :foreground sol-yellow)
  (set-face-attribute 'undo-tree-visualizer-unmodified-face nil
                      :foreground sol-blue))

(with-eval-after-load 'tab-bar
  (set-face-attribute 'tab-bar nil
                      :underline nil
                      :inherit 'minibuffer-prompt)
  (set-face-attribute 'tab-bar-tab nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :underline 'unspecified
                      :bold nil
                      :inherit '(sol-light-underline default))
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :bold nil
                      :inherit 'sol-light-foreground))

(with-eval-after-load 'avy
  (set-face-attribute 'avy-background-face nil
                      :foreground 'unspecified
                      :inherit 'nano-default-i)
  (set-face-attribute 'avy-lead-face nil
                      :background sol-red
                      :foreground 'unspecified
                      :inherit 'nano-default-i)
  (set-face-attribute 'avy-lead-face-0 nil
                      :background sol-magenta
                      :foreground 'unspecified
                      :inherit 'nano-default-i)
  (set-face-attribute 'avy-lead-face-1 nil
                      :background sol-violet
                      :foreground 'unspecified
                      :inherit 'nano-default-i)
  (set-face-attribute 'avy-lead-face-2 nil
                      :background sol-blue
                      :foreground 'unspecified
                      :inherit 'nano-default-i))

(with-eval-after-load 'corfu
  (set-face-attribute 'corfu-current nil
                      :inverse-video t
                      :bold 'unspecified
                      :inherit 'nano-faded))

(with-eval-after-load 'stripe-buffer
  (set-face-attribute 'stripe-highlight nil
                      :background 'unspecified
                      :extend t
                      :inherit 'nano-subtle))

(with-eval-after-load 'lsp-mode
  (set-face-attribute 'lsp-details-face nil
                      :height 1.0
                      :weight 'normal
                      :inherit 'sol-light-foreground))

(with-eval-after-load 'lsp-ui-peek
  (set-face-attribute 'lsp-ui-peek-peek nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'nano-subtle)
  (set-face-attribute 'lsp-ui-peek-list nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'nano-subtle)
  (set-face-attribute 'lsp-ui-peek-selection nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'nano-strong-i)
  (set-face-attribute 'lsp-ui-peek-line-number nil
                      :foreground 'unspecified
                      :inherit 'nano-default)
  (set-face-attribute 'lsp-ui-peek-filename nil
                      :foreground 'unspecified
                      :inherit 'nano-salient)
  (set-face-attribute 'lsp-ui-peek-header nil
                      :bold t
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'nano-subtle)
  (set-face-attribute 'lsp-ui-peek-highlight nil
                      :foreground 'unspecified
                      :box 'unspecified
                      :background 'unspecified
                      :inherit 'nano-strong))

(with-eval-after-load 'lsp-diagnostics
  (customize-set-variable
   'lsp-diagnostics-attributes
   `((unnecessary :foreground ,(face-attribute
                                 'sol-light-foreground
                                 :foreground))
     (deprecated :strike-through t))))

(with-eval-after-load 'lsp-headerline
 (set-face-attribute 'lsp-headerline-breadcrumb-path-error-face nil
                     :underline `(:color ,sol-red :style line))
 (set-face-attribute 'lsp-headerline-breadcrumb-symbols-error-face nil
                     :underline `(:color ,sol-red :style line))
 (set-face-attribute 'lsp-headerline-breadcrumb-path-hint-face nil
                     :underline `(:color ,sol-magenta :style line))
 (set-face-attribute 'lsp-headerline-breadcrumb-symbols-hint-face nil
                     :underline `(:color ,sol-magenta :style line))
 (set-face-attribute 'lsp-headerline-breadcrumb-path-info-face nil
                     :underline `(:color ,sol-blue :style line))
 (set-face-attribute 'lsp-headerline-breadcrumb-symbols-info-face nil
                     :underline `(:color ,sol-blue :style line))
 (set-face-attribute 'lsp-headerline-breadcrumb-path-warning-face nil
                     :underline `(:color ,sol-yellow :style line))
 (set-face-attribute 'lsp-headerline-breadcrumb-symbols-warning-face nil
                     :underline `(:color ,sol-yellow :style line)))

(with-eval-after-load 'lsp-ui-doc
  (setq lsp-ui-doc-border
        (plist-get
         (custom-face-attributes-get 'sol-superlight-foreground nil)
         :foreground))

  (set-face-attribute 'lsp-ui-doc-background nil
                      :background 'unspecified
                      :inherit 'nano-subtle)
  (set-face-attribute 'lsp-ui-doc-header nil
                      :background 'unspecified
                      :foreground 'unspecified
                      :inherit '(nano-salient nano-subtle)))

(with-eval-after-load 'focus
  (set-face-attribute 'focus-unfocused nil
                      :inherit 'sol-superlight-foreground))

(with-eval-after-load 'lsp-modeline
  (set-face-attribute 'lsp-modeline-code-actions-face nil
                      ;; consider just making this nano-faded or something
                      :foreground sol-yellow)
  (set-face-attribute 'lsp-modeline-code-actions-preferred-face nil
                      :foreground sol-red))

(with-eval-after-load 'lsp-headerline
  (set-face-attribute 'lsp-headerline-breadcrumb-symbols-face nil
                      :bold nil
                      :inherit 'nano-default))

(with-eval-after-load 'orderless
  (set-face-attribute 'orderless-match-face-0 nil
                      :inherit 'nano-strong))

(with-eval-after-load 'dired-git-info
  (set-face-attribute 'dgi-commit-message-face nil
                      :inherit 'default))

(with-eval-after-load 'dired
  (set-face-attribute 'dired-directory nil
                      :bold t))

(with-eval-after-load 'ace-window
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground 'unspecified
                      :inherit 'nano-critical-i))

(with-eval-after-load 'ansi-color
  (set-face-attribute 'ansi-color-blue nil
                      :foreground sol-blue)
  (set-face-attribute 'ansi-color-bright-blue nil
                      :background sol-blue
                      :inherit 'nano-default-i)
  (set-face-attribute 'ansi-color-cyan nil
                      :foreground sol-cyan)
  (set-face-attribute 'ansi-color-bright-cyan nil
                      :background sol-cyan
                      :inherit 'nano-default-i)
  (set-face-attribute 'ansi-color-green nil
                      :foreground sol-green)
  (set-face-attribute 'ansi-color-bright-green nil
                      :background sol-green
                      :inherit 'nano-default-i)
  (set-face-attribute 'ansi-color-magenta nil
                      :foreground sol-magenta)
  (set-face-attribute 'ansi-color-bright-magenta nil
                      :background sol-magenta
                      :inherit 'nano-default-i)
  (set-face-attribute 'ansi-color-red nil
                      :foreground sol-red)
  (set-face-attribute 'ansi-color-bright-red nil
                      :background sol-red
                      :inherit 'nano-default-i)
  (set-face-attribute 'ansi-color-yellow nil
                      :foreground sol-yellow)
  (set-face-attribute 'ansi-color-bright-yellow nil
                      :background sol-yellow
                      :inherit 'nano-default-i))

(with-eval-after-load 'scopeline
  (set-face-attribute 'scopeline-face nil
                      :inherit 'sol-superlight-foreground))

(with-eval-after-load 'dired-hacks
  (set-face-attribute 'dired-subtree-depth-1-face nil
                      :inherit 'sol-superlight-background)
  (set-face-attribute 'dired-subtree-depth-2-face nil
                      :inherit 'sol-superlight-background)
  (set-face-attribute 'dired-subtree-depth-3-face nil
                      :inherit 'sol-superlight-background)
  (set-face-attribute 'dired-subtree-depth-4-face nil
                      :inherit 'sol-superlight-background)
  (set-face-attribute 'dired-subtree-depth-5-face nil
                      :inherit 'sol-superlight-background)
  (set-face-attribute 'dired-subtree-depth-6-face nil
                      :inherit 'sol-superlight-background))

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
