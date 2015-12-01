(deftheme nicr-light
  "Face colors NiCr-light.")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'nicr-light
   `(default ((,class (:background "LightBlue" :foreground "black"))))
   `(cursor ((,class (:background "red"))))
   `(fringe ((,class (:background "gray85"))))
   ;; Highlighting faces
   `(highlight ((,class (:background "cyan"))))
   `(region ((,class (:background "MediumAquamarine"))))
   `(secondary-selection ((,class (:background "white" :foreground "black"))))
   `(isearch ((,class (:background "green" :foreground "Black"))))
   `(lazy-highlight ((,class (:background "dark turquoise"))))
   `(query-replace ((,class (:inherit isearch :background "white" :foreground "black"))))
   `(match ((,class (:background "SkyBlue"))))
   ;; Mode line faces
   `(mode-line ((,class (:background "PaleGoldenrod" :foreground "black" :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id ((,class (:overline "red" :underline "red"))))
   `(mode-line-inactive ((,class (:inherit mode-line :background "LightGray" :foreground "grey20" :box (:line-width -1 :color "grey75") :weight light))))
   ;; Escape and prompt faces
   `(escape-glyph ((,class (:background "gold" :foreground "blue" :box (:line-width 1 :color "blue" :style released-button)))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#b35caf"))))
   `(font-lock-constant-face ((,class (:foreground "#00006DE06DE0"))))
   `(font-lock-function-name-face ((,class (:foreground "red"))))
   `(font-lock-keyword-face ((,class (:foreground "Blue3"))))
   `(font-lock-string-face ((,class (:foreground "Magenta4"))))
   `(font-lock-warning-face ((,class (:foreground "orange red" :weight bold))))
   ;; Compilation faces
   `(next-error ((,class (:inherit region :background "SkyBlue"))))))

(provide-theme 'nicr-light)
