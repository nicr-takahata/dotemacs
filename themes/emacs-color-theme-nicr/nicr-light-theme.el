(deftheme nicr-light "NiCr-light color theme")

(let ((class '((class color) (min-colors 89) (background light)))
      ;; NiCr color palette
      (nicr-white             "#ffffff")
      (nicr-fg                "#606060")
      (nicr-bg                "#ffffff")
      (nicr-base01            "#465457")
      (nicr-base02            "#455354")
      (nicr-base03            "#293739")
      (nicr-red-1             "#f87373")
      (nicr-red               "#ff0000")
      (nicr-pink              "#f92672")
      (nicr-orange-1          "#fd971f")
      (nicr-orange            "#e96529")
      (nicr-yellow            "#ffff00")
      (nicr-darkgoldenrod     "#e6db74")
      (nicr-wheat             "#c4be89")
      (nicr-olive             "#808000")
      (nicr-chartreuse        "#a6e22e")
      (nicr-lime              "#00ff00")
      (nicr-yellowgreen       "#83a300")
      (nicr-green             "#008000")
      (nicr-darkwine          "#1e0010")
      (nicr-maroon            "#800000")
      (nicr-wine              "#960050")
      (nicr-teal              "#008080")
      (nicr-aqua              "#00ffff")
      (nicr-blue-3            "#88ddff")
      (nicr-blue-2            "#77e9ef")
      (nicr-blue-1            "#66d9ef")
      (nicr-blue              "#0064ff")
      (nicr-dodgerblue        "#13354a")
      (nicr-slateblue         "#7070f0")
      (nicr-purple-1          "#7f58c6")
      (nicr-purple            "#7b4eff")
      (nicr-palevioletred     "#d33682")
      (nicr-grey-7            "#f0f0f0")
      (nicr-grey-6            "#e0e0e0")
      (nicr-grey-5            "#d0d0d0")
      (nicr-grey-4            "#c0c0c0")
      (nicr-grey-3            "#b0b0b0")
      (nicr-grey-2            "#a0a0a0")
      (nicr-grey-1            "#909090")
      (nicr-grey              "#808080")
      (nicr-grey+1            "#707070")
      (nicr-grey+2            "#606060")
      (nicr-grey+3            "#505050")
      (nicr-grey+4            "#404040")
      (nicr-grey+5            "#303030")
      (nicr-grey+6            "#202020")
      (nicr-grey+7            "#101010")
      (nicr-dark              "#000000")
      (nicr-black             "#000000")
      (google-blue            "#4285f4")
      (google-red             "#ea4335")
      (google-yellow          "#fbbc05")
      (google-green           "#34a853")
      (qiita-green            "#79b74a")
      (facebook-blue          "#3a5795")
      (yahoo-blue             "#4070ff")
      (yahoo-lightblue        "#4070ff")
      (twitter-lightblue      "#56b0ec")
      (foursquare-blue        "#1865df")
      (fedex-purple           "#472289"))

(custom-theme-set-faces
  'nicr-light
  ;; M-x list-color-displayでカラーコード一覧表表示

  ;; base
  `(default ((t (:background ,nicr-bg :foreground ,nicr-fg))))
  `(cursor ((t (:background ,nicr-fg :foreground ,nicr-bg))))
  `(fringe ((t (:foreground ,nicr-bg :background ,nicr-bg))))
  `(highlight ((t (:background ,nicr-grey))))
  `(region ((t (:background  ,nicr-grey+1))
            (t :inverse-video t)))
  `(warning ((t (:foreground ,nicr-palevioletred :weight bold))))

  ;; font lock
  ;; ビルトイン
  `(font-lock-builtin-face ((t (:foreground ,nicr-green))))
  ;; コメント
  `(font-lock-comment-face ((t (:background ,nicr-bg :foreground ,nicr-grey-3))))
  `(font-lock-comment-delimiter-face ((t (:background ,nicr-bg :foreground ,nicr-grey-3))))
  ;; 定数
  `(font-lock-constant-face ((t (:foreground ,google-yellow))))
  `(font-lock-doc-string-face ((t (:foreground ,nicr-darkgoldenrod))))
  ;; 関数名
  `(font-lock-function-name-face ((t (:foreground ,google-blue))))
  ;; キーワード
  `(font-lock-keyword-face ((t (:foreground ,google-red))))
  ;; for easily-overlooked negation characters.
  `(font-lock-negation-char-face ((t (:foreground ,nicr-wine))))
  ;; 数値
  `(font-lock-number-face ((t (:foreground,nicr-blue))))
  `(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
  `(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
  `(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
  ;; 文字列
  `(font-lock-string-face ((t (:foreground ,nicr-orange-1))))
  ;; データ型名
  `(font-lock-type-face ((t (:foreground ,nicr-blue))))
  ;; 変数名
  `(font-lock-variable-name-face ((t (:foreground ,qiita-green))))
  `(font-lock-warning-face ((t (:foreground ,nicr-palevioletred :weight bold))))

  ;; mode line
  `(mode-line ((t (:foreground ,nicr-fg
                               :background ,nicr-base03
                               :box nil))))
  `(mode-line-buffer-id ((t (:weight bold))))
  `(mode-line-inactive ((t (:foreground ,nicr-fg
                                        :background ,nicr-base02
                                        :box nil))))

  ;; search
  `(isearch ((t (:foreground ,nicr-dark :background ,nicr-wheat :weight bold))))
  `(isearch-fail ((t (:foreground ,nicr-wine :background ,nicr-darkwine))))

  ;; linum-mode
  `(linum ((t (:foreground ,nicr-grey+3 :background ,nicr-bg))))

  ;; hl-line-mode
  `(hl-line-face ((,class (:background ,nicr-grey+3)) (t :weight bold)))
  `(hl-line ((,class (:background ,nicr-grey+3)) (t :weight bold)))

  ;; TODO
  ;; ido-mode
  ;; flycheck
  ;; show-paren
  ;; rainbow-delimiters
  ;; highlight-symbols

  ;; neotree
  `(neo-banner-face ((t (:foreground ,nicr-darkgoldenrod))))
  `(neo-header-face ((t (:foreground ,nicr-yellowgreen))))
  `(neo-root-dir-face ((t (:foreground ,nicr-red-1 :weight bold))))
  `(neo-dir-link-face ((t (:foreground ,nicr-yellowgreen))))
  `(neo-expand-btn-face ((t (:foreground ,nicr-orange :weight bold))))
  ))

(provide-theme 'nicr-light)
