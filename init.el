; M-x eval-bufferでinit.elを反映。

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

; inline patchを有効に
(setq default-input-method "MacOSX")

; 起動画面を抑止
(setq inhibit-startup-message t)

; スクラッチメッセージ（起動時注意書き）を抑止
(setq initial-scratch-message nil)

; optionをメタキーにする。
;(mac-key-mode 1)
;(setq mac-option-modifier 'meta)

; 警告音とフラッシュを無効
(setq ring-bell-function 'ignore)

; バックアップファイルを作らないようにする
(setq make-backup-files nil)

; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

; ウィンドウサイズの位置、サイズ
(if window-system (progn
  (setq initial-frame-alist '((width . 210)(height . 53)(top . 0)(left . 48)))
  (set-background-color "Black")
  (set-foreground-color "White")
  (set-cursor-color "Gray")
))

; ウィンドウの透明化
(add-to-list 'default-frame-alist '(alpha . (0.80 0.80)))

; ツールバーを非表示
; M-x tool-bar-mode で表示非表示を切り替えられる
(tool-bar-mode -1)

; タイトルバーにファイル名表示
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

; シフト + 矢印で範囲選択
;1(setq pc-select-selection-keys-only t)
;(pc-selection-mode 1)

; 改行後にインデント
(global-set-key "\C-m" 'newline-and-indent)

;M-g で指定行へジャンプ
(global-set-key "\M-g" 'goto-line)

;; フォント設定
;(if (eq window-system 'mac) (require 'carbon-font))
;(if window-system (fixed-width-set-fontset "hirakaku_w3" 10))
;(if window-system (setq fixed-width-rescale nil))

;; タブキー
(setq default-tab-width 4)
(setq indent-line-function 'indent-relative-maybe)

;; 何文字目にいるか表示
(column-number-mode 1)

;; 行カーソル
(defface hlline-face
  '((((class color)
      (background dark))
     ;;(:background "dark state gray"))
     (:background "gray10"
                  :underline "gray24"))
    (((class color)
      (background light))
     (:background "ForestGreen"
                  :underline nil))
	(t ()))
  "*Face used by hl-line.")
;;(setq hl-line-face 'underline)
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)

;; 行番号を表示する
;; 表示切替はM-x wb-line-number-toggleと入力。
;(require 'wb-line-number)
;(setq truncate-partial-width-windows nil)
;(set-scroll-bar-mode nil)
;(setq wb-line-number-scroll-bar t)
;(wb-line-number-toggle)

;(setq elscreen-prefix-key (kbd "C-z"))
;(elscreen-start)
