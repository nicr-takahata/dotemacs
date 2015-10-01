;;; init.el by jidaikobo-shibata
;;; thx 『Emacs実践入門』
;;; thx http://rubikitch.com

;;; usage: init.elの反映
;; M-x eval-buffer RET init.el

;;; usage: 利用前の準備
;; このinit.elを~/.emacs.dに入れる前に、以下手順を踏んでおくこと。
;; @ terminal
;; mkdir -p ~/.emacs.d/elisp
;; cd ~/.emacs.d/elisp
;; curl -0 http://www.emacswiki.org/emacs/download/auto-install.el
;; @ emacs
;; S式はC-x C-Eで反映すること
;; 下記コマンド類はM-;でコメントを外してからやるとよい
;; M-x byte-compile-file RET ~/.emacs.d/elisp/auto-install.el RET
;; (add-to-list 'load-path "~/.emacs.d/elisp")
;; (require 'auto-install)
;; (setq auto-install-directory "~/.emacs.d/elisp")
;; (auto-install-update-emacswiki-package-name t)
;; (auto-install-compatibility-setup)
;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; (package-initialize)
;; M-x install-elisp RET http://cx4a.org/pub/undohist.el
;; M-x auto-install-batch RET anything RET
;; M-x package-install RET auto-complete RET
;; M-x package-install RET recentf RET
;; M-x package-install RET cursor-chg RET
;; M-x package-install RET smart-tab
;; M-x package-install RET multi-term RET
;; M-x package-install RET google-translate RET

;;; load-pathの追加
(add-to-list 'load-path "~/.emacs.d/elisp")

;;; auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

;;; undohist
(require 'undohist)
(undohist-initialize)

;;; multi-term
(load "multi-term-init")

;;; recentf
(require 'recentf-ext)
(recentf-mode 1)

;;; Anything


;;; cursor-chg
(require 'cursor-chg)
(toggle-cursor-type-when-idle 1)

;;; smart-tab
(require 'smart-tab)
(global-smart-tab-mode)

;;; google-translate
(load "google-translate")

;;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;;; Rictyを等幅で使う
;; ~/.emacs.d/elisp/ricty.elc
(load "ricty")

;; inline patchを有効に
(setq default-input-method "MacOSX")

;; 起動画面を抑止
(setq inhibit-startup-message t)

;; スクラッチメッセージ（起動時注意書き）を抑止
(setq initial-scratch-message nil)

;; オートインデント無効
(electric-indent-mode -1)

;; 警告音とフラッシュを無効
(setq ring-bell-function 'ignore)

;; バックアップファイルを作らないようにする
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;;; キーバインド登録
;; thx http://d.hatena.ne.jp/gan2/20080109/1199887209
;; cmd+[cursor]関係
(define-key global-map [s-up] 'beginning-of-buffer)
(define-key global-map [s-down] 'end-of-buffer)
(define-key global-map [s-left] 'beginning-of-line)
(define-key global-map [s-right] 'end-of-line)
;; Shift+Returnで<br />を入力
(define-key global-map [S-return] "<br />")
;; opt+\でバックスラッシュを入力
(define-key global-map (kbd "M-¥") "\\")
;; ウィンドウ切り替え
(define-key global-map (kbd "C-t") 'other-window)
;; M-g で指定行へジャンプ
(global-set-key "\M-g" 'goto-line)

;; ウィンドウサイズの位置、サイズ
(if window-system (progn
  (setq initial-frame-alist '((width . 210)(height . 53)(top . 0)(left . 0)))
  (set-background-color "Black")
  (set-foreground-color "White")
  (set-cursor-color "Gray")
))

;; ウィンドウの透明化
(add-to-list 'default-frame-alist '(alpha . (0.90 0.90)))

;; ツールバーを非表示
;; M-x tool-bar-mode で表示非表示を切り替えられる
(tool-bar-mode -1)

;; タイトルバーにファイル名表示
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;; 何文字目にいるか表示
(column-number-mode 1)

;; 行番号を表示する
;; 表示切替はM-x wb-line-number-toggleと入力。
(require 'linum)
(global-linum-mode t)
(setq linum-format "%5d: ")

;; 改行後にインデント
;; (global-set-key "\C-m" 'newline-and-indent)

;; タブキー
(setq default-tab-width 2)
;(setq indent-line-function 'indent-relative-maybe)

;; 行カーソル
;; thx http://rubikitch.com/2015/05/14/global-hl-line-mode-timer/
(require 'hl-line)
(defun global-hl-line-timer-function ()
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))
(setq global-hl-line-timer
      (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))
;; (cancel-timer global-hl-line-timer)

;(defface hlline-face
;  '((((class color)
;      (background dark))
;     ;;(:background "dark state gray"))
;     (:background "gray10"
;                  :underline "gray24"))
;    (((class color)
;      (background light))
;     (:background "ForestGreen"
;                  :underline nil))
;	(t ()))
;  "*Face used by hl-line.")
;;;(setq hl-line-face 'underline)
;(setq hl-line-face 'hlline-face)
;(global-hl-line-mode)


;;; elscreen
;(require 'elscreen)
;(setq elscreen-prefix-key (kbd "C-z"))
;(elscreen-start)
