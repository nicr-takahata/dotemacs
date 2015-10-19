;;; init.el --- init.el for jidaikobo
;; Copyright (C) 2015 by jidaikobo-shibata
;; Author: jidaikobo
;; URL: https://github.com/jidaikobo-shibata/dotemacs
;; thx 『Emacs実践入門』『Emacs LISP バイブル』『Emacs テクニック バイブル』

;;; ------------------------------------------------------------
;;; Commentary:
;;; usage: emacsのインストール（要X-code Command Line Tools）
;;; thx http://masutaka.net/chalow/2015-04-12-1.html
;;; ftp://ftp.math.s.chiba-u.ac.jp/emacsを確認して、あたらしいパッチの存在を確認すると良い
;; curl -LO http://ftp.gnu.org/pub/gnu/emacs/emacs-24.5.tar.xz
;; curl -LO ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-24.5-mac-5.11.tar.gz
;; tar xfJ emacs-24.5.tar.xz
;; tar xfz emacs-24.5-mac-5.11.tar.gz
;; cd emacs-24.5
;; patch -p 1 < ../emacs-24.5-mac-5.11/patch-mac
;; cp -r ../emacs-24.5-mac-5.11/mac mac
;; cp ../emacs-24.5-mac-5.11/src/* src
;; cp ../emacs-24.5-mac-5.11/lisp/term/mac-win.el lisp/term
;; \cp nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns mac/Emacs.app/Contents/Resources/Emacs.icns
;; ./configure --prefix=$HOME/opt/emacs-24.5 --with-mac --without-x
;; make
;; make GZIP_PROG='' install
;; cp -r mac/Emacs.app ~/Applications

;;; ------------------------------------------------------------
;;; usage: 利用前の準備
;;; このinit.elを~/.emacs.dに入れる前に、以下手順を踏んでおくこと。
;;; @ terminal
;; mkdir -p ~/.emacs.d/elisp
;; cd ~/.emacs.d/elisp
;; curl -0 http://www.emacswiki.org/emacs/download/auto-install.el
;; wget http://www.ne.jp/asahi/alpha/kazu/pub/emacs/phpdoc.el
;;; @ emacs
;;; S式はC-x C-eで反映すること
;;; 下記コマンド類はM-;でコメントを外してからやるとよい
;; M-x byte-compile-file RET ~/.emacs.d/elisp/auto-install.el RET
;; (add-to-list 'load-path "~/.emacs.d/elisp")
;; (require 'auto-install)
;; (setq auto-install-directory "~/.emacs.d/elisp")
;; (auto-install-update-emacswiki-package-name t)
;; (auto-install-compatibility-setup)
;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (package-initialize)
;; M-x install-elisp RET http://cx4a.org/pub/undohist.el
;; M-x auto-install-batch RET anything RET
;; M-x package-install RET auto-complete RET
;; M-x package-install RET undohist RET
;; M-x package-install RET undo-tree RET
;; M-x package-install RET recentf RET
;; M-x package-install RET cursor-chg RET
;; M-x package-install RET smart-tab
;; M-x package-install RET google-translate RET
;; M-x package-install RET web-mode RET
;; M-x package-install RET auto-async-byte-compile RET
;; M-x package-install RET point-undo RET
;; M-x package-install RET multiple-cursors RET
;; M-x package-install RET smartrep RET
;; M-x package-install RET flycheck RET
;; M-x package-install RET php-mode RET
;; M-x package-install RET mic-paren RET
;; M-x package-install RET tabbar RET
;; M-x install-elisp-from-emacswiki RET eldoc-extension.el RET

;;; メモ
;; tempbuf（idle buffer）を自動的にkill-bufferしてくれるelispだけど、
;; 結構不意に必要なbufferをkillしていることがあるので、使わない方向で。
;; M-x install-elisp-from-emacswiki RET tempbuf.el RET
;; multi-termもよさそうだけど、やっぱりterminalを使う。
;; M-x package-install RET multi-term RET

;;; Code:

;;; ------------------------------------------------------------
;;; package類のロード等

;;; load-pathの追加
(add-to-list 'load-path "~/.emacs.d/elisp")

;;; auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;;; ------------------------------------------------------------
;;; 全体設定

;;; リージョンを上書きできるようにする
(delete-selection-mode t)

;;; opt+¥でバックスラッシュを入力
(global-set-key (kbd "M-¥") "\\")

;;; s+RETでeval-buffer
(global-set-key [s-return] 'eval-buffer)

;;; ウィンドウ切り替え (opt+tab)
(global-set-key [M-tab] 'other-window)

;;; 自動分割は原則左右で
(setq split-height-threshold nil)

;;; inline patchを有効に
(setq default-input-method "MacOSX")

;;; CmdをMetaにしない
(setq mac-pass-command-to-system nil)
(setq mac-command-modifier 'super)

;;; optキーがMeta
(setq mac-option-modifier 'meta)

;; yes/no を y/n へ
(fset 'yes-or-no-p 'y-or-n-p)

;; スクロール時のカーソル位置の維持
(setq scroll-preserve-screen-position t)

;;; ドラッグアンドドロップでファイルを開く＋あたらしいウィンドウでひらかない
(global-set-key [ns-drag-file] 'ns-find-file)
(setq ns-pop-up-frames nil)

;;; 起動画面を抑止
(setq inhibit-startup-message t)

;;; スクラッチメッセージ（起動時注意書き）を抑止
(setq initial-scratch-message nil)

;;; オートインデント無効
(electric-indent-mode -1)

;;; 警告音とフラッシュを無効
(setq ring-bell-function 'ignore)

;;; バックアップファイルを作らないようにする
(setq make-backup-files nil)

;;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;;; 関数名の表示
(which-func-mode 1)

;;; ツールバーを非表示
;; M-x tool-bar-mode で表示非表示を切り替えられる
(tool-bar-mode -1)

;;; タイトルバーにファイル名表示
(setq frame-title-format (format "%%f %%* Emacs@%s" (system-name)))

;;; 何文字目にいるか表示
(column-number-mode 1)

;;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;;; タブキー
(setq default-tab-width 2)
(setq-default indent-tabs-mode t)

;;; ------------------------------------------------------------
;;; モード関連

;;; mode
(require 'php-mode)
(require 'web-mode)
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; flycheck
(load "flycheck")
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'php-mode-hook 'flycheck-mode)
(global-set-key [M-up] 'flycheck-previous-error) ; previous error (M+up)
(global-set-key [M-down] 'flycheck-next-error) ; next error (M+down)

;;; eldoc
(require 'eldoc-extension)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2)
(setq eldoc-minor-mode-string "")

;;; eldoc-php
;; thx http://www.ne.jp/asahi/alpha/kazu/php.html
(require 'phpdoc)
(add-hook 'php-mode-hook (lambda () (eldoc-mode t)))

;;; よくあるマイナーモードを非表示
;; thx http://qiita.com/tadsan/items/8b5976682b955788c262
(setq my/hidden-minor-modes
			'(undo-tree-mode
				eldoc-mode
				;; auto-complete-mode
				magit-auto-revert-mode
				smart-tab-mode
				flycheck-mode
				abbrev-mode
				helm-mode))

(mapc (lambda (mode)
				(setq minor-mode-alist
							(cons (list mode "") (assq-delete-all mode minor-mode-alist))))
			my/hidden-minor-modes)

;;; ------------------------------------------------------------
;;; undo関連

;;; undohist
;; ファイルを閉じてもundoの履歴を残す
(require 'undohist)
(undohist-initialize)

;;; undo-tree
;; redo (cmd+shft+z)
(require 'undo-tree)
(global-undo-tree-mode t)

;;; point-undo
;; カーソル位置履歴 (undo: M-s-left, redo: M-s-right)
(require 'point-undo)
(global-set-key [S-s-left] 'point-undo)
(global-set-key [S-s-right] 'point-redo)

;;; ------------------------------------------------------------
;;; Anything関連

;; Anything
(require 'anything)
(require 'anything-config)
(add-to-list 'anything-sources 'anything-c-source-emacs-commands)
(global-set-key (kbd "C-;") 'anything)

;;; recentf
;; 最近開いたファイルの履歴
(require 'recentf-ext)
(recentf-mode 1)
(setq recentf-exclude '("/TAGS$" "/var/tmp/" ".recentf" "/Fetch Temporary Folder/"))
(setq recentf-max-saved-items 3000)

;;; ------------------------------------------------------------
;;; キーボード操作

;;; mac likeなcmd関係
;; thx http://d.hatena.ne.jp/gan2/20080109/1199887209
;; thx http://www.unixuser.org/~euske/doc/emacsref/#file
(global-set-key (kbd "s-a") 'mark-whole-buffer) ; select all (cmd+a)
(global-set-key (kbd "s-c") 'kill-ring-save) ; copy (cmd+c)
(global-set-key (kbd "s-x") 'kill-region) ; cut (cmd+x)
(global-set-key (kbd "s-v") 'yank) ; paste (cmd+v)
(global-set-key (kbd "s-s") 'save-buffer) ; save (cmd+s)
(global-set-key (kbd "s-S") 'write-file) ; save as (cmd+shift+s)
(global-set-key (kbd "s-o") 'find-file) ; open (cmd+o)
(global-set-key (kbd "s-z") 'undo-tree-undo) ; undo (cmd+z)
(global-set-key (kbd "s-Z") 'undo-tree-redo) ; redo (cmd+shift+z)
(global-set-key (kbd "s-+") 'text-scale-increase) ; resize increase (cmd++)
(global-set-key [s-kp-add] 'text-scale-increase) ; resize increase (cmd++)
(global-set-key (kbd "s--") 'text-scale-decrease) ; resize decrease (cmd+-)
(global-set-key [s-kp-subtract] 'text-scale-decrease) ; resize decrease (cmd+-)
;; (global-set-key [s-kp-equal] (text-scale-mode 0))
;; (global-set-key [s-kp-equal] (text-scale-mode 0))
;; (global-set-key (kbd "s-=") (text-scale-mode 0))
;; (global-set-key [s-kp-0] (text-scale-mode 0))
;; (global-set-key (kbd "s-0") (text-scale-mode 0))
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs) ; quit (cmd+q)
(global-set-key [s-up] 'beginning-of-buffer) ; cmd+up
(global-set-key [s-down] 'end-of-buffer) ; cmd+down
(global-set-key [s-left] 'beginning-of-line) ; cmd+left
(global-set-key [s-right] 'end-of-line) ; cmd+right

;;; escでM-g
;; http://emacswiki.org/emacs/CancelingInEmacs
(setq normal-escape-enabled t)
(define-key isearch-mode-map [escape] 'isearch-abort) ; isearch
(define-key isearch-mode-map "\e" 'isearch-abort) ; \e seems to work better for terminals
(global-set-key [escape] 'keyboard-escape-quit) ; everywhere else

;;; M-g or cmd+opt+j で指定行へジャンプ
(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "M-s-j") 'goto-line)

;;; smart-tab
;; コンテキストに応じたtabキー。auto-completeと共存
(require 'smart-tab)
(global-smart-tab-mode)

;;; ------------------------------------------------------------
;;; 複数箇所選択と編集

;;; multiple-cursors and smartrep
(require 'multiple-cursors)
(require 'smartrep)

(declare-function smartrep-define-key "smartrep")

(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)

(global-unset-key "\C-t")

(smartrep-define-key global-map "C-t"
	'(("C-t"  . 'mc/mark-next-like-this)
		("n"    . 'mc/mark-next-like-this)
		("p"    . 'mc/mark-previous-like-this)
		("m"    . 'mc/mark-more-like-this-extended)
		("u"    . 'mc/unmark-next-like-this)
		("U"    . 'mc/unmark-previous-like-this)
		("s"    . 'mc/skip-to-next-like-this)
		("S"    . 'mc/skip-to-previous-like-this)
		("*"    . 'mc/mark-all-like-this)
		("d"    . 'mc/mark-all-like-this-dwim)
		("i"    . 'mc/insert-numbers)
		("o"    . 'mc/sort-regions)
		("O"    . 'mc/reverse-regions)))

;;; ------------------------------------------------------------
;;; バッファ関連

;;; create-temporary-buffer
;; あたらしい空のバッファを作る (cmd+t)
(defun create-temporary-buffer ()
	"Create temporal buffer."
	(interactive)
	(switch-to-buffer (generate-new-buffer "new"))
	(global-auto-complete-mode t))
(define-key global-map (kbd "s-t") 'create-temporary-buffer) ; (cmd+t)

;;; ------------------------------------------------------------
;;; ウィンドウ関連

;;; mac like close window (cmd+w)
;; cmd+wで、開いているウィンドウを閉じる。単一のバッファなら、変更を確認してバッファを閉じる
(defun contexual-close-window ()
	"Mac like close window (cmd+w)."
	(interactive)
	(let ($save)
		;; アスタリスクで始まるバッファは何も尋ねず閉じる
		(if (or (string= "*" (substring (buffer-name) 0 1)) buffer-read-only)
				(kill-buffer)
			;; バッファがウインドウ分割をしている時は、単にdelete-windowする
			(if (one-window-p)
					;; バッファがウインドウ分割をしていない時には、変更の有無を確認
					(if (buffer-modified-p)
							;; 変更があるので振る舞いを尋ねる
							(progn
								(setq $save (read-string "overwrite? (1:overrite, 2:save as, 3:close anyway): " nil 'my-history))
								(cond
								 ((string-equal $save "1")
									(save-buffer))
								 ((string-equal $save "2")
									(progn (call-interactively 'write-file)
												 (save-buffer))))
								(kill-buffer))
						;; 変更がないのでkill-buffer
						(kill-buffer)))
			;; ウィンドウ分割されていないので、delete-window
			(delete-window))))
(global-set-key (kbd "s-w") 'contexual-close-window)

;;; mac like new window (cmd+n)
;; cmd+n でウィンドウを増やす。分割方法は対話式
(defun create-new-window-intaractive (act)
	"Mac like new window (cmd+n).  ACT is interactive."
	(interactive "nchoose (holizntal:1, vertical:2):")
	(cond ((eq act 2) (split-window-horizontally))
				(t (split-window-vertically))))
(global-set-key (kbd "s-n") 'create-new-window-intaractive)

;;; ------------------------------------------------------------
;;; フレーム関連

;;; フレームの大きさと色を変更 (cmd+shift+w)
(defun resize-selected-frame ()
	"Resize frame to jidaikobo's default."
	(interactive)
	(set-frame-position  (selected-frame) 15 0)
	(set-frame-size (selected-frame) 215 55))
(global-set-key (kbd "s-W") 'resize-selected-frame)

;;; フレーム初期値
(add-to-list 'default-frame-alist '(alpha . (1.00 1.00)))
(add-to-list 'default-frame-alist '(width . 215))
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 15))
(add-to-list 'default-frame-alist '(font . "ricty-16"))
(add-to-list 'default-frame-alist '(background-color . "#201f1f"))
(add-to-list 'default-frame-alist '(foreground-color . "white"))
(add-to-list 'default-frame-alist '(cursor-color . "gray"))

;;; 新規フレーム作成 (cmd+shift+n)
(defun create-new-frame ()
	"Create new frame."
	(interactive)
	(switch-to-buffer-other-frame "*new1*")
	;; あたらしいフレームでも行番号表示を維持したいが、うまくいかない？
	(show-line-number))
(global-set-key (kbd "s-N") 'create-new-frame)
(add-hook 'after-make-frame-functions 'show-line-number)

;;; ------------------------------------------------------------
;;; タブ関連

;;; tabbar
(require 'tabbar)
(tabbar-mode)

;;; タブ上でマウスホイールを使わない
(tabbar-mwheel-mode nil)

;;; グループを使わない
(setq tabbar-buffer-groups-function nil)

;; 画像を使わないことで軽量化する
(setq tabbar-use-images nil)

;;; 左側のボタンを消す
(dolist (btn '(tabbar-buffer-home-button
							 tabbar-scroll-left-button
							 tabbar-scroll-right-button))
	(set btn (cons (cons " " nil)
								 (cons " " nil))))

;;; 除外するタブ
;; http://dev.ariel-networks.com/wp/documents/aritcles/emacs/part11
(defun my-tabbar-buffer-list ()
	"Remove asterisked buffer tag."
	(remove-if
	 (lambda (buffer)
		 (find (aref (buffer-name buffer) 0) " *"))
	 (buffer-list)))
(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

;;; 色設定
(set-face-attribute ; バー自体の色
 'tabbar-default nil
 :background "Gray"
 :height 1.0)
(set-face-attribute ; アクティブなタブ
 'tabbar-selected nil
 :background "Black"
 :foreground "White"
 :weight 'bold
 :box nil)
(set-face-attribute ; 非アクティブなタブ
 'tabbar-unselected nil
 :background "Gray"
 :foreground "Black"
 :box nil)

(global-set-key [M-s-right] 'tabbar-forward)
(global-set-key [M-s-left] 'tabbar-backward)

;;; ------------------------------------------------------------
;;; カーソル関連

;;; cursor-chg
;; カーソルの形状を変更
(require 'cursor-chg)
(change-cursor-mode 1)
(toggle-cursor-type-when-idle 0)
(setq curchg-default-cursor-color "White")
;; 残念ながら日本語キーボードで「英数」「かな」キーを使っている限りは、これは効かない。
(setq curchg-input-method-cursor-color "Orange")

;;; C-aで、開始場所と先頭をトグル
;; thx http://qiita.com/ShingoFukuyama/items/62269c4904ca085f9149
(defun my-goto-line-beginning-or-indent (&optional position)
	"Goto line beginning or indent.  POSITION is optical."
	(interactive)
	(or position (setq position (point)))
	(let (($starting-position (progn (back-to-indentation) (point))))
		(if (eq $starting-position position)
				(move-beginning-of-line 1))))
(global-set-key (kbd "C-a") 'my-goto-line-beginning-or-indent)

;;; ------------------------------------------------------------
;;; 行設定

;;; 行カーソル
;; thx http://rubikitch.com/tag/emacs-post-command-hook-timer/
;; (setq hl-line-face 'underline)
;; (global-hl-line-mode)
;; 下線だと、日本語入力時の候補領域がわかりづらいのでやめる。
(require 'hl-line)
(defun global-hl-line-timer-function ()
	"Line cursor."
	(global-hl-line-unhighlight-all)
	(let ((global-hl-line-mode t))
		(set-face-attribute 'hl-line nil :foreground nil :background "#3e3e3e" :underline nil)
		(global-hl-line-highlight)))
(setq global-hl-line-timer
			(run-with-idle-timer 0.03 t 'global-hl-line-timer-function))
;; (cancel-timer global-hl-line-timer)

;; 行間隔を少し広げる
(set-default 'line-spacing 3)

;;; 行番号を表示する
;; 表示切替はM-x wb-line-number-toggleと入力。
(defun show-line-number ()
	"Show line number."
	(interactive)
	(require 'linum)
	(setq linum-delay t)
	(defadvice linum-schedule (around my-linum-schedule () activate)
		(run-with-idle-timer 0.2 nil #'linum-update-current))	(global-linum-mode t)
		(setq linum-format "%5d: "))
(show-line-number)

;;; ------------------------------------------------------------
;;; マウス設定

;;; 右ボタンの割り当て(押しながらの操作)をはずす。
;;; thx http://cave.under.jp/_contents/emacs.html#60
(if window-system (progn
										(global-unset-key [down-mouse-3])
										;; マウスの右クリックメニューを出す(押して、離したときにだけメニューが出る)
										(defun bingalls-edit-menu (event)
											(interactive "e")
											(popup-menu menu-bar-edit-menu))
										(global-set-key [mouse-3] 'bingalls-edit-menu)))

;;; ------------------------------------------------------------
;;; 自動バイトコンパイル

;; auto-async-byte-compile
;; thx http://www.emacswiki.org/emacs/auto-async-byte-compile.el
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "init.el")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;; ------------------------------------------------------------
;;; 釣り合いのとれる括弧のハイライト
(require 'mic-paren)
(paren-activate)
(setq paren-face '(underline paren-match-face))
(setq paren-match-face '(underline paren-face))
(setq paren-sexp-mode t)

;;; 釣り合う行カッコが画面外だったらミニバッファに表示
;; thx http://emacswiki.org/emacs/ShowParenMode
(defadvice show-paren-function
		(after show-matching-paren-offscreen activate)
	"If the matching paren is offscreen, show the matching line in theecho area.  Has no effect if the character before point is not ofthe syntax class ')'."
	(interactive)
	(let* ((cb (char-before (point)))
				 (matching-text (and cb
														 (char-equal (char-syntax cb) ?\) )
														 (blink-matching-open))))
		(when matching-text (message matching-text))))

;;; ------------------------------------------------------------
;;; ファイラ (dired)
;; diredでファイル編集
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;; ------------------------------------------------------------
;;; whitespace関連設定

;;; タブの可視化
;;; thx http://qiita.com/itiut@github/items/4d74da2412a29ef59c3a
(require 'whitespace)
(setq whitespace-style '(face          ; faceで可視化
												trailing       ; 行末
												tabs           ; タブ
												spaces         ; スペース
												empty          ; 先頭/末尾の空行
												space-mark     ; 表示のマッピング
												tab-mark
												))

(setq whitespace-display-mappings
			'((space-mark ?\u3000 [?\u25a1])
				;; WARNING: the mapping below has a problem.
				;; When a TAB occupies exactly one column, it will display the
				;; character ?\xBB at that column followed by a TAB which goes to
				;; the next TAB column.
				;; If this is a problem for you, please, comment the line below.
				(tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

;;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))

(global-whitespace-mode 1)

(set-face-attribute 'whitespace-trailing nil
										:background "#201f1f"
										:underline t)
(set-face-attribute 'whitespace-tab nil
										:background "#201f1f"
										:foreground "Gray30"
										:underline nil)
(set-face-attribute 'whitespace-space nil
										:background "#201f1f"
										:underline nil)
(set-face-attribute 'whitespace-empty nil
										:background "#201f1f"
										:underline nil)

;;; ------------------------------------------------------------
;;; 選択範囲の言語を確認して翻訳 (C-c t)
;;; google-translate
;;; http://rubikitch.com/2014/12/07/google-translate/
(require 'google-translate)
(defvar google-translate-english-chars "[:ascii:]"
	"Ascii means English.")
(defun google-translate-enja-or-jaen (&optional string)
	"Google translate enja or jaen.  STRING in region."
	(interactive)
	(setq string
				(cond ((stringp string) string)
							(current-prefix-arg
							 (read-string "Google Translate: "))
							((use-region-p)
							 (buffer-substring (region-beginning) (region-end)))
							(t
							 (save-excursion
								 (let (s)
									 (forward-char 1)
									 (backward-sentence)
									 (setq s (point))
									 (forward-sentence)
									 (buffer-substring s (point)))))))
	(let* ((asciip (string-match
									(format "\\`[%s]+\\'" google-translate-english-chars)
									string)))
		(run-at-time 0.1 nil 'deactivate-mark)
		(google-translate-translate
		 (if asciip "en" "ja")
		 (if asciip "ja" "en")
		 string)))
(global-set-key (kbd "C-c t") 'google-translate-enja-or-jaen)

;;; ------------------------------------------------------------
;;; auto-complete
;; オートコンプリート
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq ac-dwim t)
(setq ac-disable-faces nil)

;;; ユーザ辞書ディレクトリ
(defvar ac-user-dict-dir (expand-file-name "~/.emacs.d/ac-dict/"))

;;; 辞書追加
;; 英語
(defvar ac-english-cache
	(ac-file-dictionary (concat ac-user-dict-dir "english")))
(defvar ac-english-dict
	'((candidates . ac-english-cache)))

;; 技術語
(defvar ac-technical-term-cache
	(ac-file-dictionary (concat ac-user-dict-dir "technical-term")))
(defvar ac-technical-term-dict
	'((candidates . ac-technical-term-cache)))

;; FuelPHP
(defvar ac-fuelphp-cache
	(ac-file-dictionary (concat ac-user-dict-dir "fuelphp")))
(defvar ac-fuelphp-dict
	'((candidates . ac-fuelphp-cache)))

;;; 条件の追加
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(setq-default ac-sources '(ac-source-words-in-same-mode-buffers
													 ac-source-symbols
													 ac-source-filename
													 ac-english-dict
													 ac-technical-term-dict))

;;; ------------------------------------------------------------
;;; 行／選択範囲の複製 (cmd+d)
;; duplicate-region-or-line
(defun duplicate-region-or-line ()
	"Duplicate region or line."
	(interactive)
	(let (selected
				(is-line nil))
		(if (not (region-active-p))
				(progn
					(setq is-line t)
					(beginning-of-line)
					(set-mark-command nil)
					(end-of-line)
					(setq deactivate-mark nil)))
		(setq selected (buffer-substring-no-properties (region-beginning) (region-end)))
		(if is-line (progn
									(insert "\n" selected)
									(beginning-of-line))
			(insert selected))))
(define-key global-map (kbd "s-d") 'duplicate-region-or-line)

;;; ------------------------------------------------------------
;;; 編集可能な検索置換仕組み
;;; editable-search
(custom-set-variables
 '(es-is-use-super t)
 '(es-is-deactivate-region-by-cursor t))
(require 'editable-search)

;;; ------------------------------------------------------------
;;; HTMLのマークアップのキーバインド集
;;; jidaikobo web authoring set
(load "jidaikobo-web-authoring-set")

;;; ------------------------------------------------------------
;;; Todo:
;; 検索置換において、情報エリアを作って、ターゲットウィンドウと正規表現モードかどうかを表示する
;; モードラインを表示しないウィンドウ
;; 正規表現のとき、検索置換ウィンドウの色を変える（難しい）
;; 本体ウィンドウクローズ時に、検索置換もクローズ
;; 検索時に出る（ことがある）エラーの調査
;; 直観的に編集ウィンドウに戻るキーバインド
;; マルチファイル検索置換
;; js2-mode
;; HTMLのflycheck
;; doctypeを見てのbrやタグの挿入
;; editable-searchのemacs likeなデフォルトのキーバインド

;;; ------------------------------------------------------------
;;; experimental area
;; (thing-at-point)

;;; init.el ends here
