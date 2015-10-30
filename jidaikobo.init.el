;;; jidaikobo.init.el --- jidaikobo.init.el for jidaikobo
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
;; sudo port install global
;;
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
;; M-x package-install RET descbinds-anything RET
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
;; M-x install-elisp-from-emacswiki RET eldoc-extension.el RET
;; M-x package-install RET gtags RET
;; M-x package-install RET open-junk-file RET
;; M-x package-install RET bind-key RET

;;; Memo:
;; magit, js2-mode, elscreenは、M-x install-packageで、入らなかった。
;; M-x list-packagesで、C-sで探し、ページ移動の後installでなら入る。

;;; Memo2:
;; tempbuf（idle buffer）を自動的にkill-bufferしてくれるelispだけど、
;; 結構不意に必要なbufferをkillしていることがあるので、使わない方向で。
;; multi-termもよさそうだけど、やっぱりterminalを使う。

;;; Code:
;;; ------------------------------------------------------------
;;; package類のロード等

(defvar my-packages-initialize t "*Default t'.")

(when my-packages-initialize
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
	(package-initialize))

;;; init.elで使うパッケージ
(require 'bind-key)

;;; ------------------------------------------------------------
;;; 全体設定

;;; リージョンを上書きできるようにする
(delete-selection-mode t)

;;; opt+¥でバックスラッシュを入力
(bind-key* "M-¥" "\\")

;;; s+RETでeval-bufferかeval-region
(bind-key* "<s-return>" (lambda () (interactive)
														 (if (region-active-p)
																 (eval-region (region-beginning) (region-end))
															 (eval-buffer))
														 (message "eval done.")))

;;; ウィンドウ切り替え (opt+tab)
(bind-key* "<M-tab>" 'other-window)

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
(bind-key* "<ns-drag-file>" 'ns-find-file)
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

;;; ツールバーを非表示
;; M-x tool-bar-mode で表示非表示を切り替えられる
(tool-bar-mode -1)

;;; タイトルバーにファイル名表示
(setq frame-title-format (format "%%f %%* Emacs@%s" (system-name)))

;;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;;; タブキー
(setq default-tab-width 2)
(setq-default indent-tabs-mode t)

;;; cua-modeの設定
(cua-mode t) ; cua-modeをオン
(setq cua-enable-cua-keys nil) ; CUAキーバインドを無効にする

;;; ------------------------------------------------------------
;;; モード関連

;;; mode
(require 'php-mode)
(require 'web-mode)

(require 'js2-mode)
(add-hook 'js2-mode-hook '(flycheck-mode t))
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (define-key js2-mode-map (kbd "M-up") 'previous-error))
;; (define-key js2-mode-map (kbd "M-down") 'next-error)

;;; flycheck
(load "flycheck")
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'php-mode-hook 'flycheck-mode)
(bind-key* "<M-up>" 'flycheck-previous-error) ; previous error (M+up)
(bind-key* "<M-down>" 'flycheck-next-error) ; next error (M+down)

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

;;; php-modeのタブ幅
(add-hook 'php-mode-hook
					'(lambda()
						 (setq tab-width 2)
						 (setq indent-tabs-mode t)
						 (setq c-basic-offset 2)))

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
(bind-key* "<S-s-left>" 'point-undo)
(bind-key* "<S-s-right>" 'point-redo)

;;; ------------------------------------------------------------
;;; Anything関連

;;; Anything
(require 'anything)
(require 'anything-config)
(require 'anything-gtags)
(require 'anything-grep)
(bind-key* "C-;" 'my-anything)

;;; あればgtagsを起点にしてfindし、なければカレントディレクトリを対象にした情報源
(defvar anything-c-source-find-by-gtags
	'((name . "Find by gtags or ls")
		(candidates . (lambda ()
										(let
												((default-directory
													 (with-current-buffer anything-current-buffer default-directory)))
											(cond
											 ;; gtags-get-rootpathが返ったらgtagsをあてにして良い
											 ((gtags-get-rootpath)
												(split-string
												 (shell-command-to-string
													(concat "find " (directory-file-name (gtags-get-rootpath)) " -type f")) "\n"))
											 ;; findの負荷が高すぎる場所だったらやりすごす
											 ((member default-directory '("/" "~/"))
												(split-string
												 (shell-command-to-string
													(concat "ls " default-directory)) "\n"))
											 ;; とりあえず自分以下のファイルをfind
											 (t
												(split-string (shell-command-to-string (concat "find " (directory-file-name default-directory) " -type f")) "\n"))))))
		(type . file)
		(requires-pattern . 3)
		(delayed)))

;;; よく使うプロジェクトに対する操作
(defvar anything-c-source-cd-to-projects
	'((name . "cd to projects")
		(candidates . (lambda () (split-string (shell-command-to-string "find ~/Sites -maxdepth 1 -type d") "\n")))
		(action . (("Change internal directory" . anything-change-internal-directory)
							 ("Dired" . anything-project-dired)
							 ("Generate gtags at project" . anything-generate-gtags-at-project)))))

(defun anything-change-internal-directory (dir)
	"Change internal directory at Sites.  DIR is path."
	(cd dir))

(defun anything-project-dired (dir)
	"Dired.  DIR is path."
	(dired dir))

(defun anything-generate-gtags-at-project (dir)
	"Generate gtags at project.  DIR is path."
	(shell-command-to-string (concat "cd " dir " ; gtags -v")))

;;; my-anything
(defun my-anything ()
	"Anything command included find by gtags."
	(interactive)
	(anything-other-buffer
	 '((anything-c-source-emacs-commands
			anything-c-source-gtags-select
			anything-c-source-find-by-gtags)
		 anything-c-source-buffers-list
		 anything-c-source-recentf
		 anything-c-source-files-in-current-dir+
		 anything-c-source-cd-to-projects
		 anything-c-source-find-by-gtags)
	 "*my-anything*"))

;;; anything-imenu
(bind-key* "C-," 'anything-imenu)

;;; descbinds-anythingの乗っ取り
;; thx http://d.hatena.ne.jp/buzztaiki/20081115/1226760184
(require 'descbinds-anything)
(descbinds-anything-install)

;; 編集対象でないバッファを除外(必要な場合、switch-to-buffer)
;; thx https://github.com/skkzsh/.emacs.d/blob/master/conf/anything-init.el
(setq anything-c-boring-buffer-regexp
			(rx "*" (+ not-newline) "*"))

;;; recentf
;; 最近開いたファイルの履歴
(require 'recentf-ext)
(recentf-mode 1)
(setq recentf-exclude '("/TAGS$" "/var/tmp/" ".recentf" "/Fetch Temporary Folder/*"))
(setq recentf-max-saved-items 3000)

;;; ------------------------------------------------------------
;;; gtags
(require 'gtags)
(setq gtags-path-style 'relative)

;;; キーバインド
(setq gtags-mode-hook
			'(lambda ()
				 (local-set-key "\M-t" 'gtags-find-tag)
				 (local-set-key "\M-r" 'gtags-find-rtag)
				 (local-set-key "\M-s" 'gtags-find-symbol)
				 (local-set-key "\M-T" 'gtags-pop-stack)))

;;; gtags-mode を使いたい mode の hook に追加する
(add-hook 'php-mode-hook
					'(lambda()
						 (gtags-mode 1)
						 (gtags-make-complete-list)))

;;; update GTAGS
;; thx http://qiita.com/yewton/items/d9e686d2f2a092321e34
(defun update-gtags (&optional prefix)
	"Update gtags.  PREFIX."
	(interactive "P")
	(let ((rootdir (gtags-get-rootpath))
				(args (if prefix "-v" "-iv")))
		(when rootdir
			(let* ((default-directory rootdir)
						 (buffer (get-buffer-create "*update GTAGS*")))
				(save-excursion
					(set-buffer buffer)
					(erase-buffer)
					(let ((result (process-file "gtags" nil buffer nil args)))
						(if (= 0 result)
								(message "GTAGS successfully updated.")
							(message "update GTAGS error with exit status %d" result))))))))
(add-hook 'after-save-hook 'update-gtags)

;;; ------------------------------------------------------------
;;; キーボード操作

;;; mac likeなcmd関係
;; thx http://d.hatena.ne.jp/gan2/20080109/1199887209
;; thx http://www.unixuser.org/~euske/doc/emacsref/#file
(bind-key* "s-a" 'mark-whole-buffer) ; select all (cmd+a)
(bind-key* "s-c" 'kill-ring-save) ; copy (cmd+c)
(bind-key* "s-x" 'kill-region) ; cut (cmd+x)
(bind-key* "s-v" 'yank) ; paste (cmd+v)
(bind-key* "s-s" 'save-buffer) ; save (cmd+s)
(bind-key* "s-S" 'write-file) ; save as (cmd+shift+s)
(bind-key* "s-o" 'find-file) ; open (cmd+o)
(bind-key* "s-z" 'undo-tree-undo) ; undo (cmd+z)
(bind-key* "s-Z" 'undo-tree-redo) ; redo (cmd+shift+z)
(bind-key* "s-+" 'text-scale-increase) ; resize increase (cmd++)
(bind-key* "<s-kp-add>" 'text-scale-increase) ; resize increase (cmd++)
(bind-key* "s--" 'text-scale-decrease) ; resize decrease (cmd+-)
(bind-key* "<s-kp-subtract>" 'text-scale-decrease) ; resize decrease (cmd+-)
;; (bind-key* "s-kp-equal" (text-scale-mode 0))
;; (bind-key* "s-kp-equal" (text-scale-mode 0))
;; (bind-key* "s-=" (text-scale-mode 0))
;; (bind-key* "s-kp-0" (text-scale-mode 0))
;; (bind-key* "s-0" (text-scale-mode 0))
(bind-key* "s-q" 'save-buffers-kill-emacs) ; quit (cmd+q)
(bind-key* "<s-up>" 'beginning-of-buffer) ; cmd+up
(bind-key* "<s-down>" 'end-of-buffer) ; cmd+down
(bind-key* "<s-left>" 'beginning-of-line) ; cmd+left
(bind-key* "<s-right>" 'end-of-line) ; cmd+right

;; (bind-key* "M-right" 'forward-symbol)
;; (bind-key* "M-left" (lambda () (interactive) (forward-symbol -1)))

;;; escでM-g
;; http://emacswiki.org/emacs/CancelingInEmacs
(setq normal-escape-enabled t)
(define-key isearch-mode-map [escape] 'isearch-abort) ; isearch
(define-key isearch-mode-map "\e" 'isearch-abort) ; \e seems to work better for terminals
(bind-key* "<escape>" 'keyboard-escape-quit) ; everywhere else
(define-key minibuffer-inactive-mode-map [escape] 'keyboard-quit) ; minibuffer

;;; control+shift+cursorでウィンドウ内バッファ履歴
(bind-key* "<C-S-left>" 'switch-to-prev-buffer)
(bind-key* "<C-S-right>" 'switch-to-next-buffer)

;;; ミニバッファでcmd+dでDesktopへ
(define-key minibuffer-local-map (kbd "s-d")
	(lambda () (interactive)
		(beginning-of-line)
		(delete-region (point) (line-end-position))
		(insert "~/Desktop/")))

;;; M-g or cmd+opt+j で指定行へジャンプ
(bind-key* "M-g" 'goto-line)
(bind-key* "M-s-j" 'goto-line)

;;; smart-tab
;; コンテキストに応じたtabキー。auto-completeと共存
(require 'smart-tab)
(global-smart-tab-mode)

;;; ------------------------------------------------------------
;;; よく使うところに早く移動

(setq next-block-previous-direction nil)
(defun next-block (direction)
	"Go to next block by mode.  DIRECTION[prev|next]."
	(interactive)
	(when (not (string= next-block-previous-direction direction))
			(if (string= direction "prev") (beginning-of-line) (end-of-line)))
	(setq next-block-previous-direction direction)
	(let
			(target)
		(cond
		 ((string= major-mode "emacs-lisp-mode")
			(setq target "^;;; ----+$"))
		 ((string= major-mode "php-mode")
			(setq target "^\t*function\\|^\t*class\\|^\t*private\\|^\t*public"))
		 ((string= major-mode "web-mode")
			(setq target "^\t*<h2"))
		 (t
			(setq target "^;;; ----+$\\|^■")))
		(if (string= direction "prev")
				(re-search-backward target)
			(re-search-forward target))))
(bind-key* "<M-s-down>" (lambda () (interactive) (next-block "next")))
(bind-key* "<M-s-up>" (lambda () (interactive) (next-block "prev")))

;;; ------------------------------------------------------------
;;; 複数箇所選択と編集

;;; multiple-cursors and smartrep
(require 'multiple-cursors)
(require 'smartrep)

(declare-function smartrep-define-key "smartrep")

(bind-key* "C-M-c" 'mc/edit-lines)
(bind-key* "C-M-r" 'mc/mark-all-in-region)

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

;;; f2キーでmessageと今のバッファをトグル
(bind-key* "<f2>" (lambda () (interactive)
											 (let (current-buffer-for-return)
												 (if (eq (selected-window) (get-buffer-window "*Messages*"))
														 (switch-to-buffer current-buffer-for-return)
													 (setq current-buffer-for-return (current-buffer))
													 (switch-to-buffer "*Messages*")))))

;;; ------------------------------------------------------------
;;; ウィンドウ関連

;;; mac like new window (cmd+n)
;; cmd+n でウィンドウを増やす。分割方法は対話式
(defun create-new-window-intaractive (act)
	"Mac like new window (cmd+n).  ACT is interactive."
	(interactive "nchoose (holizntal:1, vertical:2):")
	(cond ((eq act 2) (split-window-horizontally))
				(t (split-window-vertically))))
(bind-key* "s-n" 'create-new-window-intaractive)

;;; ------------------------------------------------------------
;;; フレーム関連

;;; フレームの大きさと色を変更 (cmd+shift+w)
(defun resize-selected-frame ()
	"Resize frame to jidaikobo's default."
	(interactive)
	(set-frame-position (selected-frame) 0 0)
	(set-frame-size (selected-frame) 215 55))
(bind-key* "s-W" 'resize-selected-frame)

;;; フレーム初期値
(add-to-list 'default-frame-alist '(alpha . (1.00 1.00)))
(add-to-list 'default-frame-alist '(width . 215))
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . 0))
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
(bind-key* "s-N" 'create-new-frame)
(add-hook 'after-make-frame-functions 'show-line-number)

;;; ------------------------------------------------------------
;;; タブ関連 - elscreen

(require 'elscreen)
(elscreen-start)

;;; タブの先頭に[X]を表示しない
(setq elscreen-tab-display-kill-screen nil)

;;; header-lineの先頭に[<->]を表示しない
(setq elscreen-tab-display-control nil)

;;; キーバインド
(bind-key* "<M-s-right>" 'elscreen-next)
(bind-key* "<M-s-left>" 'elscreen-previous)

;;; 新しいスクリーン
(bind-key* "s-t" (lambda () (interactive)
									 (elscreen-create)
									 (switch-to-buffer (generate-new-buffer "new"))))

;;; ウィンドウやスクリーンを閉じる
(bind-key* "s-w" 'my-delete-windows)
(defun my-delete-windows ()
	"Contexual delete windows."
	(interactive)
	(cond
	 ;; ウィンドウ構成が多ければまず他のウィンドウを消す
	 ((not (one-window-p)) (delete-other-windows))
	 ;; ウィンドウ構成がひとつでバッファに変更があれば破棄を確認する
	 ((and (buffer-modified-p)
				 ;; read-onlyなら無視
				 buffer-read-only
				 ;; アスタリスクで始まるバッファ名も保存を尋ねない
				 (not (string=
							 (substring (buffer-name (current-buffer)) 0 1)
							 "*")))
		(unless (yes-or-no-p "Buffer is modified. Close anyway?")
			(call-interactively (save-buffer)))
		(kill-buffer)
		(unless (elscreen-one-screen-p) (elscreen-kill)))
	 ;; screenがひとつだったらkill-buffer
	 ((elscreen-one-screen-p) (kill-buffer))
	 ;; ここまで来る条件てあるのかしら。とりあえずkill
	 (t (elscreen-kill-screen-and-buffers))))

;;; ------------------------------------------------------------
;;; open-junk-file

(require 'open-junk-file)
(setq open-junk-file-format "~/Desktop/%Y-%m-%d-%H%M%S.txt")

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
(bind-key* "C-a" 'my-goto-line-beginning-or-indent)

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
		(run-with-idle-timer 0.2 nil #'linum-update-current))
	(global-linum-mode t)
	(setq linum-format "%5d: "))
(show-line-number)

;;; ------------------------------------------------------------
;;; モードライン設定

;;; 関数名の表示
(which-func-mode 1)

;;; 何文字目にいるか表示
(column-number-mode 1)

;; モードラインにカレントディレクトリを表示する
(let ((ls (member 'mode-line-buffer-identification mode-line-format)))
	(setcdr ls
					(cons
					 '(:eval (concat " (" (abbreviate-file-name default-directory) ")"))
					 (cdr ls))))

;;; よくあるマイナーモードを非表示
;; thx http://qiita.com/tadsan/items/8b5976682b955788c262
;; これは一通り処理が終わった後呼ぶ必要がある。
(setq my/hidden-minor-modes
			'(undo-tree-mode
				eldoc-mode
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
;;; マウス設定

;;; 右ボタンの割り当て(押しながらの操作)をはずす。
;;; thx http://cave.under.jp/_contents/emacs.html#60
(if window-system
		(progn
			(global-unset-key [down-mouse-3])
			;; マウスの右クリックメニューを出す(押して、離したときにだけメニューが出る)
			(defun bingalls-edit-menu (event)
				(interactive "e")
				(popup-menu menu-bar-edit-menu))
			(bind-key* "<mouse-3>" 'bingalls-edit-menu)))

;;; shift+clickでregion作成
;; thx http://superuser.com/questions/521223/shift-click-to-extend-marked-region
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)

;;; ------------------------------------------------------------
;;; 自動バイトコンパイル

;; auto-async-byte-compile
;; thx http://www.emacswiki.org/emacs/auto-async-byte-compile.el
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "init.el")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;; ------------------------------------------------------------
;;; git



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

;;; 現在位置を開く
(ffap-bindings)

;;; diredでマークをつけたファイルを開く（F）
(eval-after-load "dired"
	'(progn
		 (define-key dired-mode-map (kbd "F") 'my-dired-find-marked-files)
		 (defun my-dired-find-marked-files (&optional arg)
			 "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
			 (interactive "P")
			 (let* ((fn-list (dired-get-marked-files nil arg)))
				 (mapc 'find-file fn-list)))))

;;; diredでマークをつけたファイルをviewモードで開く（V）
(eval-after-load "dired"
	'(progn
		 (define-key dired-mode-map (kbd "V") 'my-dired-view-marked-files)
		 (defun my-dired-view-marked-files (&optional arg)
			 "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
			 (interactive "P")
			 (let* ((fn-list (dired-get-marked-files nil arg)))
				 (mapc 'view-file fn-list)))))

;;; diredでタブを開きすぎないようにする
;; http://nishikawasasaki.hatenablog.com/entry/20120222/1329932699
;; dired-find-alternate-file の有効化
(put 'dired-find-alternate-file 'disabled nil)

;; RET 標準の dired-find-file では dired バッファが複数作られるのでdired-find-alternate-file を代わりに使う
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "a") 'dired-find-file)

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
			'((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
				;;(space-mark ?\u3000 [?\u25a1])
				))

;;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))

;;; whitespaceの見栄え
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
(bind-key* "C-c t" 'google-translate-enja-or-jaen)

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
(bind-key* "s-d" 'duplicate-region-or-line)

;;; ------------------------------------------------------------
;;; 全角数字を半角数字に
(defun convert-to-single-byte-number ()
	"Convert multi-byte numbers in region into single-byte number."
	(interactive)
	(replace-strings-in-region-by-list
	 '(("１" . "1")
		 ("２" . "2")
		 ("３" . "3")
		 ("４" . "4")
		 ("５" . "5")
		 ("６" . "6")
		 ("７" . "7")
		 ("８" . "8")
		 ("９" . "9")
		 ("０" . "0"))))
(bind-key* "s-u" 'convert-to-single-byte-number)

;;; ------------------------------------------------------------
;;; 選択範囲を1行にする
(defun join-multi-lines-to-one ()
	"Join multi lines."
	(interactive)
	(require 'editable-search)
	(es-replace-region "\n\\|^>+ \\|\t" "" t))
(bind-key* "<s-kp-divide>" 'join-multi-lines-to-one) ; cmd+/
(bind-key* "s-/" 'join-multi-lines-to-one) ; cmd+/

;;; ------------------------------------------------------------
;;; メール整形
(defun add-mail-quotation ()
	"Add mail quotation."
	(interactive)
	(require 'editable-search)
	(mail-mode)
	(if (string-match "^>" (buffer-substring-no-properties (region-beginning) (region-end)))
			(es-replace-region "^" ">" t)
		(es-replace-region "^" "> " t)))
(defun remove-mail-quotation ()
	"Add mail quotation."
	(interactive)
	(require 'editable-search)
	(mail-mode)
	(es-replace-region "^>+ +" "" t))
(bind-key* "s-}" 'add-mail-quotation)
(bind-key* "s-{" 'remove-mail-quotation)

;; ucs-normalize-NFC-region で濁点分離を直す
;; M-x ucs-normalize-NFC-buffer または "C-x RET u" で、
;; バッファ全体の濁点分離を直します。
;; http://d.hatena.ne.jp/nakamura001/20120529/1338305696
;; http://www.sakito.com/2010/05/mac-os-x-normalization.html
(require 'ucs-normalize)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)
(defun ucs-normalize-NFC-buffer ()
  (interactive)
  (ucs-normalize-NFC-region (point-min) (point-max))
  )
(bind-key* "<C-x return u>" 'ucs-normalize-NFC-buffer)

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
(require 'jidaikobo-web-authoring-set)

;;; ------------------------------------------------------------
;;; Todo:
;; 様子を見てctagsの導入
;; モードラインを表示しないウィンドウ
;; doctypeを見てのbrやタグの挿入
;; 単語境界をもうちょっと細かく

;;; ------------------------------------------------------------
;;; experimental area
;; (thing-at-point)

;;; ------------------------------------------------------------
;; 単語境界を細かく
;; マウスのダブルクリックでシンボル単位の選択（アンダーバーもハイフンもまとめて選択したいのでたぶんシンボル単位の選択になる。Jedit Xで括弧の上でのダブルクリックの挙動についても。）もともとそうなってた。
;; 文字カテゴリの作成
;; http://smallsteps.seesaa.net/article/123661899.html
;; (define-category ?U "Upper case")
;; (define-category ?L "Lower case")
;; ;; 文字の登録。とりあえずはAからZまでの英字のみ。
;; (modify-category-entry (cons ?A ?Z) ?U)
;; (modify-category-entry (cons ?a ?z) ?L)
;; 小文字に大文字が続く場合を単語境界とする。
;; (add-to-list 'word-separating-categories (cons ?L ?U))



;;; jidaikobo.init.el ends here
