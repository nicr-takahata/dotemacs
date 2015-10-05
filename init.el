;;; init.el by jidaikobo-shibata
;;; thx 『Emacs実践入門』
;;; thx http://rubikitch.com

;;; ------------------------------------------------------------
;;; usage: emacsのインストール（要X-code Command Line Tools）
;;; thx http://masutaka.net/chalow/2015-04-12-1.html
;; curl -LO http://ftp.gnu.org/pub/gnu/emacs/emacs-24.5.tar.xz
;; curl -LO ftp://ftp.math.s.chiba-u.ac.jp/emacs/emacs-24.5-mac-5.9.tar.gz
;; tar xfJ emacs-24.5.tar.xz
;; tar xfz emacs-24.5-mac-5.9.tar.gz
;; cd emacs-24.5
;; patch -p 1 < ../emacs-24.5-mac-5.9/patch-mac
;; cp -r ../emacs-24.5-mac-5.9/mac mac
;; cp ../emacs-24.5-mac-5.9/src/* src
;; cp ../emacs-24.5-mac-5.9/lisp/term/mac-win.el lisp/term
;; \cp nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns mac/Emacs.app/Contents/Resources/Emacs.icns
;; ./configure --prefix=$HOME/opt/emacs-24.5 --with-mac --without-x
;; make
;; make GZIP_PROG='' install
;; cp -r mac/Emacs.app ~/Applications

;;; ------------------------------------------------------------
;;; usage: init.elの反映
;; M-x eval-buffer RET init.el

;;; ------------------------------------------------------------
;;; usage: 利用前の準備
;;; このinit.elを~/.emacs.dに入れる前に、以下手順を踏んでおくこと。
;;; @ terminal
;; mkdir -p ~/.emacs.d/elisp
;; cd ~/.emacs.d/elisp
;; curl -0 http://www.emacswiki.org/emacs/download/auto-install.el
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
;; (package-initialize)
;; M-x install-elisp RET http://cx4a.org/pub/undohist.el
;; M-x auto-install-batch RET anything RET
;; M-x package-install RET auto-complete RET
;; M-x package-install RET undohist RET
;; M-x package-install RET undo-tree RET
;; M-x package-install RET recentf RET
;; M-x package-install RET cursor-chg RET
;; M-x package-install RET smart-tab
;; M-x package-install RET multi-term RET
;; M-x package-install RET google-translate RET
;; M-x package-install RET s RET
;; M-x package-install RET f RET
;; M-x package-install RET web-mode RET
;; M-x package-install RET auto-async-byte-compile RET
;; M-x package-install RET point-undo RET
;; M-x package-install RET multiple-cursors RET
;; M-x package-install RET smartrep RET

;;; ------------------------------------------------------------
;;; package類のロード等

;;; load-pathの追加
(add-to-list 'load-path "~/.emacs.d/elisp")

;;; auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/elisp")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

;;; undohist
;; ファイルを閉じてもundoの履歴を残す
(require 'undohist)
(undohist-initialize)

;;; undo-tree
;; redo (cmd+shft+z)
(require 'undo-tree)
(global-undo-tree-mode t)

;;; multi-term
(load "multi-term-init")

;;; recentf
;; 最近開いたファイルの履歴
(require 'recentf-ext)
(recentf-mode 1)

;;; Anything
;; C-;
(require 'anything)
(require 'anything-config)
(add-to-list 'anything-sources 'anything-c-source-emacs-commands)

;;; s
;; https://github.com/magnars/s.el
;; 文字列処理の関数群
(require 's)

;;; f
;; https://github.com/rejeep/f.el
;; ファイル処理の関数群
(require 'f)

;;; web-mode
;; テンプレート編集時のモード
(require 'web-mode)

;;; duplicate-region
;; 行／選択範囲の複製 (cmd+d)
(require 'duplicate-region)

;;; cursor-chg
;; カーソルの形状を変更
(require 'cursor-chg)
(toggle-cursor-type-when-idle 1)

;;; smart-tab
;; コンテキストに応じたtabキー。auto-completeと共存
(require 'smart-tab)
(global-smart-tab-mode)

;;; google-translate
;; 選択範囲の言語を確認して翻訳 (C-c t)
(load "google-translate-init")

;;; auto-complete
;; オートコンプリート
(load "auto-complete-init")

;;; point-undo
;; カーソル位置履歴 (undo: M-s-left, redo: M-s-right)
(require 'point-undo)

;;; multiple-cursors
;; 複数カーソル (キーバインドは後述)
(require 'multiple-cursors)

;;; smartrep
;; 連続操作
(require 'smartrep)

;;; auto-async-byte-compile
;; .el自動コンパイルファイルを保存時に自動でバイトコンパイル。init.elを除く
;; http://www.emacswiki.org/emacs/auto-async-byte-compile.el
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "init.el")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;; create-temporary-buffer
;; あたらしい空のバッファを作る (cmd+t)
(load "create-temporary-buffer")

;;; ------------------------------------------------------------
;;; キーバインド登録

;;; mac likeなcmd関係
;; thx http://d.hatena.ne.jp/gan2/20080109/1199887209
;; thx http://www.unixuser.org/~euske/doc/emacsref/#file
(global-set-key (kbd "s-c") 'kill-ring-save) ; copy (cmd+c)
(global-set-key (kbd "s-x") 'kill-region) ; cut (cmd+x)
(global-set-key (kbd "s-v") 'yank) ; paste (cmd+v)
(global-set-key (kbd "s-s") 'save-buffer) ; save (cmd+s)
(global-set-key (kbd "s-S") 'write-file) ; save as (cmd+shift+s)
(global-set-key (kbd "s-o") 'find-file) ; open (cmd+o)
(global-set-key (kbd "s-f") 'isearch-forward) ; search (cmd+f)
(global-set-key (kbd "s-g") 'isearch-forward) ; search forward (cmd+g)
(global-set-key (kbd "s-G") 'isearch-backward) ; search backword (cmd+shift+g)
(global-set-key (kbd "s-z") 'undo-tree-undo) ; undo (cmd+z)
(global-set-key (kbd "s-Z") 'undo-tree-redo) ; redo (cmd+shift+z)
(global-set-key (kbd "s-+") 'text-scale-increase) ; resize increase (cmd++)
(global-set-key [s-kp-add] 'text-scale-increase) ; resize increase (cmd++)
(global-set-key (kbd "s--") 'text-scale-decrease) ; resize decrease (cmd+-)
(global-set-key [s-kp-subtract] 'text-scale-decrease) ; resize decrease (cmd+-)
;; (define-key global-map [s-kp-equal] (text-scale-mode 0))
;; (define-key global-map (kbd "s-=") (text-scale-mode 0))
;; (define-key global-map [s-kp-0] (text-scale-mode 0))
;; (define-key global-map (kbd "s-0") (text-scale-mode 0))
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs) ; quit (cmd+q)
(define-key global-map [s-up] 'beginning-of-buffer) ; cmd+up
(define-key global-map [s-down] 'end-of-buffer) ; cmd+down
(define-key global-map [s-left] 'beginning-of-line) ; cmd+left
(define-key global-map [s-right] 'end-of-line) ; cmd+right

;;; escでM-g
;; http://emacswiki.org/emacs/CancelingInEmacs
(setq normal-escape-enabled t)
(define-key isearch-mode-map [escape] 'isearch-abort) ; isearch
(define-key isearch-mode-map "\e" 'isearch-abort) ; \e seems to work better for terminals
(global-set-key [escape] 'keyboard-escape-quit) ; everywhere else

;;; mac like close window (cmd+w)
;; cmd+wで、開いているウィンドウを閉じる。最後のバッファなら、バッファを閉じる
(defun contexual-close-window ()
	"mac like close window (cmd+w)."
  (interactive)
	(if (one-window-p)
			(kill-buffer)
		(kill-buffer-and-window)))
(define-key global-map (kbd "s-w") 'contexual-close-window)

;; mac like new window (cmd+n)
;; cmd+n でウィンドウを増やす。分割方法は対話式
(defun create-new-window-intaractive (act)
	"mac like new window (cmd+n)."
  (interactive "nchoose (holizntal:1, vertical:2):")
	(cond ((eq act 2) (split-window-horizontally))
				(t (split-window-vertically))))
(define-key global-map (kbd "s-n") 'create-new-window-intaractive)

;; temporary
(define-key global-map [M-up] 'previous-line)
(define-key global-map [M-down] 'next-line)

;;; C-aで、開始場所と先頭をトグル
;; thx http://qiita.com/ShingoFukuyama/items/62269c4904ca085f9149
(defun my-goto-line-beginning-or-indent (&optional $position)
  (interactive)
  (or $position (setq $position (point)))
  (let (($starting-position (progn (back-to-indentation) (point))))
    (if (eq $starting-position $position)
      (move-beginning-of-line 1))))
(global-set-key (kbd "C-a") 'my-goto-line-beginning-or-indent)

;;; Shift+Returnで<br />を入力
(define-key global-map [S-return] "<br />")

;;; opt+¥でバックスラッシュを入力
(define-key global-map (kbd "M-¥") "\\")

;;; ウィンドウ切り替え (M-tab)
(define-key global-map [M-tab] 'other-window)

;;; M-g で指定行へジャンプ
(global-set-key "\M-g" 'goto-line)

;;; anything
(define-key global-map (kbd "C-;") 'anything)

;; create shell another at window
;; thx http://qiita.com/7gano@github/items/6afbe24a00c4ee4a634b
(defun create-shell-window-vertically ()
  (interactive)
	(when (one-window-p)
		(split-window-vertically))
	(other-window 1)
	(multi-term))
(define-key global-map (kbd "M-!") 'create-shell-window-vertically)

;; point undo and redo
;; opt+cmd+[left|right]でカーソル履歴移動
(define-key global-map [M-s-left] 'point-undo)
(define-key global-map [M-s-right] 'point-redo)

;; multiple-cursor and smartrep
;; 複数箇所選択と編集
(declare-function smartrep-define-key "smartrep")

(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)

(global-unset-key "\C-t")

(smartrep-define-key global-map "C-t"
  '(("C-t"      . 'mc/mark-next-like-this)
    ("n"        . 'mc/mark-next-like-this)
    ("p"        . 'mc/mark-previous-like-this)
    ("m"        . 'mc/mark-more-like-this-extended)
    ("u"        . 'mc/unmark-next-like-this)
    ("U"        . 'mc/unmark-previous-like-this)
    ("s"        . 'mc/skip-to-next-like-this)
    ("S"        . 'mc/skip-to-previous-like-this)
    ("*"        . 'mc/mark-all-like-this)
    ("d"        . 'mc/mark-all-like-this-dwim)
    ("i"        . 'mc/insert-numbers)
    ("o"        . 'mc/sort-regions)
    ("O"        . 'mc/reverse-regions)))

;;; ------------------------------------------------------------
;; マウス設定
;; thx http://cave.under.jp/_contents/emacs.html#60
(if window-system (progn
;; 右ボタンの割り当て(押しながらの操作)をはずす。
(global-unset-key [down-mouse-3])

;; マウスの右クリックメニューを出す(押して、離したときにだけメニューが出る)
(defun bingalls-edit-menu (event)
  (interactive "e")
  (popup-menu menu-bar-edit-menu))
(global-set-key [mouse-3] 'bingalls-edit-menu)))

;;; ------------------------------------------------------------
;;; ほか諸設定

;; Rictyを等幅で使う（ウェブ上で散見される記事と違い）
(load "ricty")

;; リージョンを上書きできるようにする
(delete-selection-mode t)

;; inline patchを有効に
(setq default-input-method "MacOSX")

;; CmdをMetaにしない
(setq mac-pass-command-to-system nil)
(setq mac-command-modifier 'super)

;; optキーがMeta
(setq mac-option-modifier 'meta)

;; IME関係
;; thx http://qiita.com/catatsuy/items/c5fa34ead92d496b8a51

;; emacs 起動時は英数モードから始める
;(add-hook 'after-init-hook 'mac-change-language-to-us)

;; minibuffer 内は英数モードにする
;(add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)

;; [migemo]isearch のとき IME を英数モードにする
;(add-hook 'isearch-mode-hook 'mac-change-language-to-us)

;; ドラッグアンドドロップでファイルを開く＋あたらしいウィンドウでひらかない
(define-key global-map [ns-drag-file] 'ns-find-file)
(setq ns-pop-up-frames nil)

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

;; ウィンドウサイズの位置、サイズ
(if window-system (progn
  (setq initial-frame-alist '((width . 210)(height . 50)(top . 0)(left . 10)))
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
(setq hl-line-face 'underline)
(global-hl-line-mode)
;; (require 'hl-line)
;; (defun global-hl-line-timer-function ()
;;   (global-hl-line-unhighlight-all)
;;   (let ((global-hl-line-mode t))
;;     (global-hl-line-highlight)))
;; (setq global-hl-line-timer
;;       (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))
;; (cancel-timer global-hl-line-timer)

;; 釣り合いのとれる括弧のハイライト
(show-paren-mode 1)

;;; elscreen
;(require 'elscreen)
;(setq elscreen-prefix-key (kbd "C-z"))
;(elscreen-start)


;; -------------------------------------------------
;; -------------------------------------------------
;; -------------------------------------------------
;; under construct
(defun do-searsh-from-other-window-string ()
	"mac like new window (cmd+n)."
  (interactive)
	(split-window-horizontally)
)
;(define-key global-map (kbd "s-f") 'do-searsh-from-other-window-string)


;;現在バッファのファイルのフルパスを取得

(defun my-get-current-path ()
  (interactive)
  (or (buffer-file-name) (expand-file-name default-directory)))

;;バッファのテキストエンコーディングを表示
(defun my-show-text-encoding ()
  (interactive)
  (message (format "%s" buffer-file-coding-system)))

;; HTML:タグとタグの間、またはタグ内を一気に選択
;;http://qiita.com/ShingoFukuyama/items/62269c4904ca085f9149
;;ここよく見ること。やりたいことがたくさん書いてある！
(defun my-region-angle-brackets ()
  (interactive)
  (let ($pt)
    (skip-chars-backward "^<>")
    (setq $pt (point))
    (skip-chars-forward "^<>")
    (set-mark $pt)))

;;後述の置換に使用
(defun my-replace-strings-in-region-by-list ($list)
  "Replace strings in a region according to $list"
  (if mark-active
      (let* (($beg (region-beginning))
             ($end (region-end))
             ($word (buffer-substring-no-properties $beg $end)))
        (mapc (lambda ($r)
                (setq $word (replace-regexp-in-string (car $r) (cdr $r) $word)))
              $list)
        (delete-region $beg $end)
        (insert $word))
    (error "Need to make region")))

;;選択範囲を1行にする
(defun my-join-multi-lines-to-one ()
  (interactive)
  (my-replace-strings-in-region-by-list
   '(("\\(\n\\s-*\\)+" . ""))))

;;s-wrap

(defun my-command-test () 
	(interactive) 
	(let (src)
		(setq src (s-split "
" (buffer-substring 
									(region-beginning) (region-end))))
		(message src)))

(defun my-command-test1 () 
	(interactive)
	(let (lines)
		(setq lines (s-split "
" (buffer-substring 
									(region-beginning) (region-end))))
		;; (setq lines '())
		;; (add-to-list 'lines line)
		(while lines
			(insert "res: ")
			(insert (car lines) "\n")
			(setq lines (cdr lines)))))

;(define-key global-map (kbd "s-2") 'my-command-test1)


