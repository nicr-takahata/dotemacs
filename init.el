;;; init.override.el --- init.override.el for fourplus
;; Copyright (C) 2015 by fourplus
;; Author: fourplus
;; thx 『Emacs実践入門』『Emacs LISP バイブル』『Emacs テクニック バイブル』

;;; ------------------------------------------------------------
;;; Commentary:

;;; Code:
;;; ------------------------------------------------------------
;;; 定数定義
(defconst my/bg-color "#ffffff")
(defconst my/fg-color "#606060")
(defconst my/tab-color "#ececec")
(defconst my/line-color "#fefed5")

(setq is-deactivate-region-by-cursor nil)

;;; load jidaikobo.init.el
(when (file-exists-p "~/.emacs.d/jidaikobo.init.el")
		(load "~/.emacs.d/jidaikobo.init.el"))

;;; ------------------------------------------------------------
;;; init.elのリセット

;;; 関数名の非表示
(which-func-mode 0)

;; フレーム初期値
(setq default-frame-alist nil)
(add-to-list 'default-frame-alist '(font . "ricty-13"))

;; タブの可視化
(set-face-attribute 'whitespace-trailing nil
										:background my/bg-color
										:underline t)
(set-face-attribute 'whitespace-tab nil
										:background my/bg-color
										:foreground my/tab-color
										:underline nil)
(set-face-attribute 'whitespace-space nil
										:background my/bg-color
										:underline nil)
(set-face-attribute 'whitespace-empty nil
										:background my/bg-color
										:underline nil)

;; 現在行の色
(defadvice global-hl-line-timer-function (around hl-line-override activate)
	"Line cursor advice."
	(global-hl-line-unhighlight-all)
	(let ((global-hl-line-mode t))
		(set-face-attribute 'hl-line nil :foreground nil :background my/line-color :underline nil)
		(global-hl-line-highlight)))

;; 選択領域の色
(set-face-background 'region "#afc4da")

;;; ------------------------------------------------------------
;;; テーマフレームワークの設定

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-nicr")
;;(load-theme 'nicr-dark t)
(load-theme 'nicr-light t)

;;; ------------------------------------------------------------
;;; multi-term

;;; multi-term
;;(load "init-multi-term")

;;; ------------------------------------------------------------
;;; 言語別設定

;; モードが変更されても色が変更されない場合以下の2行を評価
;;(global-font-lock-mode t)
;;(require 'font-lock)

;;; C
(add-hook 'c-mode-common-hook
					(lambda ()
						(c-set-style "bsd")))

;;; Java

;;; PHP
(defun php-user-hook ()
  (setq php-search-url "http://jp.php.net/ja/")
  (setq php-manual-url "http://jp.php.net/manual/ja/")
  )
(add-hook 'php-mode-hook 'php-user-hook)

;; php-modeのインデント設定
(defun php-indent-hook ()
  (global-font-lock-mode t)
  (require 'font-lock)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 2)
  (c-set-offset 'arglist-intro '+) ; 配列の最初の要素が改行した場合
  (c-set-offset 'arglist-close 0)) ; 配列の閉じ括弧->インデントなし
(add-hook 'php-mode-hook 'php-indent-hook)

;; php-modeの補完を強化する
(defun php-completion-hook ()
  (require 'php-completion)
  (php-completion-mode t)
  (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)

  (require 'auto-complete)
  (make-variable-buffer-local 'ac-sources)
  (add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
  (add-to-list 'ac-sources 'ac-source-php-completion)
  (add-to-list 'ac-sources 'ac-source-filename)
  (auto-complete-mode t))
(add-hook 'php-mode-hook 'php-completion-hook)

;;; Ruby

;;; Perl

;;; JavaScript
(defun js-indent-hook ()
  ;; インデント幅を4にする
  (setq js-indent-level 2
        js-expr-indent-offset 2
        indent-tabs-mode nil)
  ;; switch文のcaseラベルをインデントする関数を定義する
  (defun my-js-indent-line ()
    (interactive)
    (let* ((parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (js--proper-indentation parse-status)))
      (back-to-indentation)
      (if (looking-at "case\\s-")
          (indent-line-to (+ indentation 2))
        (js-indent-line))
      (when (> offset 0) (forward-char offset))))
  ;; caseラベルのインデント処理をセットする
  (set (make-local-variable 'indent-line-function) 'my-js-indent-line)
)
(add-hook 'js2-mode-hook 'js-indent-hook)

;;; CoffeeScript

;;; TypeScript

;;; HTML

;;; CSS
;; ■■■要検討
(defun css-mode-hooks ()
  "css-mode hooks"
  ;; インデント幅を2にする
  (setq css-indent-offset 2)
  ;; インデントにタブ文字を使わない
  (setq-default indent-tabs-mode nil)
  )
(add-hook 'css-mode-hook 'css-mode-hooks)

;;; Elisp
(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    ;; Eldocによるエコーエリアへの表示
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

;;; ------------------------------------------------------------
;;; misc

;; ファイルが #! から始まる場合、+xを付けて保存する
(add-hook 'after-save-hook
    'executable-make-buffer-file-executable-if-script-p)

;;; ------------------------------------------------------------
;;; elscreen

(elscreen-persist-mode t)
;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))

(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))

(set-face-attribute 'elscreen-tab-background-face nil
      :background "grey60"
      :foreground "grey30")

(set-face-attribute 'elscreen-tab-control-face nil
      :background "grey20"
      :foreground "grey90")

(set-face-attribute 'elscreen-tab-current-screen-face nil
      :background "grey30"
      :foreground "grey90")

(set-face-attribute 'elscreen-tab-other-screen-face nil
      :background "grey30"
      :foreground "grey60")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; ------------------------------------------------------------
;;; plus-vim
;; (require 'plus-vim)

;;; ------------------------------------------------------------
;;; powerline
;; elscreenの設定の後に記述しないと不具合発生
(require 'powerline)
;; 指定したマイナーモードを非表示にします
(setq my/hidden-minor-modes
      '(undo-tree-mode
        eldoc-mode
        auto-complete-mode
        smart-tab-mode
        flycheck-mode
        abbrev-mode
        magit-auto-revert-mode))
(mapc (lambda (mode)
        (setq minor-mode-alist
                (cons (list mode "") (assq-delete-all mode minor-mode-alist))))
        my/hidden-minor-modes)

(make-face 'powerline-active3)
(make-face 'powerline-inactive3)
(make-face 'powerline-major-mode-active)
(make-face 'powerline-major-mode-inactive)
(custom-set-faces
  '(mode-line ((t (:background "#c0c0c0":foreground "#f0f0f0"))))
  '(mode-line-buffer-id ((t (:foreground "#f0f0f0")))) ; ファイル名
  '(powerline-active1 ((t (:background "#404040" :foreground "#f0f0f0" :inherit mode-line))))
  '(powerline-active2 ((t (:background "#606060" :inherit mode-line))))
  '(powerline-active3 ((t (:background "#a0a0a0" :inherit mode-line))))
  '(powerline-major-mode-active ((t (:foreground "#f0f0f0" :inherit powerline-active3))))
  '(mode-line-inactive ((t (:background "#c0c0c0" :foreground "#b0b0b0"))))
  '(powerline-inactive1 ((t (:background "#404040" :inherit mode-line-inactive))))
  '(powerline-inactive2 ((t (:background "#606060" :inherit mode-line-inactive))))
  '(powerline-inactive3 ((t (:background "#a0a0a0" :inherit mode-line-inactive))))
  '(powerline-major-mode-inactive ((t (:foreground "#404040" :inherit powerline-inactive3)))))

(defvar powerline-major-mode-color-success "#60f060")
(defvar powerline-major-mode-color-error "#f06060")
(defvar powerline-major-mode-color-warning "#f0f060")
(defvar powerline-major-mode-color-info "#6060f0")
(defun update-powerline-major-mode-face ()
(let ((color (cond ((flycheck-has-current-errors-p 'error) powerline-major-mode-color-error)
                    ((flycheck-has-current-errors-p 'warning) powerline-major-mode-color-warning)
                    ((flycheck-has-current-errors-p 'info) powerline-major-mode-color-info)
                    (t powerline-major-mode-color-success))))
	   (set-face-attribute 'powerline-major-mode-active nil :foreground color)
     (set-face-attribute 'powerline-major-mode-inactive nil :foreground "#404040")))
(add-hook 'flycheck-after-syntax-check-hook 'update-powerline-major-mode-face)
(add-hook 'flycheck-syntax-check-failed-hook 'update-powerline-major-mode-face)

(defun wc-line ()
(let ((chars (if (use-region-p) (abs (- (point) (mark))) (point-max)))
        (words (if (use-region-p) (count-words-region (point) (mark)) (count-words-region (point-min) (point-max))))
        (lines (if (use-region-p) (abs (- (line-number-at-pos (point)) (line-number-at-pos (mark)))) (line-number-at-pos (point-max)))))
       (format "%d L " lines)))

(setq-default mode-line-format
  '("%e"
    (:eval
      (let* ((active (or (not (boundp 'powerline-selected-window)) (powerline-selected-window-active)))
             (mode-line (if active 'mode-line 'mode-line-inactive))
             (face1 (if active 'powerline-active1 'powerline-inactive1))
             (face2 (if active 'powerline-active2 'powerline-inactive2))
             (face3 (if active 'powerline-active3 'powerline-inactive3))
             (major-mode-face (if active 'powerline-major-mode-active 'powerline-major-mode-inactive))
             (separator-left (intern (format "powerline-%s-%s"
                                        powerline-default-separator
                                        (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                        powerline-default-separator
                                        (cdr powerline-default-separator-dir))))
             (lhs (list
                    (powerline-raw "%*" face1 'l)
                    (powerline-buffer-id face1 'l)
                    (powerline-raw " " face1)
                    (funcall separator-left face1 face2)
                    (powerline-raw mode-line-mule-info face2 'l)
                    (powerline-raw " " face2)
                    (funcall separator-left face2 face3)
                    (powerline-major-mode major-mode-face 'l)
                    (powerline-narrow face3 'l)
                    (powerline-raw " " face3)
                    (funcall separator-left face3 mode-line)
                    (powerline-minor-modes mode-line 'l)
                    (powerline-vc mode-line 'r)
                    (when (and (boundp 'which-func-mode) which-func-mode)
                    (powerline-raw which-func-format nil 'l))))
             (rhs (list
                    (powerline-raw global-mode-string mode-line 'r)
                    (powerline-raw (wc-line) mode-line 'r)
                    (funcall separator-right mode-line face3)
                    (powerline-raw "%4l:%3c" face3 'r)
                    (funcall separator-right face3 face2)
                    (powerline-raw " %6p" face2 'r)
                    (powerline-hud mode-line face3))))
              (concat (powerline-render lhs)
                      (powerline-fill mode-line (powerline-width rhs))
                        (powerline-render rhs))))))

;;; init.el ends here
