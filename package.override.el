;;; package.override.el --- package.override.el for fourplus
;; Copyright (C) 2015 by fourplus
;; Author: fourplus

;;; ------------------------------------------------------------
;;; Commentary:

;;; Code:
;;; ------------------------------------------------------------
;;; package類のロード等

;;; load path を追加
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
        (expand-file-name (concat user-emacs-directory path))))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path))))))
(add-to-load-path "elisp")

;;; auto-install
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; package
(require 'package)
;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; MELPA-stableを追加
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;; Marmaladeを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; ELPAを追加
(add-to-list 'package-archives  '("ELPA" . "http://tromey.com/elpa/") t)
;; Gnuを追加
;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;; Orgを追加
;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; パッケージ情報の更新
(package-refresh-contents)

;;; インストールするパッケージ
(defvar my/favorite-packages
  '(
    ;; ver.1.0.0
    anything
    ;; https://gist.github.com/myuhe/467982 からダウンロード
    ;; anything-gist
    auto-async-byte-compile
    auto-complete
    auto-install
    bind-key
    cursor-chg
    descbinds-anything
    duplicate-thing
    eldoc-extension
    elscreen
    elscreen-persist
    flycheck
    gist
    google-translate
    js2-mode
    magit
    mic-paren
    multi-term
    multiple-cursors
    open-junk-file
    php-completion
    ;; wget http://www.ne.jp/asahi/alpha/kazu/pub/emacs/phpdoc.el
    ;; phpdoc
    php-mode
    point-undo
    powerline
		projectile
    recentf-ext
    smartrep
    smart-tab
    undohist
    undo-tree
    web-mode
  ))

;; my/favorite-packagesからインストールしていないパッケージをインストール
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'pakcage.override)

;;; pakcage.override.el ends here
