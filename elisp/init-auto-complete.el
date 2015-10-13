;;; auto-complete
;; thx http://fukuyama.co/emacs-auto-complete
;; オートコンプリート

(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; ユーザ辞書ディレクトリ
(defvar ac-user-dict-dir (expand-file-name "~/.emacs.d/ac-dict/"))

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

;; 条件の追加
(add-to-list 'ac-modes 'text-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(setq-default ac-sources '(ac-source-words-in-same-mode-buffers
									 ac-source-symbols
									 ac-source-filename
									 ac-english-dict
									 ac-technical-term-dict))

