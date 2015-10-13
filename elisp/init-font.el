;;; Rictyを等幅で使う
(add-to-list 'default-frame-alist '(font . "ricty-16"))

;; thx http://tam5917.hatenablog.com/entry/20120915/1347688961
;;(if window-system
;;    (progn
;;      (create-fontset-from-ascii-font
;;       "Ricty:style=regular:spacing=0" nil "ricty")
;;
;;      (dolist (charset
;;               '(unicode
;;                 japanese-jisx0208
;;                 japanese-jisx0208-1978
;;                 japanese-jisx0212
;;                 japanese-jisx0213-1
;;                 japanese-jisx0213-2
;;                 japanese-jisx0213-a
;;                 japanese-jisx0213.2004-1
;;                 katakana-jisx0201))
;;        (set-fontset-font "fontset-ricty"
;;                          charset
;;                          (font-spec :family "Ricty" :size 16) ;16以外のサイズでは等幅にならない :-(
;;                          nil 'prepend))
;;
;;      (setq default-frame-alist
;;            (append (list
;;                     '(font . "fontset-ricty")
;;                     )
;;                    default-frame-alist))
;;      (set-face-attribute 'fixed-pitch nil :family "Ricty")
;;      ))

;;; Rictyの準備の仕方
;; thx Rictyなるものがあるらしい | cozy attic
;; https://cozyattic.wordpress.com/2013/07/24/ricty%E3%81%AA%E3%82%8B%E3%82%82%E3%81%AE%E3%81%8C%E3%81%82%E3%82%8B%E3%82%89%E3%81%97%E3%81%84/
; sudo port install fontforge
; で、fontforgeをインストール
; fontforge -version
; で、インストールの確認。
; https://github.com/yascentur/Ricty/tree/3.2.2
; で、Ricty-3.2.2.zipをダウンロード。
; http://levien.com/type/myfonts/inconsolata.html
; で、OpenType fileのInconsolata.otfをダウンロード。
; http://mix-mplus-ipa.sourceforge.jp/migu/
; で、migu-1m-20150712.zipのダウンロード。
; 全て解凍し、Inconsolata.otf、migu-1m-bold.ttf、migu-1m-regular.ttfをRictyのフォルダへ移動
; ターミナルでRictyフォルダに移動
; sh ricty_generator.sh Inconsolata.otf migu-1m-regular.ttf migu-1m-bold.ttf
; とする。僕はデフォルトだと全角スペースが目立ちすぎるので、
; sh ricty_generator.sh -z Inconsolata.otf migu-1m-regular.ttf migu-1m-bold.ttf
; として、全角スペースを不可視にしている。
; https://github.com/yascentur/Ricty/tree/3.2.2
; のREADMEには、このシェルスクリプトのオプションがあるので、参考にすると良い。


