;; http://noqisofon.hatenablog.com/entry/20101102/1288647885
;; テンポラリバッファを作成し、それをウィンドウに表示

(defun create-temporary-buffer ()
  "テンポラリバッファを作成し、それをウィンドウに表示します。"
  (interactive)
  (switch-to-buffer (generate-new-buffer "*temp*")))

;; cmd+t
(define-key global-map (kbd "s-t") 'create-temporary-buffer)
