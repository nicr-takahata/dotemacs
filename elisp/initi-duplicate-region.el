;; duplicate-region

(defun duplicate-region () 
	(interactive) 
	(let (selected) 
		(setq selected (buffer-substring 
										(region-beginning) (region-end)))
		(insert selected "\n")))
(provide 'duplicate-region)
(define-key global-map (kbd "s-d") 'duplicate-region)
