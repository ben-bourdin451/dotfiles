;;; buffer --- better buffer listing
;;; Commentary:

;;; Code:
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      '(("default"
				 ("Help" (or (name . "\*Help\*")
										 (name . "\*Apropos\*")
										 (name . "\*info\*")))
				 ("Misc" (or (name . "\*scratch\*")
										 (name . "\*Messages\*")
										 (name . "\*Warnings\*")))
				 ("emacs-config" (or (filename . ".emacs.d")
														 (filename . "emacs-config")))
				 )))
(add-hook 'ibuffer-mode-hook
					'(lambda ()
						 (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

;; doesn't auto-update
;; (use-package ibuffer-vc)
;; (add-hook 'ibuffer-mode-hook
;; 	  '(lambda ()
;; 	     (ibuffer-auto-mode 1)
;; 			 (ibuffer-vc-set-filter-groups-by-vc-root)
;; 	     (ibuffer-switch-to-saved-filter-groups "default")
;; 			 ))


;;; buffer.el ends here
