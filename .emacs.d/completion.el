;;; completion --- auto-complete stuff
;;; Commentary:
;;; Code:

;; company autocompletion
(use-package company
  :ensure t
	:bind (:map company-active-map
							("M-n" . nil)
							("M-p" . nil)
							("C-n" . company-select-next)
							("C-p" . company-select-previous))
	:config
	(setq company-minimum-prefix-length 1)
	(setq company-tooltip-idle-delay 0)
	(setq company-tooltip-limit 20)                      ; bigger popup window
	(setq company-echo-delay 0)                          ; remove annoying blinking
	(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  :hook ((after-init . global-company-mode)))

(use-package yasnippet
	:ensure t
	:after company
	:commands yas-minor-mode
	:hook (
				 (go-mode . yas-minor-mode))
	:init
	(yas-global-mode 1))
(global-set-key (kbd "C-c y") 'company-yasnippet)

(use-package yasnippet-snippets
	:ensure t
	:after yasnippet)

;; https://github.com/abo-abo/hydra/wiki/YASnippet
(defhydra hydra-yasnippet (:color blue :hint nil)
  "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas-global-mode)
  ("m" yas-minor-mode)
  ("a" yas-reload-all))
(global-set-key (kbd "C-c s") 'hydra-yasnippet/body)

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
;; (defvar company-mode/enable-yas t
;;   "Enable yasnippet for all backends.")
;; (defun company-mode/backend-with-yas (backend)
;; 	"Enable yasnippet for BACKEND."
;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
;;       backend
;;     (append (if (consp backend) backend (list backend))
;;             '(:with company-yasnippet))))
;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;;; completion.el ends here
