;;; git --- git related stuff
;;; Commentary:

;;; Code:
(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1))

(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                            :hint nil)
  "
Git gutter:
  _n_: next hunk        _s_tage hunk     _q_uit
  _p_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _P_opup hunk
  _f_: first hunk
  _l_: last hunk        set start _R_evision
"
  ("n" git-gutter:next-hunk)
  ("p" git-gutter:previous-hunk)
  ("f" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("P" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
	 :color blue))
(global-set-key (kbd "C-c h") 'hydra-git-gutter/body)


(use-package git-timemachine
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
				 ("C-x M-g" . magit-dispatch)))

;; GitHub in emacs
;; https://magit.vc/manual/forge/index.html
;; (use-package forge
;;   :after magit)

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-multiframe)
(setq ediff-split-window-function 'split-window-horizontally) ; show side-by-side diff

;; focus control panel on start
(add-hook 'ediff-startup-hook
					(lambda () 
						(progn
							(select-frame-by-name "Ediff")
							(insert "?")
							(set-frame-size(selected-frame) 40 10))))

;; Some custom configuration to ediff
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
	"*Register to be set up to hold `my-ediff-bwin-config' configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e
	"*Register to be used to hold `my-ediff-awin-config' window configuration.")

(defun my-ediff-bsh ()
	"Function to be called before any buffers or window setup for ediff."
	(setq my-ediff-bwin-config (current-window-configuration))
	(when (characterp my-ediff-bwin-reg)
		(set-register my-ediff-bwin-reg
									(list my-ediff-bwin-config (point-marker)))))

(defun my-ediff-ash ()
	"Function to be called after buffers and window setup for ediff."
	(setq my-ediff-awin-config (current-window-configuration))
	(when (characterp my-ediff-awin-reg)
		(set-register my-ediff-awin-reg
									(list my-ediff-awin-config (point-marker)))))

(defun my-ediff-qh ()
	"Function to be called when ediff quits."
	(when my-ediff-bwin-config
		(set-window-configuration my-ediff-bwin-config)))

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh 'append)


;; get repository url from file
(defun git-repo ()
	"Retrieve repository url from file."
	(interactive)
	(let* ((repository (shell-command-to-string "git config --get remote.origin.url"))
				 (trimmed (replace-regexp-in-string "[ \t\n\r]+\\'" "" repository))
				 (removed-extension (replace-regexp-in-string "\\.git$" "" trimmed)))
		;; if the repository is less than 5 characters long, it's probably not valid
		(if (> (length trimmed) 5)
				(replace-regexp-in-string "git@\\(.*\\):" "https://\\1/" removed-extension))))

(defun git-file-url ()
	"Get remote file url from git repo."
  (let ((filename (buffer-file-name))
        (basedir (replace-regexp-in-string "[ \t\n\r]+\\'" "" (shell-command-to-string "git rev-parse --show-toplevel")))
        (branch (replace-regexp-in-string "[ \t\n\r]+\\'" "" (shell-command-to-string "git branch --show-current")))
        (repo-url (git-repo)))
    (concat repo-url "/blob/" branch (replace-regexp-in-string basedir "" filename))))

(defun git-grep-dir ()
	"Git grep in current buffer's directory."
	(interactive)
	(counsel-git-grep nil (file-name-directory (buffer-file-name)) t))

(global-set-key (kbd "C-c f") 'git-grep-dir)

;;; git.el ends here
