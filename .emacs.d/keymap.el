;;; keymap --- custom global keys

;;; Commentary:
;; Nothing in this file should depend on other packages
;; These are just bindings that I've gotten used to from other IDEs

;;; Code:
(global-set-key (kbd "<f5>") 'revert-buffer) ; easy buffer reload
(global-set-key (kbd "C-u") (fset 'kill-to-line-start [?\C-  ?\C-a backspace]))
(global-set-key (kbd "M-<up>") (fset 'move-line-up [?\C-x ?\C-t ?\C-2 ?\C-p]))
(global-set-key (kbd "M-<down>") (fset 'move-line-down "\C-n\C-x\C-t\C-p"))
(global-set-key (kbd "C-;") (fset 'comment-line-dwim [?\C-x ?\C-\; ?\C-p]))
(global-set-key (kbd "C-<prior>") 'switch-to-prev-buffer)
(global-set-key (kbd "C-<next>") 'switch-to-next-buffer)
(global-set-key (kbd "M-n") (fset 'jump-lines-down [?\C-5 ?\C-n]))
(global-set-key (kbd "M-p") (fset 'jump-lines-up [?\C-5 ?\C-p]))
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)

(global-set-key (kbd "C-c p") 'flycheck-previous-error)
(global-set-key (kbd "C-c n") 'flycheck-next-error)

(global-set-key (kbd "C-c u") (fset 'untabify-all
   [?\C-x ?h ?\M-x ?u ?n ?t return]))

(global-set-key (kbd "C-c b e") (fset 'b64-encode-all
   [?\C-x ?h ?\M-x ?b ?a ?s ?e return]))
(global-set-key (kbd "C-c b d") (fset 'b64-decode-all
   [?\C-x ?h ?\M-x ?b ?a ?s ?e ?\C-n return]))

;; set super v to yank on linux for mac compatibility
(when (eq system-type 'gnu/linux)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-s") 'save-buffer))

;;; keymap.el ends here
