;;; lsp --- language server protocol

;;; Commentary:
;; Language specific packages

;;; Code:

;; General save hooks
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; editorconfig
;; doesn't work
;; (use-package editorconfig
;;   :ensure t
;;   :config
;;   (editorconfig-mode 1))

;; LSP mode
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
	:ensure t
	:commands lsp
	:custom
	(lsp-auto-guess-root nil)
	(lsp-enable-snippet nil)
	(lsp-prefer-flymake nil) ; use flycheck instead of flymake
	:bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
	:hook (
				 ((go-mode rust-mode scala-mode typescript-mode) . lsp)
				 (lsp-mode . lsp-enable-which-key-integration)))
(global-set-key (kbd "C-S-o") 'lsp-organize-imports)
(global-set-key (kbd "M-RET") 'lsp-ui-sideline-apply-code-actions)

;; (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

(defun lsp-save-actions ()
	"LSP save actions."
	(lsp-format-buffer)
	(lsp-organize-imports))

(use-package lsp-ui
	:after lsp-mode
	:commands lsp-ui-mode
	:init
	(setq lsp-ui-sideline-show-diagnostics t))

(use-package lsp-ivy
	:after lsp-mode
	:commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
	:after lsp-mode
	:commands lsp-treemacs-errors-list
	:init
	(lsp-treemacs-sync-mode 1))
;; (lsp-metals-treeview-show-when-views-received t))

;; https://github.com/abo-abo/hydra/wiki/lsp-mode
(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration     [_i_] implementation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition      [_t_] type
 [_n_] next error       [_M-s_] describe session   [_o_] documentation   [_s_] signature
 [_p_] prev error                                [_R_] references      [_r_] rename
 [_l_] list errors
 [_x_] execute action                            [_q_] quit"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

	("n" flycheck-next-error :exit nil)
	("p" flycheck-previous-error :exit nil)
	("l" flycheck-list-errors)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)
  ("q" nil)
  ("RET" nil)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-workspace-restart)
  ("S" lsp-workspace-shutdown))
(global-set-key (kbd "C-c l") 'hydra-lsp/body)

;; Use the Debug Adapter Protocol for running tests and debugging
;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe)
(use-package dap-mode
	:custom
	(dap-print-io t)
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))
(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

;; Performance
;; https://emacs-lsp.github.io/lsp-mode/page/installation/#performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Language specific  packages
; Golang
(use-package go-mode)

;; TODO: chain linters correctly
;; https://github.com/flycheck/flycheck/issues/1762
;; https://github.com/weijiangan/flycheck-golangci-lint/issues/8
;; (use-package flycheck-golangci-lint
;;   :ensure t
;;   :hook (go-mode . flycheck-golangci-lint-setup)
;; 	:config
;; 	;; (setq flycheck-golangci-lint-config "~/workspace/gists/golangci.yml")
;; 	(setq flycheck-golangci-lint-fast t))

(use-package gotest)
;; (define-key go-mode-map (kbd "C-c t") 'go-test-current-test)

(use-package company-go
	:after company)
(add-hook 'go-mode-hook (lambda ()
													(set (make-local-variable 'company-backends)
															 '((company-go company-yasnippet)))
													(company-mode)))
(add-hook 'go-mode-hook
					(lambda () (add-hook 'before-save-hook #'lsp-save-actions t 'local)))

;; Rust
(use-package rust-mode)

(add-hook 'before-save-hook (lambda () (when (eq 'rust-mode major-mode)
                                           (lsp-format-buffer))))

;; JS
(use-package vue-mode
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook #'lsp))

;; ts
(use-package typescript-mode)
(add-hook 'before-save-hook (lambda () (when (eq 'typescript-mode major-mode)
                                           (lsp-format-buffer))))

;; yaml
(use-package yaml-mode)

;; Lua
(use-package lua-mode
	:after lsp-mode)

;; Docker
(use-package dockerfile-mode)

;; Groovy & jenkins
(use-package groovy-mode
	:config
	(setq groovy-indent-offset 2)
	(load-file-safe "~/.emacs.d/jenkins-mode.el"))


;; HCL
(use-package hcl-mode)
(use-package terraform-mode)
(add-hook 'terraform-mode-hook
					(lambda () (add-hook 'before-save-hook #'terraform-format-buffer t 'local)))
;; Doesn't work for some reason
;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection '("/usr/local/bin/terraform-ls" "serve"))
;;                   :major-modes '(terraform-mode)
;;                   :server-id 'terraform-ls))
;; (add-hook 'terraform-mode-hook #'lsp)

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

;;; lsp.el ends here
