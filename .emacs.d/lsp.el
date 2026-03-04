;;; lsp --- language server protocol

;;; Commentary:
;; Language specific packages

;;; Code:

;; General
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; LSP mode
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
	:ensure t
	:commands (lsp lsp-deferred)
	:init
	(setq lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil          ; don't touch basic indent
        lsp-format-on-save nil
        ;; per language:
        lsp-typescript-format-enable nil
        lsp-eslint-format nil)
	:custom
	(lsp-auto-guess-root nil)
	(lsp-enable-snippet nil)
	(lsp-prefer-flymake nil) ; use flycheck instead of flymake
	:bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
	:hook (
				 ((js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode json-mode json-ts-mode vue-mode go-mode rust-mode) . lsp-deferred)
				 (lsp-mode . lsp-enable-which-key-integration)))

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

;;
;; Language specific  packages
;;

;; Golang
(use-package go-mode)
(use-package gotest)

;; Rust
(use-package rust-mode)

;; JS
;; js-mode is built-in
(use-package vue-mode
  :mode "\\.vue\\'"
  :hook (vue-mode . lsp-deferred))

;; json
;; highlighting support for editing json files is provided by js-mode

;; ts
(use-package typescript-mode :defer t)

;; yaml
(use-package yaml-mode :mode "\\.ya?ml\\'")

;; Lua
(use-package lua-mode
	:after lsp-mode)

;; Docker
(use-package dockerfile-mode)

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

;;
;; Formatting & linting
;;

;; defaults
(setq-default js-indent-level 2
              typescript-indent-level 2
              tab-width 2)
(global-set-key (kbd "C-c C-f") #'lsp-format-buffer)
(global-set-key (kbd "C-S-o") 'lsp-organize-imports)
(global-set-key (kbd "M-RET") 'lsp-ui-sideline-apply-code-actions)


;; Apheleia + Prettier (async, respects .editorconfig)
(use-package apheleia
  :ensure t
  :config
  ;; Ensure Prettier gets filepath to pick correct parser & project config
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  ;; Map modes -> Prettier
  (dolist (pair '((js-mode . prettier)
                  (js-ts-mode . prettier)
                  (typescript-mode . prettier)
                  (typescript-ts-mode . prettier)
                  (tsx-ts-mode . prettier)
                  (js-json-mode . prettier)
                  (js-json-ts-mode . prettier)
                  (json-mode . prettier)
                  (json-ts-mode . prettier)
                  (vue-mode . prettier)
                  (yaml-mode . prettier)))
    (setf (alist-get (car pair) apheleia-mode-alist) (cdr pair)))
  (apheleia-global-mode +1))

;; format using apheleia & prettier for JS/TS/JSON/Vue
(dolist (hook '(js-mode-hook js-ts-mode-hook
														 typescript-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook
														 json-mode-hook json-ts-mode-hook
														 vue-mode-hook yaml-mode-hook))
  (add-hook hook (lambda () (apheleia-mode +1))))

;; Go & Rust: explicit save hooks for LSP formatting (and imports for Go)
(dolist (hook '(go-mode-hook go-ts-mode-hook rust-mode-hook rust-ts-mode-hook))
  (add-hook hook
            (lambda ()
              ;; Make LSP the only formatter here
              (setq-local lsp-format-on-save t)
              (when (boundp 'apheleia-mode) (apheleia-mode -1))

              ;; Install buffer-local save hooks explicitly
              ;; (the 't at the end makes them buffer-local)
              (add-hook 'before-save-hook #'lsp-format-buffer nil t)

              ;; Go-only: organize imports on save via gopls
              (when (derived-mode-p 'go-mode 'go-ts-mode)
                (add-hook 'before-save-hook #'lsp-organize-imports nil t))

              ;; Avoid double-format from rust-mode’s rustfmt (if present)
              (when (boundp 'rust-format-on-save)
                (setq-local rust-format-on-save nil)))))
;;
;; sbt
;;

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
