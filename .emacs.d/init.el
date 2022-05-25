;;; init --- Emacs setup
;;; Commentary:

;; Missing stuff
;; * symbol navigation hydra - https://github.com/bgwines/symbol-navigation-hydra/tree/6fd5d5dbcf9c57659a88d39a9cb1a8b819028360
;; * scratch files with different modes
;; * custom shorcuts for frames
;; * go gen tests https://github.com/s-kostyaev/go-gen-test
;; * shell with aliases
;; * terraform-ls https://github.com/hashicorp/terraform-ls
;; * RSS feeds: elfeed or NewsTicker
;; * markdown https://cestlaz.github.io/post/using-emacs-59-markdown/ https://github.com/jrblevin/markdown-mode
;; * show leading whitespace only & discretely (https://www.emacswiki.org/emacs/WhiteSpace)

;; Nice to have
;; * autoyas https://github.com/abo-abo/auto-yasnippet
;; * magit forge https://cestlaz.github.io/post/using-emacs-62-magit-forge/
;; * better dired (https://github.com/Fuco1/dired-hacks) or dired+? fix hydra
;; * ivy manual (http://oremacs.com/swiper/#completion-styles) fzf? M-x without regex ^
;; * dictionary & thesaurus https://cestlaz.github.io/post/using-emacs-56-dictionaries

;; Examples
;; https://pages.sachachua.com/.emacs.d/Sacha.html

;;; Code:
(server-start)

;; Custom utility functions
(defun load-file-safe (f)
  "Load the elisp file F only if it exists and is readable."
  (when (file-readable-p f) (load-file f)))

;; Package manager
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Load theme as quickly as possible
(use-package doom-themes
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-spacegrey t)
	(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
	(doom-themes-treemacs-config))
(use-package all-the-icons
	:init
	;; (all-the-icons-install-fonts)
	)

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; old theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;; (load-theme 'ben t)

;; Nyan cat
(use-package nyan-mode
	:init
	(nyan-mode 1))

;; Backups
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq backup-by-copying-when-linked t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Copy pasta - mac os only
;; (when (eq system-type 'darwin)
;; 	(defun copy-from-osx ()
;; 		(shell-command-to-string "pbpaste"))
;; 	(defun paste-to-osx (text &optional push)
;; 		(let ((process-connection-type nil))
;; 			(let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
;; 				(process-send-string proc text)
;; 				(process-send-eof proc))))
;; 	(setq interprogram-cut-function 'paste-to-osx)
;; 	(setq interprogram-paste-function 'copy-from-osx))

;; Display line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Turn off menubar/toolbar/scrollbar
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq inhibit-startup-message t)			 ; don't display welcome message
(fset 'yes-or-no-p 'y-or-n-p)					 ; simple yes or no questions
(delete-selection-mode 1) ; when a region is selected, typing will overwrite it
(global-hl-line-mode t)		; highlight current line
(put 'narrow-to-region 'disabled nil)		; narrow regions
(windmove-default-keybindings)					; better window movement
(setq tramp-default-method "ssh")				; faster than default scp
(setq electric-pair-mode t)							; pair quotes, brackets etc...
;; Unix utf8 please
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; set colours for whitespace-mode (and which things whitespace-cleanup should clean)
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark trailing empty)))
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; change font size
(set-face-attribute 'default (selected-frame) :height 130)

;; Packages
(use-package try)

(use-package which-key
  :config (which-key-mode))

(use-package ace-window
	:init
	(progn
		(custom-set-faces
		 '(aw-leading-char-face
			 ((t (:inherit ace-jump-face-foreground :height 3.0)))))
		))

;; better undo
(use-package undo-tree
  :init (global-undo-tree-mode)
	:custom
	 (undo-tree-history-directory-alist '(("" . "~/.emacs.d/backup-undo-tree"))))

;; ability to edit a grep buffer and apply those changes
(use-package wgrep)

;; expand the marked region in semantic increments (negative prefix to reduce region)
(use-package expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; multi-cursors
;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "M-S-<up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-S-<down>") 'mc/mark-next-like-this)
(global-set-key (kbd "M-d") 'mc/mark-next-like-this-word)
(global-set-key (kbd "M-D") 'mc/mark-previous-like-this-word)

;; (define-key mc/keymap (kbd "<return>") nil) ; make <return> insert newlines; quit with C-g

(use-package crux)
(global-set-key (kbd "C-S-d") 'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-d") 'kill-whole-line)

;; syntax highlighting
(use-package flycheck
  :init (global-flycheck-mode))

;; smart tabs
(use-package smart-tabs-mode)
(smart-tabs-insinuate 'javascript)

;; zoom windows tmux style
(use-package zoom-window
	:bind (("C-x C-z" . zoom-window-zoom))
	:config
	(custom-set-variables
	 '(zoom-window-mode-line-color "DarkGreen")))


(use-package hydra
	:ensure t)
;; TODO: load everything from a hydra folder
;; https://emacs.stackexchange.com/questions/18418/split-configuration-into-multiple-files
(load-file-safe "~/.emacs.d/hydra/dired.el")
(load-file-safe "~/.emacs.d/hydra/ibuffer.el")
(load-file-safe "~/.emacs.d/hydra/window.el")
;; (add-to-list 'load-path "~/.emacs.d/hydra")

;; Load more config
(load-file-safe "~/.emacs.d/buffer.el")
(load-file-safe "~/.emacs.d/search.el")
(load-file-safe "~/.emacs.d/git.el")
(load-file-safe "~/.emacs.d/dir.el")
(load-file-safe "~/.emacs.d/completion.el")
(load-file-safe "~/.emacs.d/lsp.el")
(load-file-safe "~/.emacs.d/keymap.el")


;; Automatically added stuff
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
	 '("aa5dee47c85f12d166745ae56c778eb7833df3f6799c2b2d607d5b8da8f5f579" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "82360e5f96244ce8cc6e765eeebe7788c2c5f3aeb96c1a765629c5c7937c0b5b" "811853cd3c1a1b653d5d247736d2b3527d84382b8104aa39d19606e3e4142b3f" default))
 '(lsp-auto-guess-root nil)
 '(lsp-enable-snippet nil)
 '(lsp-go-build-flags ["-tags=integration"])
 '(lsp-prefer-flymake nil t)
 '(package-selected-packages
	 '(all-the-icons wgrep rust-mode flycheck-golangci-lint yaml-mode vue-mode zoom-window smart-tabs-mode dockerfile-mode typescript-mode groovy-mode nyan-mode gotest company-go lsp-ivy lsp-ui doom-themes terraform-mode dap-mode posframe sbt-mode scala-mode crux yasnippet-snippets yasnippet doom-modeline lsp-mode hcl-mode treemacs-magit dumb-jump undo-tree multiple-cursors git-timemachine expand-region magit git-gutter dired+ treemacs-icons-dired treemacs-projectile treemacs lua-mode ivy which-key go-mode flycheck company counsel swiper try use-package))
 '(zoom-window-mode-line-color "DarkGreen"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

;;; init.el ends here
