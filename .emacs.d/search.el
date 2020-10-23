;;; search.el --- search & find utilities
;;; Commentary:

;;; Code:
(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-display-style 'fancy))

(use-package counsel
  :ensure t
  :bind (("M-y" . counsel-yank-pop)
	 :map ivy-minibuffer-map
	 ("M-y" . ivy-next-line)))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-S-s" . swiper-thing-at-point)
	 ("C-S-r" . swiper-thing-at-point)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (setq enable-recursive-minibuffers t)
    ;; enable this if you want `swiper' to use it
    ;; (setq search-default-mode #'char-fold-to-regexp)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    ))

;; jump to chars
;; https://github.com/abo-abo/avy
(use-package avy)
(global-set-key (kbd "C-'") 'avy-goto-char)
(global-set-key (kbd "C-\"") 'avy-goto-char-2)

;; jump to definitions
;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g g" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-prefer-searcher 'rg))

;;; search.el ends here
