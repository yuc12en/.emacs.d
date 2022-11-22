  (require 'package)
  (setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
			   ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
			   ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))  
  (package-initialize)

  ;; refresh empty archives
  (unless package-archive-contents
    (package-refresh-contents))

  ;; install use-package if it hasn't be installed
  (unless (package-installed-p 'usepackage)
    (package-install 'use-package))
  (require 'use-package)

  ;; default to set :ensure t for all packages
  (setq use-package-always-ensure t)  
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))  
  (display-time)

  ;; bars
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)

  ;; numbers
  (column-number-mode 1) 
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-type 'relative)

  ;; theme
  (use-package doom-themes
    :init (load-theme 'doom-dracula t))
  ;; modeline
  (use-package doom-modeline
    :init (doom-modeline-mode 1)
    :custom ((doom-modeline-height 10)))

  ;; font
  (set-face-attribute 'default nil :font "Source Code Pro"  :height 120)
  (set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 120 )
  (set-face-attribute 'variable-pitch nil :font "Source Code Pro"  :height 120 :weight 'regular)

  ;; stratup
  (setq inhibit-startup-message t)
  ;; begin or end of a line
  (use-package mwim
    :bind
    ("C-a" . mwim-beginning-of-code-or-line)
    ("C-e" . mwim-end-of-code-or-line))
  ;; neotree
  (use-package neotree
    :ensure t)

  ;; org-structre
  (setq org-startup-folded "show2levels")
  (setq org-hide-block-startup t)
  (setq org-ellipsis " ▾")
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "✧" "○" "▢"  )))

  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
  (setq org-confirm-babel-evaluate nil)

  ; templates
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("latex" . "src latex"))




  ;; evil
  (setq evil-want-keybinding nil)
  (use-package evil
    :init
    (setq evil-shift-width 2)
    (setq evil-want-integration t)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-d-scroll t)
    (setq evil-visual-screen-line t)
    :config
    (evil-mode t)
    (setq evil-move-beyond-eol t))
  ;; evil-collection
  (use-package evil-collection
    :after evil
    :config
    (setq forge-add-default-bindings nil)
    (evil-collection-init))

  ;; evil-surround
  (use-package evil-surround
    :config
    (global-evil-surround-mode 1)
    (add-to-list 'evil-surround-pairs-alist '(?\" ("\"" . "\"" ))))


;; helm
(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-x C-b") #'helm-buffers-list)
  (helm-mode 1))


   
   
   
  ;; yasnippet
  (use-package yasnippet
    :diminish
    :init
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    :config
    (yas-global-mode 1))


  ;; flycheck
  (use-package flycheck
    :ensure t
    :hook
    (prog-mode . flycheck-mode))

  ;; hydra
  (use-package hydra)

  ;; general
  (use-package general)


;; python
;; C-c C-c evaluate the current python script
;; C-RET evaluate the curernt statement
;; C-c C-d displays documentation for the thing under cursor, quit with "q"
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package highlight-indent-guides)

(use-package pyenv-mode
  :ensure t)



(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . lsp-modeline-code-actions-mode)
	 (lsp-mode . lsp-headerline-breadcrumb-mode))
  :commands lsp
  :config
  (setq gc-cons-threshold 900000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-log-io nil))
(with-eval-after-load 'lsp-mode
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace))



;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

(use-package projectile
  :hook
  (prog-mode-hook . projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
(use-package company)
(use-package helm-pydoc)
(use-package py-yapf)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(highlight-indent-guides elpy highlight-indent-guides-mode pyenv-mode pyenv py-yapf pydoc company-mode helm-pydoc linum-relative helm zone-sl which-key use-package undo-tree solaire-mode rainbow-delimiters python-mode pandoc org-bullets nyan-mode no-littering neotree mwim minimap lsp-ui lsp-ivy ivy-rich ivy-prescient ivy-bibtex indent-guide highlight-symbol helpful good-scroll general forge flycheck evil-surround evil-nerd-commenter evil-collection ebib doom-themes doom-modeline dap-mode counsel-projectile company-box bug-hunter beacon amx all-the-icons aggressive-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
