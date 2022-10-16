; startup buffer
  (setq inhibit-startup-message t)

  ; column and line numbers , visual-lines
  (column-number-mode 1)
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-type 'relative)
  (dolist (mode '(term-mode-hook
                  eshell-mode-hook
                  shell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
  (visual-line-mode 1)

  ; scroll-, tool-, menu-bar , visible-bell
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (setq visible-bell t)
  ; good-scroll
  (use-package good-scroll
    :if window-system
    :init (good-scroll-mode))

  ; theme
  (use-package doom-themes
    :init (load-theme 'doom-dracula t))

  ; yes-or-no to y-o-n
  (fset 'yes-or-no-p 'y-or-n-p)

  ; icons
  (use-package all-the-icons
    :if (display-graphic-p)
    :config
    (add-to-list 'load-path "~/.emacs.d/icon"))
  (use-package doom-modeline
    :init (doom-modeline-mode 1)
    :custom ((doom-modeline-height 10)))

  ; beacon
  (use-package beacon
    :diminish

    (beacon-mode t))

  ; no-littering
  (use-package no-littering)

  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ; font

  (set-face-attribute 'default nil :font "hack"  :height 120)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono" :height 120 )

;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "DejaVu Sans Mono" :height 120 :weight 'regular)

(set-fontset-font "fontset-default"
                'unicode
                '("Cambria Math" . "iso10646-1"))

; archives
(require 'package)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

; refresh empty archives
(unless package-archive-contents
  (package-refresh-contents))

; install use-package if it hasn't be installed
(unless (package-installed-p 'usepackage)
  (package-install 'use-package))
(require 'use-package)

; default to set :ensure t for all packages
(setq use-package-always-ensure t)  
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

; mwin
(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

; ace-window
(use-package ace-window
  :bind
(("C-x o" . 'ace-window)))

(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-c n t") 'neotree-toggle)
  (setq neo-window-fixed-size 20)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(defun yc/org-mode-setup ()
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(add-hook 'org-mode-hook #'yc/org-mode-setup)
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-ellipsis " ▾")
(setq org-habit-graph-column 60)

(use-package org-bullets

  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

; add emacs-lisp and python
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))
(setq org-confirm-babel-evaluate nil)


; set templates
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
;; capture
(setq org-capture-templates '(("t" "todo" entry
			       (file+headline "~/.emacs.d/agenda/routine.org" "tasks") "* todo %i%?")
			      ("d" "days' item" entry
			       (file+headline "~/.emacs.d/agenda/dayview.org" "today's items") "* %i%? \n %u")))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-SPC") 'set-mark-command)

(require 'init-utils )
(global-set-key (kbd "C-c i") 'insert-time-string)

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

; evil
(use-package evil
  :init
  (setq evil-shift-width 2)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  :config
  (evil-mode t)
  (setq evil-move-beyond-eol t)
  (setq evil-undo-system t)
  (setq evil-undo-system 'undo-tree)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'eshell-mode 'insert))

; evil-collection  it can  be tuned by edit evil-collection-mode
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package amx
  :init (amx-mode))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; counsel M-o to some defined function
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	("C-x b" . counsel-ibuffer)
	("C-x C-f" . counsel-find-file)
	:map minibuffer-local-map
	("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

;ivy
(use-package ivy
  :diminish
  :init
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
  ("C-x b" . 'ivy-switch-buffer)
  ("C-c v" . 'ivy-push-view)
  ("C-c s" . 'ivy-switch-view)
  ("C-c V" . 'ivy-pop-view)
  ("C-x C-@" . 'counsel-mark-ring)
  ("C-x C-SPC" . 'counsel-mark-ring)
  :map minibuffer-local-map
  ("C-r". counsel-minibuffer-history))
  :config
  (ivy-mode 1))
; ivy-prescient
(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (ivy-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :diminish which-keym-ode
  :init (which-key-mode t)
  :config
  (setq which-key-idle-delay 1))

(use-package general
  :after evil
  :config
  (general-create-definer yc/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (yc/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package hydra
  :config
  (defhydra hydra-text-scale (:timeout 4)
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("i" nil "finished" :exit t)))

(use-package yasnippet
  :diminish
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1))

(use-package undo-tree
  :diminish
  :config
  (setq sml/theme 'powerline)
  (global-undo-tree-mode))

; highlight the paren
(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode)
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package highlight-symbol
  :init (highlight-symbol-mode)
  :bind ("C-c h" . highlight-symbol))

(use-package flycheck
  :ensure t
  :hook
  (prog-mode . flycheck-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))
(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))
(use-package lsp-treemacs
  :after lsp)
(use-package lsp-ivy
  :after lsp)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	 ("<tab>" . company-complete-selection))
	(:map lsp-mode-map
	 ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))



(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . lsp-deferred)
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))
(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet which-key use-package undo-tree rainbow-delimiters pyvenv python-mode org-bullets no-littering neotree mwim lsp-ui lsp-ivy ivy-rich ivy-prescient highlight-symbol helpful good-scroll general forge flycheck evil-nerd-commenter evil-collection doom-themes doom-modeline dap-mode counsel-projectile company-box beacon amx all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
