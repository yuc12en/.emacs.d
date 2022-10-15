;;; init.el -- initializing all configs and setings
;;; commentary:
;;; code:

;; Interface and basic settings
(setq inhibit-startup-message t)
; line numbers and column numbers, ignore them when in org, term and eshell
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(dolist (mode '(term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(Scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(visual-line-mode 1)
(setq visible-bell t)
(load-theme 'doom-dracula t)

;; key bindings 
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; package initializing
(require 'package)
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'usepackage)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;; org



;; handy commands

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-auto-revert-mode t)
(delete-selection-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(electric-pair-mode t)
(global-set-key (kbd "M-SPC") 'set-mark-command)
(require 'init-utils )
(global-set-key (kbd "C-c i") 'insert-time-string)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; Capture
(setq org-capture-templates '(("t" "Todo" entry
			       (file+headline "~/.emacs.d/Agenda/Routine.org" "Tasks") "* TODO %i%?")
			      ("d" "days' item" entry
			       (file+headline "~/.emacs.d/Agenda/dayview.org" "Today's Items") "* %i%? \n %U")))

(global-set-key (kbd "C-c c") 'org-capture)

;; Agenda
(setq org-agenda-files '("~/.emacs.d/Agenda"))
(global-set-key (kbd "C-c a") 'org-agenda)


;;projectile
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
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

;; yasnippet
(use-package yasnippet
  :diminish
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1))

;; WichKey
(use-package which-key
  :diminish
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;; general
(use-package general
  :config
  (general-create-definer yc/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (yc/leader-keys
   "t" '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme")))
;; hydra
(use-package hydra
  :config
  (defhydra hydra-text-scale (:timeout 4)
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("i" nil "finished" :exit t)))
;; amx
(use-package amx
  :ensure t
  :init (amx-mode))

;; helpful
(use-package helpful
  :custom
  (ounsel-decribe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-fubction] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;doom-themes
(use-package doom-themes)

    

;;; all-the-icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :config
  (add-to-list 'load-path "~/.emacs.d/icon"))
;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;;ivyRich
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; highlight symbol
(use-package highlight-symbol
  :init (highlight-symbol-mode)
  :bind ("C-c h" . highlight-symbol))

;;; good-scroll
(use-package good-scroll
  :ensure t
  :if window-system
  :init (good-scroll-mode))

;;; undo-tree
(use-package undo-tree
  :diminish
  :config
  (setq sml/theme 'powerline)
  (global-undo-tree-mode))

;;; mwin
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;;; ace-window
(use-package ace-window
  :ensure t
  :bind
(("C-x o" . 'ace-window)))
;;Helpful
(use-package )

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

;;; goto-line-preview
(use-package goto-line-preview
  :ensure t
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

;;; beacon
(use-package beacon
  :diminish
  :config
  (beacon-mode t))

;;; neo-tree
(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-c n t") 'neotree-toggle)
  (setq neo-window-fixed-size 20)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;;; window-numbering
(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode t))

;;; evil
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

;; evil-collection  it can  be tuned by edit evil-collection-mode
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
;;; flycheck
(use-package flycheck
  :ensure t
  :hook
  (prog-mode . flycheck-mode))

;(use-package org-roam
;  :ensure t
;  :custom
;  (org-roam-directory "~/.emacs.d/RoamNotes")
;  :bind (("C-c n l" . org-roam-buffer-toggle)
;	 ("C-c n f" . org-roam-node-find)
;	 ("C-c n i" . org-roam-node-insert))
;  :config
; (org-roam-setup))
;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "fc2fa0ef550cbbf5d4d0a0e8b208d0dc75110918c9ad00823439b86748669736" "c4cecd97a6b30d129971302fd8298c2ff56189db0a94570e7238bc95f9389cfb" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "d8cec8251169ccfe192aa87d69b9378bc81599330f31498f85deaef633721302" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(org-agenda-files
   '("d:/Emacs/.emacs.d/Agenda/dayview.org" "d:/Emacs/.emacs.d/Agenda/inbox.org" "d:/Emacs/.emacs.d/Agenda/workout.org"))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(evil-magit magit counsel-projectile projectile hydra evil-collection general helpful ivy-rich doom-themes which-key doom-modeline use-package snippet quote
		(smart-mode-line-powerline-theme rainbow-delimiters highlight-symbol dashboard good-scroll smart-mode-line undo-tree mwim ace-window amx counsel monokai-theme yasnippet window-numbering use-package org-roam neotree monokai-pro-theme ivy goto-line-preview flycheck evil dracula-theme beacon atom-one-dark-theme all-the-icons))))
