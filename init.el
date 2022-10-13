;;; init.el -- initializing all configs and setings
;;; commentary:
;;; code:

;; Interface
(setq inhibit-startup-message t)
(display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(visual-line-mode 1)
(setq visible-bell t)
(load-theme zenburn)


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



;;; primitive configs
;; convenience
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
(setq initial-buffer-choice "~/.emacs.d/init.el")
(load-theme 'zenburn t)
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
;;; 


;; yasnippet
(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1))

;; amx
(use-package amx
  :ensure t
  :init (amx-mode))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


;; highlight symbol
(use-package highlight-symbol
  :ensure t
  :init (highlight-symbol-mode)
  :bind ("C-c h" . highlight-symbol))

;;; good-scroll
(use-package good-scroll
  :ensure t
  :if window-system
  :init (good-scroll-mode))

;;; undo-tree
(use-package undo-tree
  :ensure t
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

;;; ivy
(use-package counsel
  :ensure t)
(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
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
   ("C-r" . counsel-minibuffer-history)))

;;; goto-line-preview
(use-package goto-line-preview
  :ensure t
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))

;;; beacon
(use-package beacon
  :ensure t
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
   (setq evil-shift-width 4)
  :ensure t
  :config
   (evil-mode t)
   (setq evil-want-C-u-scroll t)
   (setq evil-want-C-d-scroll t)
   (setq evil-move-beyond-eol t)
   (setq evil-undo-system t)
   (setq evil-undo-system 'undo-tree))

;;; flycheck
(use-package flycheck
  :ensure t
  :hook
  (prog-mode . flycheck-mode))

;;; all-the-icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :config
  (add-to-list 'load-path "~/.emacs.d/icon"))
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
   '("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "d8cec8251169ccfe192aa87d69b9378bc81599330f31498f85deaef633721302" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(org-agenda-files
   '("d:/Emacs/.emacs.d/Agenda/dayview.org" "d:/Emacs/.emacs.d/Agenda/inbox.org" "d:/Emacs/.emacs.d/Agenda/workout.org"))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(snippet quote
	     (smart-mode-line-powerline-theme rainbow-delimiters highlight-symbol dashboard good-scroll smart-mode-line undo-tree mwim ace-window amx counsel monokai-theme yasnippet window-numbering use-package org-roam neotree monokai-pro-theme ivy goto-line-preview flycheck evil dracula-theme beacon atom-one-dark-theme all-the-icons))))
