;;;; package -- initializing all configs and setings
;;;; Commentary:
;;;; code:

;;; package initializing
(eval-when-compile
  (require 'use-package))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq url-proxy-services '(("no_proxy" . "^\\(192\\.168\\..*\\)")
                           ("http" . "127.0.0.1:7890")
			   ("https" . "127.0.0.1:7890")))
(require 'package)
(add-to-list 'package-archives '(("melpa" . "https://melpa.org/packages/")
				 ("gnu" . "https://elpa.gun.org/packages/")))
(package-initialize)

;;; primitive configs
;; convenience
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-auto-revert-mode t)
(delete-selection-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "M-SPC") 'set-mark-command)
;; graphic
(global-display-line-numbers-mode 1)
(display-line-numbers-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq initial-buffer-choice "~/.emacs.d/init.el")
(load-theme 'dracula t)
(toggle-truncate-lines 1)

;; Capture
(setq org-capture-templates '(("t" "Todo" entry
			       (file+headline "../Agenda/Routine.org" "Tasks") "* TODO %i%?")
			      ("d" "days' item" entry
			       (file+headline "../Agenda/dayview.org" "Today's Items") "* %i%? \n %U")))

(global-set-key (kbd "C-c c") 'org-capture)

;;; refile & archives
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
  (progn
   (global-set-key (kbd "C-c n t") 'neotree-toggle)
  (setq neo-window-fixed-size 20)))

;;; window-numbering
(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode t))

;;; evil
(use-package evil
  :init
  (progn
    (setq evil-shift-width 4))
  :ensure t
  :config
  (progn 
    (evil-mode t)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-C-d-scroll t)
    (setq evil-move-beyond-eol t)
    (setq evil-undo-system t)))
; (setq evil-undo-system t))

;;; flycheck
(use-package flycheck
  :ensure t
  :hook
  (prog-mode . flycheck-mode))

;;; all-the-icons
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
;(use-package org-roam
;  :ensure t
;  :custom
;  (org-roam-directory "e:/RoamNotes")
;  :bind (("C-c n l" . org-roam-buffer-toggle)
;	 ("C-c n f" . org-roam-node-find)
;	 ("C-c n i" . org-roam-node-insert))
;  :config
;  (org-roam-setup))
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


