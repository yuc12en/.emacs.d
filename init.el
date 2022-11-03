;; archives
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

;; time
(display-time)

;; bars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

;; number
(column-number-mode 1) 
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(dolist (mode '(term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; theme
(use-package doom-themes
  :init (load-theme 'doom-dracula t))

;; modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

;; Nyanyanyanyanyanyanyanyanyan
(use-package nyan-mode
  :config
  (nyan-mode 1)
  (setq nyan-animate-nyancat nil))


;; icons
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (add-to-list 'load-path "~/.emacs.d/icon"))

;; font
(set-face-attribute 'default nil :font "Source Code Pro"  :height 120)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 120 )
(set-face-attribute 'variable-pitch nil :font "Source Code Pro"  :height 120 :weight 'regular)

;; stratup
(setq inhibit-startup-message t)

;; init debug
(use-package bug-hunter)

;; windows
;;; undo & redo
(use-package winner
:config
(winner-mode 1))
;;; ace-window
(use-package ace-window
  :bind
  ("C-x o" . ace-window)
  :config
(setq aw-background nil)
(ace-window-display-mode t))


;; buffer
;;; buffer move
(require 'buffer-move)
;;; buffer color
(use-package solaire-mode
  :config
  (solaire-global-mode t))

;; lines
;; begin or end of a line
(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))


;; neotree 
(require 'init-dir)
(use-package neotree
  :ensure t
  :config
  (setq neo-window-fixed-size 20)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; undo-tree
(use-package undo-tree
  :diminish
  :config
  (setq sml/theme 'powerline)
  (global-undo-tree-mode))

;; zone
(use-package zone)

;; beacon
(use-package beacon
  :diminish
  (beacon-mode t))

;; startup
(setq org-startup-folded "show2levels")
(setq org-hide-block-startup t)
(setq org-ellipsis " ▾")
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "✧" "○" "▢"  )))

;; todo keywords
(setq org-todo-keywords
  '((sequence "TODO(t)" "|" "DONE(d)" )
    (sequence "EMERGENCY(e!)" "WORTHY(w!)" "NEED(n@/!)" "|" "FEEDBACK(f)" "OVER(o)" "SUSPEND(s)" )
    (sequence "|" "CANCLED(c)")))

;; Tag
(setq org-tag-alist '(
  (:startgrouptag) ("place") (:grouptags)
  ("@Class". ?w) ("@Dormitory" . ?d)
  (:endgrouptag)
  ("intrests" . ?i) ("hard" . ?h)
  ))


; GTD
; org-agenda-category-icon-alist
; org-agenda-filter-by-tag \
; org-agenda-filter-by-category <
; org-agenda-filter-by-regexp =
; org-agenda-filter-by-effort _
; org-agenda-filter-by-top-headline ^
; org-agenda-filter /
(setq org-agenda-start-with-follow-mode t)
(setq org-agenda-files '("e:/Zen/current_working_sheet.org"))
(setq org-capture-templates '(("c" "capture raw items" entry
			       (file+headline "e:/GTD/Inbox.org" "Capture") "* TODO %?"))) 
(setq org-refile-targets '(("e:/Zen/GTD.org" :level . 2)))

;; short-cut
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

  (dolist (mode '(org-agenda-mode-hook))
    (add-hook mode (lambda () (evil-mode 0))))

  (setq evil-move-beyond-eol t)
  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'eshell-mode 'insert))

;; evil-collection
(use-package evil-collection
  :after evil
  :config
  (setq forge-add-default-bindings nil)
  (evil-collection-init))

;; evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (add-to-list 'evil-surround-pairs-alist '(?\" ("\"" . "\"" ))))

;; pre-requirities
(use-package amx
  :init (amx-mode))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	("C-x b" . counsel-ibuffer)
	("C-x C-f" . counsel-find-file)
	:map minibuffer-local-map
	("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

;; ivy
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

(use-package yasnippet
  :diminish
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1))

;; startup
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package highlight-symbol
  :init (highlight-symbol-mode)
  :bind ("C-c h" . highlight-symbol))
(use-package aggressive-indent
  :hook
  (prog-mode-hook. aggressive-indent-mode))
(use-package indent-guide
  :hook
  (prog-mode-hook . indent-guide-mode))
(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)

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
  (when (file-directory-p "~/.emacs.d/Projects/Code")
    (setq projectile-project-search-path '("~/.emacs.d/Projects/Code")))
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

(use-package dap-mode
  ;; uncomment the config below if you want all ui panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; set up node debugging
  (require 'dap-node)
  (dap-node-setup) ;; automatically installs node debug adapter if needed

  ;; bind `c-c l d` to `dap-hydra` for easy access
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "d" '(dap-hydra t :wk "debugger")))

(use-package elpy)
(use-package python-mode
  :mode "\\.py\\'"
  :after elpy
  :hook
  (python-mode . lsp-deferred)
  (python-mode . elpy-mode)
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python)
  (elpy-enable))
(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

;; ebib
(use-package ebib
  :config
  (setq ebib-preload-bib-files '("e:/papers/reference.bib"))
  (setq ebib-index-columns '(
			     ("Entry Key" 40 t)
			     ("Author/Editor" 40 t)
			     ("Year" 6 t)
			     ("Title" 50 t)
			     ))
  (setq ebib-use-timestamp t))
(require 'org-ebib)
;; ebib-citation-commands

;; ivy-bibtex
(use-package ivy-bibtex
  :config
  (setq ivy-re-builders-alist
	'((ivy-bibtex . ivy--regex-ignore-order)
	  (t . ivy--regex-plus))))
(setq bibtex-completion-bibliography
      '("e:/papers/reference.bib"))

(setq bibtex-completion-format-citation-functions
      '((org-mode      . bibtex-completion-format-citation-ebib)
	(latex-mode    . bibtex-completion-format-citation-cite)
	(markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
	(default       . bibtex-completion-format-citation-default))) 

(setq ivy-bibtex-default-action 'ivy-bibtex-insert-key)

;; ebib-create-org-url-link KEY DB
;; ebib--display-url-field

(use-package which-key
  :diminish which-keym-ode
  :init (which-key-mode t)
  :config
  (setq which-key-idle-delay 1))

(use-package minimap)
(use-package evil-nerd-commenter)

;; hydra
(use-package hydra)  
;;; window
(defhydra hydra-window (evil-normal-state-map "C-w")
  "window"
  ("," evil-window-decrease-width "width decrease")
  ("." evil-window-increase-width "width increase")
  ("-" evil-window-decrease-height "height decrease")
  ("=" evil-window-increase-height "height decrease")
  ("u" winner-undo "undo")
  ("r" winner-redo "redo")
  ("j" windmove-down "move down" :exit t)
  ("k" windmove-up "move up" :exit t)
  ("h" windmove-left "move left" :exit t)
  ("l" windmove-right "move right" :exit t))

;;; buffer
(defhydra hydra-buffer(evil-normal-state-map "C-b")
  ("h" buf-move-left "left")
  ("l" buf-move-right "right")
  ("j" buf-move-down "down")
  ("k" buf-move-up "up"))



(use-package general
  :after evil)
(general-create-definer spc/leader-keys
  :keymaps '(normal visual)
  :prefix "SPC")
(spc/leader-keys
 "e" '(eval-buffer :which-key "eval buffer")
 "s" '(save-buffer :which-key "save buffer")
 "b" '(ivy-switch-buffer :which-key "switch buffer"))
(spc/leader-keys
 "o" '(:ignore t :which-key "Org command")
 "ob" '((lambda () (interactive) (org-babel-tangle)) :which-key "Babel")
 "oa" '(org-agenda :which-key "Agenda")
 "oc" '(org-goto-calendar :which-key "Capture")
 "od" '(org-capture :which-key "Calendar")
 "of" '(org-refile :which-key "Refile")
 "ol" '(org-store-link :which-key "Link"))
(spc/leader-keys
 "n" '(:ignore t :which-key "narrow")
 "nr" '(narrow-to-region :which-key "narrow to region")
 "ns" '(org-narrow-to-subtree :which-key "narrow to subtree")
 "nb" '(org-narrow-to-block :whic-key "narrow to block")
 "nw" '(widen :which-key "widen")
 "ne" '(eaf-open-demo :which-key "Screen"))
(spc/leader-keys
 "f" '(:ingore t :which-key "find")
 "fs" '(swiper :which-key "words")
 "ff" '(counsel-find-file :which-key "file")
 "fd" '(counsel-dired) :which-key "dired"
 "fn" '(dir/neo-here :which-key "neotree")
 "fb" '(ivy-switch-buffer :which-key "swith buffer"))
(spc/leader-keys
 "p" '(:ignore t :which-key "papers")
 "pe" '(ebib :which-key "ebib")
 "pb" '(ivy-bibtex :which-key "ivy-tex")
 "pd" '(org-insert-drawer :which-key "drawer")
 "pt" '(org-set-tags-command :which-key "tags"))


(require 'init-utils )
(require 'init-keys)

(general-define-key
 :keymaps 'global-map
 "<escape>" 'keyboard-escape-quit
 "M-SPC" 'set-mark-command
 "C-c i" 'insert-time-string
 "C--" 'text-scale-decrease
 "C-=" 'text-scale-increase)

;; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(general-define-key
 :keymaps 'evil-normal-state-map
 "C-/" 'evilnc-comment-or-uncomment-lines
 "C-<" 'org-speedbar-set-agenda-restriction
 "C->" 'org-agenda-remove-restriction-lock
 "C-g" 'evil-nomal-state)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(zoom zone-sl zone-rainbow zone-nyan zenburn-theme window-numbering which-key use-package undo-tree solarized-theme solaire-mode smart-mode-line rainbow-delimiters quelpa python-mode ox-pandoc org-roam org-bullets nyan-mode no-littering neotree mwim move-text minimap markdownfmt lsp-ui lsp-ivy lentic language-id ivy-rich ivy-prescient ivy-bibtex inheritenv indent-guide hyperbole highlight-symbol helpful helm-themes goto-line-preview good-scroll general forge focus flycheck ewal evil-surround evil-nerd-commenter evil-collection elpy ebib dracula-theme doom-themes doom-modeline dimmer dashboard dap-mode counsel-projectile company-emojify company-box bug-hunter beacon auctex amx all-the-icons aggressive-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
