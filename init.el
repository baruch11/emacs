(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package)) 

(require 'use-package)

;; key bindings
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c g") 'list-matching-lines)
(global-set-key (kbd "C-c a") 'org-agenda)
;; disable right alt to be able to type ~ | etc.
(setq ns-right-alternate-modifier nil)


(use-package exec-path-from-shell
  :ensure t)

;; LSP
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         ;;(lsp-mode . lsp-enable-which-key-integration)
	 )
  :ensure t
  :commands lsp)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-show-with-mouse nil
	lsp-ui-doc-max-height 50
	lsp-ui-doc-show-with-cursor t
	lsp-ui-doc-delay 1
	lsp-ui-doc-enable nil) ;; disable doc mode
  )
;;https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

;; company : autocompletion
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 1))

;; ;; elpy
;; (use-package elpy
;;   :ensure t
;;   :config
;;   (setq elpy-rpc-virtualenv-path 'current)
;;   :init
;;   (elpy-enable))

;; anaconda
(use-package conda
  :ensure t
  :config
  (setq conda-anaconda-home (expand-file-name "~/opt/miniconda3/"))
  (setq conda-env-home-directory (expand-file-name "~/opt/miniconda3/"))
  (setq conda-env-subdirectory "envs")
  ;; from https://github.com/necaris/conda.el
  ;; if you want interactive shell support, include:
  (conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; if you want auto-activation (see below for details), include:
  (conda-env-autoactivate-mode t)
  (conda-env-activate "base")
  )

;; poetry
(use-package poetry
  :ensure t)

;; org config
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; mail config
(load-file "./email.el")

;; magit
(use-package magit
  :ensure t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode t)
 '(electric-pair-mode t)
 '(global-auto-revert-mode t)
 '(inhibit-startup-screen t)
 '(org-agenda-files
   '("/Users/charlesprat/RepoGit/emacs.org" "/Users/charlesprat/RepoGit/missiontransition/mt.org" "/Users/charlesprat/.emacs.d/misc_todo.org" "/Users/charlesprat/.emacs.d/bouboulinos.org" "/Users/charlesprat/RepoGit/yotta/yotta.org"))
 '(package-selected-packages
   '(magit lsp-ui lsp-mode exec-path-from-shell conda poetry company use-package))
 '(scroll-bar-mode nil)
 '(show-paren-mode 1)
 '(tool-bar-mode nil)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo")))))
