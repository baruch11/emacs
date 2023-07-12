;; https://jeffkreeftmeijer.com/emacs-straight-use-package/
;; Install straight.el
;;(defvar bootstrap-version)
;;(let ((bootstrap-file
;;       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;      (bootstrap-version 5))
;;  (unless (file-exists-p bootstrap-file)
;;    (with-current-buffer
;;        (url-retrieve-synchronously
;;         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;         'silent 'inhibit-cookies)
;;      (goto-char (point-max))
;;      (eval-print-last-sexp)))
;;  (load bootstrap-file nil 'nomessage))
;;
;;;; Install use-package
;;(straight-use-package 'use-package)
;;
;;;; Configure use-package to use straight.el by default
;;(use-package straight
;;  :custom (straight-use-package-by-default t))
;;
;;(use-package org)
;;(use-package zenburn-theme)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)



;; lcal packages loading with require
(add-to-list 'load-path "~/.emacs.d/lisp")

;; (require 'package)
;; (package-initialize)


;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; (require 'use-package)


;; key bindings
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c g") 'list-matching-lines)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key "\C-x\C-b" 'buffer-menu)
;; disable right alt to be able to type ~ | etc.
(setq ns-right-alternate-modifier nil)


(use-package exec-path-from-shell)

;; which-key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; LSP
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp))
  :commands lsp
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (lsp-enable-which-key-integration t)
  )
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
  ;;(conda-env-initialize-interactive-shells)
  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)
  ;; if you want auto-activation (see below for details), include:
  ;;(conda-env-autoactivate-mode t)
  (conda-env-activate "base")
  )


(use-package poetry
  :ensure t)
(use-package dockerfile-mode
  :ensure t)

;; org config
(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; inhibit pair mode for < in org-mode, important for org-structure-template-alist
(add-hook 'org-mode-hook
	  (lambda () (setq-local electric-pair-inhibit-predicate `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

;; mail config
(load-file "~/.emacs.d/mail.el")

;; magit
(use-package magit)

;; python interpreter
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt"))

(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("28a34dd458a554d34de989e251dc965e3dc72bace7d096cdc29249d60f395a82" "2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" "c4cecd97a6b30d129971302fd8298c2ff56189db0a94570e7238bc95f9389cfb" default))
 '(delete-selection-mode t)
 '(dired-listing-switches "-alFh")
 '(display-fill-column-indicator-column 80)
 '(electric-pair-mode t)
 '(global-auto-revert-mode t)
 '(holiday-bahai-holidays nil)
 '(holiday-hebrew-holidays nil)
 '(holiday-islamic-holidays nil)
 '(inhibit-startup-screen t)
 '(org-agenda-files '("~/.emacs.d/bouboulinos.org"))
 '(org-agenda-include-diary t)
 '(org-babel-load-languages '((emacs-lisp . t) (python . t)))
 '(org-confirm-babel-evaluate nil)
 '(org-return-follows-link t)
 '(package-selected-packages
   '(sokoban mu4e dashboard jupyter mu4e-overview zenburn-theme markdown-toc flycheck dockerfile-mode projectile kubernetes yaml-mode org-bullets bash-completion pdf-tools inf-mongo which-key magit lsp-mode exec-path-from-shell conda poetry company use-package))
 '(python-shell-completion-native-enable nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode 1)
 '(tool-bar-mode nil)
 '(visible-bell t)
 '(warning-suppress-types
   '((org-element-cache)
     (org-element-cache)
     (org-element-cache)
     ((python python-shell-completion-native-turn-on-maybe)))))

(set-face-attribute 'default nil :height 140)



;; PYTHONPATH to curdir
(defun my/set-pythonpath_curdir()
  "SET PYTHONPATH TO CURRENT DIR."
  (interactive)
  (setenv "PYTHONPATH" (expand-file-name (nth 1 (split-string (pwd)))))
  )
(put 'upcase-region 'disabled nil)


(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable 
to match that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
					  ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;(load-file "~/.emacs.d/mail.el")


;; https://sqrtminusone.xyz/posts/2021-05-01-org-python/

(use-package jupyter
  :ensure t)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) ;; Other languages
   (shell . t)
   ;; Python & Jupyter
   (python . t)
   (jupyter . t)))
(org-babel-jupyter-override-src-block "python")
(setq ob-async-no-async-languages-alist '("python" "jupyter-python"))


(require 'jupyter-tune)
(require 'crafted-evil)
;;(require 'outlook-cal)


(use-package alert
  :ensure t
  :config
  (setq
       ;; alert-default-style 'notifier
       alert-default-style 'osx-notifier
       )
  ;; (alert "This is an alert" :severity 'high)
  ;; (alert "This is an alert" :title "My Alert" :category 'debug)
  )
(use-package org-alert
  :ensure t
  :custom (alert-default-style 'osx-notifier)
  :config
  (setq org-alert-interval 3600
	org-alert-notification-title "Org alert reminder")
  (org-alert-enable)
  )

(use-package yaml-mode
  :ensure t)

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter)) 

;; org-ai for chatgpt: 
;; put your API key in .netrc with the format
;; machine api.openai.com login org-ai password <your-api-key>
(use-package org-ai
  :ensure t
  :commands (org-ai-mode
             org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  ;;(setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets

(provide 'init)
;;; init.el ends here
(put 'set-goal-column 'disabled nil)

(require 'info)
(add-to-list 'Info-default-directory-list
             "~/.emacs.d/info/files")


;;(load-file "~/.emacs.d/roam-tune.el")
(require 'roam-tune)
(require 'my_custom)
;;(use-package org-roam)

(use-package swiper
  :ensure t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))


(use-package counsel
  :ensure t
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :ensure t
  :after counsel
  :config
  (ivy-prescient-mode 1))

;; Remember candidate frequencies across sessions
(prescient-persist-mode 1)

(put 'magit-clean 'disabled nil)

(use-package powerline-evil)
(powerline-evil-center-color-theme)

;; Or if you use use-package
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(put 'narrow-to-region 'disabled nil)
