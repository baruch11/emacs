;; Set up custom.el file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; Bootstrap crafted-emacs in init.el
(load "~/RepoGit/crafted-emacs/modules/crafted-init-config")

;; Add package definitions for completion packages
;; to `package-selected-packages'.
(require 'crafted-completion-packages)

;; Install selected packages
(package-install-selected-packages :noconfirm)

;; Load configuration for the completion module
(require 'crafted-completion-config)






;; lcal packages loading with require
(add-to-list 'load-path "~/.emacs.d/lisp")

;; (require 'package)
;; (package-initialize)


(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)


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


(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

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



;; anaconda
(setenv "ANACONDA_HOME" "~/opt/miniconda3/")
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


;; magit
(unless (package-installed-p 'magit)
  (package-install 'magit))


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

(load-file "~/.emacs.d/mail.el")


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



(unless (package-installed-p 'powerline-evil)
  (package-install 'powerline-evil))

(use-package powerline-evil)
(powerline-evil-center-color-theme)

;; Or if you use use-package
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(put 'narrow-to-region 'disabled nil)
(setq dashboard-items '((recents  . 5)
                        (projects . 3)
                        (agenda . 5)))

;; code cells
(defun convert-ipynb-to-markdown ()
  "Convert the current buffer from ipynb format to markdown using jupytext."
  (when (and (string= (file-name-extension buffer-file-name) "ipynb")
             (executable-find "jupytext"))
    (shell-command (concat "jupytext --to markdown " buffer-file-name))))

(use-package code-cells
  :ensure t)
(add-hook 'after-save-hook 'convert-ipynb-to-markdown)


(with-eval-after-load 'code-cells
  (let ((map code-cells-mode-map))
    (define-key map (kbd "M-p") 'code-cells-backward-cell)
    (define-key map (kbd "M-n") 'code-cells-forward-cell)
    (define-key map (kbd "C-c C-c") 'code-cells-eval)
    ;; Overriding other minor mode bindings requires some insistence...
    (define-key map [remap jupyter-eval-line-or-region] 'code-cells-eval)))

(with-eval-after-load 'code-cells
  (let ((map code-cells-mode-map))
    (define-key map [remap evil-search-next]
      (code-cells-speed-key 'code-cells-forward-cell)) ;; n
    (define-key map [remap evil-paste-after]
      (code-cells-speed-key 'code-cells-backward-cell)) ;; p
    (define-key map [remap evil-backward-word-begin]
      (code-cells-speed-key 'code-cells-eval-above)) ;; b
    (define-key map [remap evil-forward-word-end]
      (code-cells-speed-key 'code-cells-eval)) ;; e
    (define-key map [remap evil-jump-forward]
      (code-cells-speed-key 'outline-cycle)))) ;; TAB

;; display iso week https://www.emacswiki.org/emacs/CalendarWeekNumbers
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 0.7)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

;; workaround for postgresql misalignement
;; https://www.emacswiki.org/emacs/SqlMode#h5o-5
(add-hook 'sql-login-hook 'my-sql-login-hook)
(defun my-sql-login-hook ()
  "Custom SQL log-in behaviours. See `sql-login-hook'."
  ;; n.b. If you are looking for a response and need to parse the
  ;; response, use `sql-redirect-value' instead of `comint-send-string'.
  (when (eq sql-product 'postgres)
    (let ((proc (get-buffer-process (current-buffer))))
      ;; Output each query before executing it. (n.b. this also avoids
      ;; the psql prompt breaking the alignment of query results.)
      (comint-send-string proc "\\set ECHO queries\n"))))

;; org todo
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE")
        (sequence "|" "BACKLOG")
        (sequence "|" "CANCELED")))

(setq org-todo-keyword-faces
      '(("BACKLOG" . "orange") ("CANCELED" . "blue")))

