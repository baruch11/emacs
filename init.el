;; Set up custom.el file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; Bootstrap crafted-emacs in init.el
;; (load "~/RepoGit/crafted-emacs/modules/crafted-init-config")
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'my-init)

(require 'my-completion)

(require 'my-evil)


;; Add package definitions for completion packages
;; to `package-selected-packages'.
;; (require 'crafted-completion-packages)

;; Install selected packages
(package-install-selected-packages :noconfirm)

;; Load configuration for the completion module
;;(require 'crafted-completion-config)


;; lcal packages loading with require

;; (require 'package)
;; (package-initialize)


(unless (package-installed-p 'use-package)
  (package-install 'use-package))



;; disable right alt to be able to type ~ | etc.
(setq ns-right-alternate-modifier nil)



(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))

;; which-key
(use-package which-key
  :ensure t
  :config  (which-key-mode))


(use-package poetry
  :ensure t)
(use-package dockerfile-mode
  :ensure t)



;; magit
(unless (package-installed-p 'magit)
  (package-install 'magit))


(use-package magit)

;; python interpreter
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt"))


(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))


(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


(set-face-attribute 'default nil :height 140)



(use-package yaml-mode
  :ensure t)

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter)) 


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

