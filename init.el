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

(require 'my-mail)


;; Install selected packages
(package-install-selected-packages :noconfirm)


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



