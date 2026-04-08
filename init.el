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

(require 'my-org-mode)

(require 'roam-tune)

(require 'my_custom)




;; disable right alt to be able to type ~ | etc.
(setq ns-right-alternate-modifier nil)













  :ensure t
