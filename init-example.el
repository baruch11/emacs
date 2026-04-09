(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)

(unless (package-installed-p 'use-package)
;; Set up custom.el file
  (package-install 'use-package))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
	   (file-exists-p custom-file))
  (load custom-file nil :nomessage))

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'my-init)

(require 'my-project)

(require 'my-completion)

(require 'my-evil)

;;(require 'my-mail)

(require 'my-org-mode)

(require 'roam-tune)

(require 'my_custom)

(require 'my-llm-copilots)




;; disable right alt to be able to type ~ | etc.
(setq ns-right-alternate-modifier nil)

