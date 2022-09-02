(defun octave-sync-function-file-names () (ignore))
(setq visible-bell 1)

;; disable right alt to be able to type ~ | etc.
(setq ns-right-alternate-modifier nil)

;; display file path in title bar
(setq frame-title-format
      '(buffer-file-name "%b - %f" ; File buffer
			 (dired-directory dired-directory ; Dired buffer
					  (revert-buffer-function "%b" ; Buffer Menu
	    ("%b - Dir: " default-directory))))) ; Plain buffer


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

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
(setq column-number-mode t)


(add-to-list 'load-path "~/.emacs.d/elpa/flymake-easy-20140818.755")

;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "/Users/charlesprat/miniconda3/")
 '(delete-selection-mode t)
 '(display-line-numbers nil)
 '(ein:output-area-inlined-images t)
 '(elpy-rpc-virtualenv-path 'current)
 '(elpy-shell-echo-output nil)
 '(global-display-line-numbers-mode nil)
 '(inhibit-startup-screen t)
 '(line-number-mode t)
 '(org-agenda-files
   '("/Users/charlesprat/RepoGit/emacs.org" "/Users/charlesprat/RepoGit/missiontransition/mt.org" "/Users/charlesprat/.emacs.d/misc_todo.org"))
 '(package-selected-packages
   '(company-quickhelp impatient-mode csv-mode markdown-preview-eww ox-reveal hide-mode-line org-tree-slide mu4e-overview markdown-preview-mode ein org-bullets use-package elpy magit-gerrit magit conda anaconda-mode minimap flymake-flycheck jedi-direx pyvenv pyenv-mode jedi flymake-python-pyflakes flymake-proselint flycheck solarized-theme exotica-theme))
 '(scroll-bar-mode nil)
 '(send-mail-function 'mailclient-send-it)
 '(show-paren-mode 1)
 '(tool-bar-mode nil)
 '(user-mail-address "charprat@yahoo.fr")
 '(warning-suppress-types '(((python python-shell-completion-native-turn-on-maybe)))))


(add-hook 'python-mode-hook 'flycheck-mode)


;; Standard Jedi.el setting
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot nil)


(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c g") 'list-matching-lines)
(global-set-key (kbd "C-c a") 'org-agenda)


;; from https://github.com/necaris/conda.el
(require 'conda)
;; if you want interactive shell support, include:
(conda-env-initialize-interactive-shells)
;; if you want eshell support, include:
(conda-env-initialize-eshell)
;; if you want auto-activation (see below for details), include:
(conda-env-autoactivate-mode t)


(setq conda-env-home-directory (expand-file-name "~/miniconda3/"))
(put 'magit-clean 'disabled nil)

(defun create-tags (dir-name)
     "Create tags file."
     (interactive "DDirectory: ")
     (eshell-command 
      (format "cd %s; find %s -type f -name \"*.py\" | etags -" dir-name dir-name)))

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt"))


(set-face-attribute 'default nil :height 140)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; enable elpy
(elpy-enable)
(put 'upcase-region 'disabled nil)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


;; load mu4e from the installation path.
;; yours might differ check with the Emacs installation
(use-package mu4e
  :load-path  "/usr/local/share/emacs/site-lisp/mu/mu4e/")
;; for sending mails
(require 'smtpmail)

;; we installed this with homebrew
(setq mu4e-mu-binary (executable-find "mu"))

;; this is the directory we created before:
(setq mu4e-maildir "~/.maildir")
;; this command is called to sync imap servers:
(setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
;; how often to call it in seconds:
(setq mu4e-update-interval 300)
;; save attachment to desktop by default
;; or another choice of yours:
(setq mu4e-attachment-dir "~/Desktop")
;; rename files when moving - needed for mbsync:
(setq mu4e-change-filenames-when-moving t)
;; list of your email adresses:
(setq mu4e-user-mail-address-list '("charprat@yahoo.fr"))


;; gpg encryptiom & decryption:
;; this can be left alone
(require 'epa-file)
(epa-file-enable)
(setq epa-pinentry-mode 'loopback)
(auth-source-forget-all-cached)

;; don't keep message compose buffers around after sending:
(setq message-kill-buffer-on-exit t)

;; send function:
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it)

;; send program:
;; this is exeranal. remember we installed it before.
(setq sendmail-program (executable-find "msmtp"))

;; select the right sender email from the context.
(setq message-sendmail-envelope-from 'header)

;; mu4e cc & bcc
;; this is custom as well
(add-hook 'mu4e-compose-mode-hook
          (defun timu/add-cc-and-bcc ()
            "My Function to automatically add Cc & Bcc: headers.
    This is in the mu4e compose mode."
            (save-excursion (message-add-header "Cc:\n"))
            (save-excursion (message-add-header "Bcc:\n"))))

;; mu4e address completion
(add-hook 'mu4e-compose-mode-hook 'company-mode)
