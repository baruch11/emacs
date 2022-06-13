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
 '(elpy-shell-echo-output nil)
 '(global-display-line-numbers-mode nil)
 '(line-number-mode t)
 '(package-selected-packages
   '(elpy magit-gerrit magit conda anaconda-mode minimap flymake-flycheck jedi-direx pyvenv pyenv-mode jedi flymake-python-pyflakes flymake-proselint flycheck solarized-theme exotica-theme))
 '(scroll-bar-mode nil)
 '(show-paren-mode 1)
 '(tool-bar-mode nil))


(add-hook 'python-mode-hook 'flycheck-mode)


;; Standard Jedi.el setting
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c g") 'list-matching-lines)


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
