;; https://macowners.club/posts/email-emacs-mu4e-macos/
;; load mu4e from the installation path.
;; yours might differ check with the Emacs installation
(use-package mu4e
  :straight nil
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


;; (setq mu4e-contexts
;;       `((make-mu4e-context
;;           :name "yahoo"
;;           :enter-func
;;           (lambda () (mu4e-message "Enter dummy@example.de context"))
;;           :leave-func
;;           (lambda () (mu4e-message "Leave dummy@example.de context"))
;;           :match-func
;;           (lambda (msg)
;;             (when msg
;;               (mu4e-message-contact-field-matches msg
;;                                                   :to "charprat@yahoo.fr")))
;;           :vars '((user-mail-address . "charprat@yahoo.fr")
;;                   (user-full-name . "Dummy McDummerson")
;;                   ;; check your ~/.maildir to see how the subdirectories are called
;;                   ;; e.g `ls ~/.maildir/example'
;;                   (mu4e-drafts-folder . "/yahoo_gmail/[Gmail]/Brouillons")
;;                   (mu4e-refile-folder . "/yahoo_gmail/Archive")
;;                   (mu4e-sent-folder   . "/yahoo_gmail/[Gmail]/Messages envoy&AOk-s")
;;                   (mu4e-trash-folder  . "/yahoo_gmail/[Gmail]/Corbeille")))))

;; (setq mu4e-context-policy 'pick-first) ;; start with the first (default) context;
;; (setq mu4e-compose-context-policy 'ask) ;; ask for context if no context matches;
(setq mu4e-trash-folder "/yahoo_gmail/[Gmail]/Corbeille")
(setq mu4e-sent-folder "/yahoo_gmail/[Gmail]/Messages envoy&AOk-s")
(setq mu4e-drafts-folder "/yahoo_gmail/[Gmail]/Brouillons")
