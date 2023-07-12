

(defun my/copy-buffer-file-name-as-kill ()
  "Copy the buffer-file-name to the kill-ring"
  (interactive)
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (setq new-kill-string name)
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

(provide 'my_custom)
