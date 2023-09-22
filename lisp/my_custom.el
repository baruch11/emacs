

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


(defun my/add-current-dir-to-pythonpath ()
  "Add the current directory to the env variable PYTHONPATH."
  (interactive)
  (setenv "PYTHONPATH" (concat (getenv "PYTHONPATH") ":" default-directory)))


(provide 'my_custom)
