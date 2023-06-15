;; load python mode, for hacking at python code
(use-package python-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook
    ;; turn on whitespace-mode
    #'enable-whitespace-mode))

(provide 'init-languages-python)
