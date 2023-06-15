(use-package yaml-mode
  :ensure t
  :init
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(provide 'init-languages-yaml)
