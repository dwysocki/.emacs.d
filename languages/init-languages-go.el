(use-package go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save)
              (setq tab-width 4)
              (setq indent-tabs-mode 1))))

(provide 'init-languages-go)
