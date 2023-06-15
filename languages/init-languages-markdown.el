;; load markdown mode
(use-package markdown-mode
  :ensure t
  :init
  (add-hook 'markdown-mode-hook
            (lambda ()
              ;; add keyboard shortcuts for compiling
              (define-key markdown-mode-map
                (kbd "C-c C-c C-c") 'Rmarkdown-compile-silent)
              (define-key markdown-mode-map
                (kbd "C-c C-c C-v") 'Rmarkdown-compile-verbose))))

(provide 'init-languages-markdown)
