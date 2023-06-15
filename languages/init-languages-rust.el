;; load Rust mode
(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil))))

(provide 'init-languages-rust)
