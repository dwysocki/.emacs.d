(defconst supported-languages
  '(clojure
    git
    go
    json
    markdown
    python
    rust
    terraform
    yaml
    R
    TeX))

(dolist (language supported-languages)
  (if (options-enabled-p 'languages language)
    (let ((filename (format "init-languages-%s" language)))
      (message "Loading language: %s" language)
      (require (intern filename)))
    (message "Skipping language: %s" language)))

(provide 'init-languages)
