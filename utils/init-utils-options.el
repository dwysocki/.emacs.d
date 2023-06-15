(defconst options-default
  '((languages git)
    (languages json)
    (languages markdown)
    (languages python)
    (languages yaml)))

(defconst options-atto
  '((languages git)
    (languages go)
    (languages json)
    (languages markdown)
    (languages python)
    (languages terraform)
    (languages yaml)
    (languages TeX)))

(defconst options-femto
  '(languages))

(defun get-device-options ()
  (cond ((string= system-name "atto") options-atto)
        ((string= system-name "femto") options-femto)
        (t options-default)))

(defconst options (get-device-options))

(defun options-enabled-p (prefix subject)
  (or
    ;; Checks if prefix alone is contained in options
    (member prefix options)
    ;; Checks if prefix-subject pair is contained in options
    (member (list prefix subject) options)))

(provide 'init-utils-options)
