;;
;; -- Clojure --
;;
;; Packages for working with Clojure

;; load clojure mode, for hacking at clojure code
(use-package clojure-mode
  :ensure t)

;; load cider, for interactive clojure hacking
(use-package cider
  :ensure t)

(provide 'init-languages-clojure)
