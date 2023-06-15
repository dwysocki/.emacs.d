;; -*-lisp-*-
;;
;; init.el

(dolist (dirname '("utils" "languages"))
  (add-to-list 'load-path (expand-file-name dirname user-emacs-directory)))

(require 'init-utils)

;;
;; -- Startup --
;;

;; hide welcome screen
(setq inhibit-startup-message t)

;; custom scratch buffer message
(setq initial-scratch-message
      (with-temp-buffer
        (insert-file-contents "~/.emacs.d/logo.txt")
        (buffer-string)))


;;
;; -- UI Layout --
;;

;; hide menu bar
(menu-bar-mode -1)

;; hide toolbar
(and (boundp 'tool-bar-mode)
     (tool-bar-mode -1))

;; hide scrollbar
(and (boundp 'scroll-bar-mode)
     (scroll-bar-mode -1))


;; display line and column numbers
(line-number-mode)
(column-number-mode)


;;
;; -- global keybindings --
;;

;; switch focus to the next window
(global-set-key (kbd "C-;")
  'other-window)
;; switch focus to the previous window
(global-set-key (kbd "C-:")
  'prev-window)
;; split the focused window into 4
(global-set-key (kbd "C-x 4 4")
  'split-window-4-way)
;; save a link for inserting in org-mode
(define-key global-map "\C-c l" 'org-store-link)
;; open the org-mode global agenda
(define-key global-map "\C-c a" 'org-agenda)
;; open Magit status menu
(global-set-key (kbd "C-c m")
  'magit-status)
;; replace all instances of the provided string
(global-set-key (kbd "C-c C-r")
  'replace-string)
;; replace all instances of the provided regexp
(global-set-key (kbd "C-c M-r")
  'replace-regexp)


;;
;; -- package management --
;;

;; bootstrap `package'
(require 'package)
(setf package-archives
  '(("gnu"       . "http://elpa.gnu.org/packages/")
    ("melpa"     . "http://melpa.org/packages/")))
(package-initialize)


;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; set up org-mode
(use-package org
  :ensure t
  :init
  (setf org-log-done t)
  (setf org-agenda-files
        (file-expand-wildcards "~/org/*.org")))

(use-package pabbrev
  :ensure t)

;; load Avy
(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0)))

(require 'init-languages)

;; load magit, the Emacs TUI for Git
(use-package magit
  :ensure t
  :config
  ;; hide magit warnings up to 1.4.0
  (setq magit-last-seen-setup-instructions "1.4.0"))


;; load paredit mode, which keeps parentheses balanced, and allows for easy
;; manipulation of S-expressions
(use-package paredit
  :ensure t
  :init
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code." t)
  ;; add hook for all lisp dialect modes
  (dolist (hook '(clojure-mode-hook
                  emacs-lisp-mode-hook
                  eval-expression-minibuffer-setup-hook
                  ielm-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  scheme-mode-hook))
    (add-hook hook #'enable-paredit-mode)))


;; load local files
(add-to-list 'custom-theme-load-path
             (expand-file-name "~/.emacs.d/themes/"))

;; load theme
(load-theme 'base16-eighties-dark t)


;;
;; -- doc-view-mode settings --
;;

; makes scrolling past the end of a page jump to the next page
(setf doc-view-continuous t)

;;
;; -- formatting --
;;

;; tabs are evil
(setq-default indent-tabs-mode nil)

;; set tab width to 4
(setq-default tab-width 4)

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; whitespace-mode
(require 'whitespace)

;; set up options for whitespace mode
(setq whitespace-style '(face empty tabs lines-tail trailing))

(defun enable-whitespace-mode ()
  "Enables whitespace-mode."
  (whitespace-mode t))

;; enable whitespace-mode only in certain modes
(dolist (hook '(c-mode-hook
                clojure-mode-hook
                java-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                python-mode-hook))
  (add-hook hook #'enable-whitespace-mode))




(defun prev-window (&optional count)
  "Switches focus to the previous window. Opposite of `other-window'."
  (interactive)
  (unless count (setq count 1))
  (other-window (- count)))

(defun split-window-4-way ()
  "Splits the current window into 4."
  (interactive)
  (let (;; remember the window we started at, so we can return to it
        (starting-window (selected-window)))
    ;; split the original window into top & bottom halves
    (split-window-below)
    ;; split the top half into a left & right half
    (split-window-right)
    ;; switch focus to the bottom half
    (other-window 2)
    ;; split the bottom half into a left & right half
    ;; we have now split the initial window into 4
    (split-window-right)
    ;; ensure that the 4 windows are the same size
    (balance-windows starting-window)
    ;; return to initial window
    (select-window starting-window)))


;;
;; -- Terminals --
;;

(defmacro make-term-command (program)
  "Defines a function which calls ansi-term with the given program name,
  and names the buffer *<program>-term*."
  (let* ((program-term (concat program "-term"))
         (fn-name (intern program-term)))
    `(defun ,fn-name ()
       "Docstring goes here..."
       (interactive)
       (ansi-term ,program ,program-term))))

;; Bash terminal
(make-term-command "bash")
(global-set-key (kbd "C-c M-f b") 'bash-term)
;; IPython terminal
(make-term-command "ipython")
(global-set-key (kbd "C-c M-f p") 'ipython-term)
;; Maxima terminal
(make-term-command "maxima")
(global-set-key (kbd "C-c M-f m") 'maxima-term)
;; R terminal
(make-term-command "R")
(global-set-key (kbd "C-c M-f r") 'R-term)
;; SBCL terminal
(make-term-command "sbcl")
(global-set-key (kbd "C-c M-f s") 'sbcl-term)


;;
;; -- Rmarkdown --
;;

(defun Rmarkdown-compile-silent ()
  "Compile file being visited with Rmarkdown, suppressing its output."
  (interactive)
  (shell-command
    (format (concat "echo 'rmarkdown::render(\"%s\", \"all\")'"
                    "|"
                    "R --no-save --silent > /dev/null")
            (buffer-file-name))))

(defun Rmarkdown-compile-verbose ()
  "Compile file being visited with Rmarkdown, writing its output to the
   minibuffer."
  (interactive)
  (shell-command
    (format (concat "echo 'rmarkdown::render(\"%s\", \"all\")'"
                    "|"
                    "R --no-save --silent")
            (buffer-file-name))))

;;
;; -- AI --
;;

;; (use-package chatgpt
;;   :straight (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el"))
;;   :init
;;   (require 'python)
;;   (setq chatgpt-repo-path "~/.emacs.d/straight/repos/ChatGPT.el/")
;;   :bind ("C-c q" . chatgpt-query))


;;
;; -- OS X specific --
;;

(when (eq system-type 'darwin)
  ;; provide the path to ispell
  (setf ispell-program-name "/usr/local/bin/ispell")
  ;; add TeXbin to the PATH environment variable
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/Library/TeX/texbin/"))
  ;; add /usr/local/bin and /Library/TeX/texbin to the exec-path variable
  (setf exec-path (append exec-path '("/usr/local/bin" "/Library/TeX/texbin/"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(avy terraform-mode yaml-mode use-package python-mode paredit pabbrev markdown-mode magit json-mode go-mode git-modes ess cider auctex))
 '(terraform-indent-level 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
