;; Loader - set home dir
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         (concat user-emacs-directory "elisp/" ))
        ((boundp 'user-init-directory)
         (concat user-init-directory "elisp/" ))
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "local.el")

;;-------------------------------------------------------------
;;-- Packages
;; ------------------------------------------------------------
;; Packages
(require 'package)

(package-initialize)
(setq my-packages '(paredit
		    cider
		    dash ;; cider depends on these
		    ;; pkg-info ;; cider depends on these
		    color-theme
		    expand-region  ;; Coole package intellij
        better-defaults
        material-theme
		    elpy
		    projectile
        magit
		    flx-ido
        js2-mode
        ac-js2
		    rainbow-delimiters
        web-beautify
        emojify))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-enable-at-startup t)
;; See if some packages needs reinstalling
;; (if (some (lambda (x) (not (package-installed-p x))) my-packages))
(when (not package-archive-contents) (package-refresh-contents))

(dolist (p my-packages)
   (when (not (package-installed-p p))
     (package-install p)))


;; -----------------------------------------------------------
;; -- Autosave in one folder
;; -----------------------------------------------------------
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))



;; ------------------------------------------------------------
;; -- CONFIG MODES
;; ------------------------------------------------------------
;;; turn on syntax hilighting
(global-font-lock-mode 1)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

;; emacs and Python
(elpy-enable)
(setq python-shell-virtualenv-path "/Users/wcoessen/anaconda3/envs/sandbox/bin/python")
(setenv "WORKON_HOME" "/Users/wcoessen/anaconda3/envs")

;; to many rope bugs...
(setq elpy-rpc-backend "jedi")

;; projectile
(projectile-global-mode)

;; flx-ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; ORG mode
(setq org-directory "/Users/wcoessen/Archive/org")

;;; Global keys for org mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; capture mode
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/wardgtd.org") "Tasks")
             "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
             "* %?\nEntered on %U\n  %i\n  %a")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

;; Add this file to the agenda
(setq org-agenda-files (mapcar (lambda (x) (concat org-directory x )) '("/agenda.org" "/wardgtd.org" )))

;; Markdown Mode
(add-to-list 'load-path "~/.emacs.d/elisp/ext/markdown-mode")
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-hook 'slime-mode-hook 'show-paren-mode)
(add-hook 'slime-mode-hook 'paredit-mode)
;;;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq slime-contribs '(slime-fancy))

;; CIDER
(add-to-list 'load-path "~/.emacs.d/elisp/ext/cider")
;; (require 'cider)
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'show-paren-mode)
(add-hook 'cider-repl-mode-hook 'show-paren-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
;; (add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'cider-repl-mode-hook 'company-mode)


;; j2-mode and swank to have cider-like repl whith JS
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

;; Windows shortcuts support
;; (add-to-list 'load-path "~/.emacs.d/elisp/ext")
;; (require 'w32-symlinks)

;; color-scheme
(load-theme 'material t)

;; No toolbar please
(tool-bar-mode -1)
;; No menubar please
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; global line numbers
;; (global-linum-mode )
 
;; ------------------------------------------------------------------------
;; Keyboard Shortcuts  
;; ------------------------------------------------------------------------

;; Run current file
(global-set-key (kbd "<f11>") 'run-current-file)
(global-set-key (kbd "C-c s") 'slime-selector)
(global-set-key (kbd "<f8>") 'magit-status)
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "<end>") 'move-end-of-line)


;; Winner mode saves the configuration of windws. I can toggle previous 
;; configurations with C-c left|right
(when (fboundp 'winner-mode)
	       (winner-mode 1))


;; This cool mode lets met jump buffers with shift-arrows!
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; function to add dir to execute path
(defun add-path-to-exec-dir (dir)
  "Adds the dirs to the exec-path and PATH env var"
  (push dir exec-path)
  (setenv "PATH"
          (concat (getenv "PATH") ":"
                  dir)))

(setq exec-dirs '("/usr/local/bin/" "/Applications/Racket v6.6/bin" "/Users/wcoessen/bin"))

(mapc 'add-path-to-exec-dir exec-dirs )

(defmacro after (mode &rest body)
  `(eval-after-load ,mode
     '(progn ,@body)))

(after 'auto-complete
       (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
       (setq ac-use-menu-map t)
       (define-key ac-menu-map "\C-n" 'ac-next)
       (define-key ac-menu-map "\C-p" 'ac-previous))

(after 'auto-complete-autoloads
       (autoload 'auto-complete-mode "auto-complete" "enable auto-complete-mode" t nil)
       (add-hook 'python-mode-hook
                 (lambda ()
                   (require 'auto-complete-config)
                   (add-to-list 'ac-sources 'ac-source-ropemacs)
                   (auto-complete-mode))))

;; (setenv "PYTHONUNBUFFERED" "x") ;; Solves an issue with Python 3 on
;; windows, where the output is lagging behind...

(setq inhibit-startup-message t)


;; Done, start listening
(server-start)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-rpc-python-command "/Users/wcoessen/anaconda3/bin/python")
 '(geiser-active-implementations (quote (racket)))
 '(geiser-racket-binary "racket")
 '(pyvenv-virtualenvwrapper-python "/Users/wcoessen/anaconda3/bin/python")
 '(pyvenv-workon "sandbox"))

(find-file "~/.emacs.d/init.el")

(cd "/Users/wcoessen")
(setq default-tab-width 2)

(add-hook 'after-init-hook #'global-emojify-mode)
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)
(set-face-attribute 'default nil :height 140)

(defun pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          )))

(add-hook 'scheme-mode-hook 'pretty-lambda)
(add-hook 'emacs-lisp-mode 'pretty-lambda)
(add-hook 'lisp-mode 'paredit-mode)

(global-prettify-symbols-mode 1)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-?") 'mc/mark-next-like-this)
