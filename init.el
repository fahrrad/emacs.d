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

;;-------------------------------------------------------------
;;-- Packages
;; ------------------------------------------------------------
(require 'package)
(package-initialize)
(setq my-packages '(paredit
		      color-theme
                      cider
		      icicles))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-enable-at-startup t)

;; See if some packages needs reinstalling
;; (if (some (lambda (x) (not (package-installed-p x))) my-packages))
(when (not package-archive-contents) (package-refresh-contents))

(dolist (p my-packages)
   (when (not (package-installed-p p))
     (package-install p)))


;; ------------------------------------------------------------
;; -- CONFIG MODES
;; ------------------------------------------------------------

;;; turn on syntax hilighting
(global-font-lock-mode 1)

;; Clojure mode
(add-hook 'cider-mode-hook 'paredit-mode)

;; SLIME
(add-to-list 'load-path "~/.emacs.d/elisp/ext/slime")
(require 'slime-autoloads)
(setq inferior-lisp-program "sbcl")
(setenv "SBCL_HOME" "/Users/wardcoessens/Applications/lib/sbcl")
(add-hook 'slime-mode-hook 'show-paren-mode)
(add-hook 'slime-mode-hook 'paredit-mode)
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq slime-contribs '(slime-fancy))


(icy-mode 1)

;; color-scheme
(require 'color-theme)
(color-theme-initialize)
(color-theme-standard)

;; No toolbar please
(tool-bar-mode -1)
;; No menubar please
(menu-bar-mode -1)
 
;; ------------------------------------------------------------------------
;; Keyboard Shortcuts  
;; ------------------------------------------------------------------------

;; Run current file
(global-set-key (kbd "<f11>") 'run-current-file)
(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key (kbd "C-<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "C-<end>") 'end-of-buffer)
(global-set-key (kbd "C-c s") 'slime-selector)v


;; Winner mode saves the configuration of windws. I can toggle previous 
;; configurations with C-c left|right
(when (fboundp 'winner-mode)
	       (winner-mode 1))


;; This cool mode lets met jump buffers with shift-arrows!
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; function to add folder to executable path, and to environment var
(defun add-to-exec-path (dir)
  (push dir exec-path)
  (setenv "PATH" (concat dir ":" (getenv "PATH"))))
(add-to-exec-path "~/Applications/bin")


;; Done, start listening
(server-start)




