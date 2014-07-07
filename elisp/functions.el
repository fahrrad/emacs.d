;; Own functions
(defun run-current-file (arguments)
  "Execute the current file.
For example, if the current buffer is the file xx.py,
then it'll call “python xx.py” in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

If the file is emacs lisp, run the byte compiled version if exist."
  (interactive (list (read-string "Arguments: ")))
  (let* (
         (suffixMap
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "C:/Python27_32/python.exe")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
            ("js" . "node")             ; node.js
            ("sh" . "bash")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            )
          )
         (fName (buffer-file-name))
         (fSuffix (file-name-extension fName))
         (progName (cdr (assoc fSuffix suffixMap)))
	 (cmdStr (concat progName " \""   fName "\" "  arguments))
	 
         )
    

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
          (save-buffer) ) )

    (if (string-equal fSuffix "el") ; special case for emacs lisp
        (load (file-name-sans-extension fName))
      (if progName
          (progn
            (message "Running…")
            (shell-command cmdStr "*run-current-file output*" )
            )
        (message "No recognized program file suffix for this file.")))))

(defun installed-or-install-package (package-name)
  "looks if a package is installed, and if not, tries to install it"
  (when (not (package-installed-p package-name))
    (package-install package-name)))

(defun installed-or-install (package-list)
  "Looks at each package in the package-list. If installed, skip. Else, try to
install it")

(defun replace-last-sexp-with-evaluation ()
  "A bit like c-m e except, it replaces the sexp with the result. handy for
on the fly calculations"
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))
