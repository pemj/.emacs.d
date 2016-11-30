(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;colorizing 
(add-to-list 'default-frame-alist '(foreground-color . "white"))
(add-to-list 'default-frame-alist '(background-color . "black"))

;;Adds the system name and full file path to emacs window
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
;;makes paren-matching a little quicker
(show-paren-mode 1)
(setq show-paren-delay 0)
;;shows the column number in the infobar thingy
(column-number-mode 1)
;;shows the line number next to the line
(global-linum-mode t)

;;enables powershell editing.
(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'powershell-mode)
;; this line automatically causes all files with the .ps1 or .psm1 extensions to
;; be loaded into powershell mode
(setq auto-mode-alist (append '(("\\.ps1$" . powershell-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.psm1$" . powershell-mode)) auto-mode-alist))


;;this enables ido
(require 'ido)
(ido-mode t)


;; this is to enable tuareg mode automatically for ocaml source files
(setq auto-mode-alist (cons '("\\.ml[iylp]?\\'" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'ocamldebug "ocamldebug" "Run the Caml debugger" t)

;;so that we have MELPA in the packages, that more packages will appear!
(when (> emacs-major-version 23)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives 
	       '("melpa" . "http://melpa.milkbox.net/packages/")
	       'APPEND))

;;set TrampMode to use SSH for its connections
(setq tramp-default-method "ssh")
(defun my-generate-tab-stops (&optional width max)
  "Return a sequence suitable for `tab-stop-list'."
  (let* ((max-column (or max 200))
         (tab-width (or width tab-width))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))

(setq tab-width 4)
(setq tab-stop-list (my-generate-tab-stops))
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode t)
	    (setq tab-width 4)
	    (setq python-indent 4)))




;; Haskell indentation mode goes here
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;;--------------------------------------------------------------------
;; Lines enabling gnuplot-mode

;; move the files gnuplot.el to someplace in your lisp load-path or

;; these lines enable the use of gnuplot mode
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

;; this line automatically causes all files with the .gp extension to
;; be loaded into gnuplot mode
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

;; This line binds the function-9 key so that it opens a buffer into
;; gnuplot mode 
(global-set-key [(f9)] 'gnuplot-make-buffer)

;; end of line for gnuplot-mode
;;--------------------------------------------------------------------






