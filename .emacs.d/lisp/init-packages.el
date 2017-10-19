(when (>= emacs-major-version 24)
     (require 'package)
     (package-initialize)
     (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
     ;;(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
     ;;			      ("melpa" . "http://elpa.emacs-china.org/melpa/")))
     )

(require 'cl)

 ;; Add Packages
 (defvar my/packages '(
                company
                hungry-delete
                swiper
                counsel
                smartparens
                js2-mode
                nodejs-repl
                exec-path-from-shell
		popwin
		reveal-in-osx-finder
		web-mode
		js2-refactor
		expand-region
		iedit
		org-pomodoro
		helm-ag 
		flycheck
		auto-yasnippet
                monokai-theme
		evil
		evil-leader
		window-numbering
		evil-surround
		evil-nerd-commenter
		which-key
		powerline
		powerline-evil
		ivy
		smartparens
		;; my own packages
                solarized-theme
		helm
		helm-gtags
		ggtags
		;;for python
		ein 
		elpy
		py-autopep8
		websocket
		request
		dash
		s
		skewer-mode
		request-deferred
		smartrep
		org
		magit 
		;;for latex
		auctex
		auctex-latexmk
		auctex-lua
		company-auctex
		cdlatex
		latex-preview-pane
		
                ) "Default packages")

 (setq package-selected-packages my/packages)

 (defun my/packages-installed-p ()
     (loop for pkg in my/packages
           when (not (package-installed-p pkg)) do (return nil)
           finally (return t)))

 (unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
         (package-install pkg))))

 ;; Find Executable Path on OS X
 (when (memq window-system '(mac ns))
   (exec-path-from-shell-initialize))

(global-hungry-delete-mode)
(require 'smartparens-config)

(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(smartparens-global-mode t)

(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

(load-theme 'monokai t)
;; config js2-mode for js files
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode)
	 ("\\.html\\'" . web-mode))
       auto-mode-alist))

(global-company-mode t)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")

(defun my-web-mode-indent-setup ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'my-web-mode-indent-setup)

(defun my-toggle-web-indent ()
 (interactive)
  (if (or (eq major-mode 'js-mode) (eq major-mode 'js2-mode))
      (progn
	(setq js-indent-level (if (= js-indent-level 2) 4 2))
	(setq js2-basic-offset (if (=js2-basic-offset 2) 4 2))))
    (if (eq major-mode 'web-mode)
	(progn (setq web-mode-markup-indent-offset (if (= web-mode-markup-indent-offset 2) 4 2))
	       (setq web-mode-css-indent-offset (if (= web-mode-css-indent-offset 2) 4 2))
	       (setq web-mode-code-indent-offset (if (= web-mode-code-indent-offset 2) 4 2))))
    (if (eq major-mode 'css-mode)
	(setq css-indent-offset (if (= js-indent-offset 2) 4 2)))
    
    (setq indent-tabs-mode nil))

  (global-set-key (kbd "C-c t i") 'my-toggle-web-indent)
(require 'popwin)
(popwin-mode t)

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

(global-set-key (kbd "M-s o") 'occur-dwim)
(global-set-key (kbd "M-s i") 'counsel-imenu)

(defun js2-imenu-make-index ()
      (interactive)
      (save-excursion
        ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
        (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("test" "\\s-*test\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
                                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                                   ("Function" "^var[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*()[ \t]*{" 1)
                                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)
                                   ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))
(add-hook 'js2-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'js2-imenu-make-index)))

(global-set-key (kbd "M-s i") 'counsel-imenu)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-s e") 'iedit-mode)
(require 'org-pomodoro)
(elpy-enable)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))
(elpy-use-ipython)
(require 'ein)
(require 'ein-loaddefs)
(require 'ein-notebook)
(require 'ein-subpackages)


(load "auctex.el" nil t t)
;;(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-selft t)
(setq TeX-save-query nil)
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 
	  (lambda()
	    (turn-off-auto-fill)
	    (outline-minor-mode 1)
	    (setq TeX-view-program-list '(("skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
	    (setq TeX-view-program-selection
		  '((output-pdf "skim")))
	    (setq TeX-global-PDF-mode t
		  TeX-engine 'xetex)
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (setq TeX-command-default "XeLaTeX")
  (setq TeX-save-querynil )
  (setq TeX-show-compilation t)
))


(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(autoload 'reftex-mode "reftex" "RefTeXMinorMode" t)
(autoload 'turn-on-reftex "reftex" "RefTeXMinorMode" nil)
(autoload 'reftex-citation "reftex-cite" "Makecitation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrasemode" t)
(latex-preview-pane-enable)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
(autoload 'cdlatex-mode "cdlatex" "CDLaTeXMode" t)
(autoload 'turn-on-cdlatex "cdlatex" "CDLaTeXMode" nil)
(provide 'init-packages)
