
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;(require 'org-install)
;;(require 'ob-tangle)
;;(org-babel-load-file (expand-file-name "zilongshanren.org") user-emacs-directory)

(add-to-list 'load-path "~/.emacs.d/lisp")


(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el")
    )


(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-org)
(require 'yasnippet)
(require 'init-keybindings)
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(load-file custom-file)


;;(require 'smex) ; Not needed if you use package.el
;;(smex-initialize)
;;(global-set-key (kbd "M-x") 'smex)
;;  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
;;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)




(require 'hungry-delete)
(global-hungry-delete-mode)

;;(global-flycheck-mode t)
(add-hook 'js2-mode-hook 'flycheck-mode)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)
(evil-mode 1)
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(setq evil-want-C-u-scroll t)
(global-evil-leader-mode 1)

(evil-leader/set-key
  "ff" 'find-file
  "fr" 'recentf-open-files
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "4" 'select-window-4
  ":" 'counsel-M-x)  

(window-numbering-mode 1)
(require 'powerline)
(powerline-default-theme)
(require 'powerline-evil)

(require 'evil-surround)
(global-evil-leader-mode 1)
;;(define-key evil-normal-state-map (kbd "/") 'evilnc-comment-or-uncomment-lines)
;;(define-key evil-visual-state-map (kbd "/") 'evilnc-comment-or-uncomment-lines)
(evilnc-default-hotkeys)
(which-key-mode 1)

(dolist (mode '(ag-mode
		flycheck-error-list-mode
		git-rebase-mode))
  (add-to-list 'evil-emacs-state-modes mode))

(add-hook 'occur-mode-hook
	  (lambda ()
	    (evil-add-hjkl-bindings occur-mode-map 'emacs
	      (kbd "/") 'evil-search-forward
	      (kbd "n") 'evil-search-next
	      (kbd "N") 'evil-search-previous
	      (kbd "C-d") 'evil-scroll-down
	      (kbd "C-u") 'evil-scroll-up
	      ))
;;(electric-indent-mode 1)















