;;Basic settings
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-hl-line-mode 1)
(delete-selection-mode 1)
(global-display-line-numbers-mode 1)
(desktop-save-mode 1)
(global-visual-line-mode 1)
(global-tree-sitter-mode)
(setq visible-bell 1)
(setq mode-require-final-newline nil)
;; (unless (equal 'fullscreen 'fullboth)
;;   (toggle-frame-fullscreen))

;;Org mode
(setq org-startup-indented t)
(add-to-list 'load-path (file-name-as-directory "/home/kalin/.emacs.d/replace-colorthemes/"))

(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(use-package timu-spacegrey-theme)

(load-theme 'timu-spacegrey t)
;; (load-theme 'cobalt t)

;;pdf-tools
;; (use-package pdf-tools
;;   :ensure t)

;;magit
(use-package magit)

(use-package d2-mode)

(use-package undo-tree
  :init
  (global-undo-tree-mode))

;;helm
(use-package helm
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-s") #'helm-occur)
  (global-set-key (kbd "M-y") #'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") #'helm-mini)
  (global-set-key (kbd "C-c h") #'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (helm-mode 1))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)'
(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)
(setq helm-semantic-fuzzy-match t
      helm-imeny-fuzzy-match t)

(global-set-key (kbd "C-x k") #'kill-this-buffer)

;;yasnippet
(use-package yasnippet
  :init
  (yas-global-mode 1))

;;which-key
(use-package which-key
  :init
  (which-key-mode))

(use-package elpy
  :init
  (elpy-enable))

(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  :init
  (global-company-mode))

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook 'eglot-ensure))

(use-package tsx-mode
  :straight '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))


;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; ;;lsp-mode
;; (use-package lsp-mode
;;   :ensure t
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (c++-mode . lsp)
;; 	 (c-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; (use-package helm-lsp
;;   :ensure t
;;   :commands helm-lsp-workspace-symbol)

;; (use-package lsp-treemacs
;;   :ensure t
;;   :commands lsp-treemacs-errors-list)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode
;;   :ensure t)

;; (require 'dap-lldb)
;; ;; (use-package dap-lldb
;; ;;   :ensure t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package projectile
  :ensure t
  :config (projectile-mode +1)
  :custom ((projectile-completion-system 'helm))
  :bind
  ("C-c p" . projectile-command-map)
  ;; :init
  ;; (when (file-directory-p "~/Projects/Code")
  ;;   (setq projectile-project-search-path '("~/Projects/Code")))
  ;; (setq projectile-switch-project-action #'projectile-dired)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(quelpa-use-package quelpa tsx-mode tree-sitter-langs rust-mode d2-mode undo-tree timu-spacegrey-theme cobalt-theme projectile dap-lldb dap-mode lsp-treemacs helm-lsp lsp-ui lsp-mode magit elpy use-package org-ref gruvbox-theme))
 '(python-interpreter "python3")
 '(python-shell-interpreter "python3")
 '(python-shell-interpreter-args ""))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:extend t :background "#45672c"))))
 '(linum ((t (:background "#2e3d39" :foreground "steel blue")))))
(put 'dired-find-alternate-file 'disabled nil)
