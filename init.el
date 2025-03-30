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

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-hl-line-mode 1)
(delete-selection-mode 1)
(global-display-line-numbers-mode 1)
(column-number-mode)
(global-visual-line-mode 1)
(add-to-list 'default-frame-alist '(alpha-background . 85))

(setq visible-bell 1
      mode-require-final-newline nil
      inhibit-startup-message t
      backup-directory-alist '(("." . "~/.emacs_backups"))
      undo-tree-history-directory-alist '(("." . "~/.emacs_undos"))
      eldoc-echo-area-prefer-doc-buffer t
      help-window-select t
      tab-always-indent 'complete)

(setq-default fill-column 80)

(global-set-key (kbd "C-x k") #'kill-current-buffer)
(global-set-key (kbd "C-c w") #'whitespace-mode)
(global-set-key (kbd "C-c s") #'window-swap-states)

(use-package spacious-padding
  :straight (:host github :repo "protesilaos/spacious-padding" :branch "main")
  :config
  (spacious-padding-mode 1))

(use-package ibuffer
  :straight (:type built-in)
  :config
  (defun custom-ibuffer-update (&optional arg)
    "A custom function for updating IBuffer, that also refreshes
filter groups"
    (interactive "P")
    (if arg
	  (ibuffer-update arg)
	(ibuffer-update nil))
    (ibuffer-set-filter-groups-by-mode))

  (define-key ibuffer-mode-map (kbd "g") #'custom-ibuffer-update)
  (global-set-key (kbd "C-X C-b") #'ibuffer)
  (add-hook 'ibuffer-mode-hook 'ibuffer-set-filter-groups-by-mode))

(use-package dired
  :straight (:type built-in)
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode) ;; make less verbose by
                                                       ;; default
  (define-key dired-mode-map (kbd "C-c w") #'wdired-change-to-wdired-mode))

(use-package timu-spacegrey-theme)
(load-theme 'timu-spacegrey t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :init
  (vertico-mode))

(use-package corfu
  :init
  (global-corfu-mode))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package dimmer
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  (dimmer-mode t)
  (setq dimmer-fraction 0.4))

(use-package d2-mode
  :config (add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-mode)))

(use-package markdown-mode)

(use-package org
  :straight (:type built-in)
  :config
  (setq org-startup-indented t
    org-todo-keywords '((sequence "TODO" "WIP" "WAITING" "|" "DONE" "OBSOLETE"))
    org-todo-keyword-faces '(("TODO" . "YellowGreen")
                             ("WIP" . "SteelBlue1")
                             ("WAITING" . "DarkOrange"))
    org-agenda-files '("~/org-roam/daily")
    org-time-stamp-custom-formats '("%a %d %b %Y %H:%M"))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((python . t)))
  (setq org-babel-python-command "python3"))


(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/org-roam")
        org-roam-capture-templates (append org-roam-capture-templates
                                           '(("p" "proc_anatomy" plain "%?"
                                              :target (file+head "proc_anatomy_kb/%<%Y%m%d%H%M%S>-${slug}.org"
                                                                 "#+title: ${title}\n")
                                              :unnarrowed t))))
  (org-roam-db-autosync-mode)


  :after (org))

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package magit)

(use-package multiple-cursors)

(use-package projectile
  :config (projectile-mode +1)
  :custom ((projectile-completion-system 'default))
  :bind
  ("C-c p" . projectile-command-map))

(use-package treesit
  :straight (:type built-in)
  :config
  (let* ((recipes '((c "https://github.com/tree-sitter/tree-sitter-c" nil nil nil nil)
                    (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" nil nil nil nil))
                    (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src/" nil nil)
                    (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src" nil nil)
                    (python "https://github.com/tree-sitter/tree-sitter-python" nil nil nil nil)
                    (json "https://github.com/tree-sitter/tree-sitter-json" nil nil nil nil)
                    (bash "https://github.com/tree-sitter/tree-sitter-bash" nil nil nil nil)
                    (rust "https://github.com/tree-sitter/tree-sitter-rust" nil nil nil nil)
                    (css "https://github.com/tree-sitter/tree-sitter-css" nil nil nil nil)
                    (html "https://github.com/tree-sitter/tree-sitter-html" nil nil nil nil)
                    (toml "https://github.com/tree-sitter/tree-sitter-toml" nil nil nil nil)
                    (yaml "https://github.com/ikatyang/tree-sitter-yaml" nil nil nil nil)
                    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile.git")
		    (nix "https://github.com/nix-community/tree-sitter-nix.git")
                    (wgsl "https://github.com/szebniok/tree-sitter-wgsl" nil nil nil nil)))
         (langs (mapcar #'car recipes)))

    (mapcar (lambda (arg) (add-to-list 'treesit-language-source-alist arg)) recipes)
    (mapcar (lambda (lang) (unless (treesit-language-available-p lang)
                             (treesit-install-language-grammar lang)))
            langs)))

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold"))

(use-package caddyfile-mode
  :ensure t
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))

(use-package typescript-ts-mode
  :config
  (setq typescript-ts-mode-indent-offset 4)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure))

(use-package wgsl-ts-mode
  :straight (wgsl-ts-mode :type git :host github :repo "acowley/wgsl-ts-mode")
  :config
  (add-hook 'wgsl-ts-mode 'turn-on-font-lock))

(use-package nix-mode
  :straight (nix-mode :type git :host github :repo "NixOS/nix-mode")
  :mode "\\.nix\\'")

(use-package rust-ts-mode
  :straight (:type built-in)
  :config
  (add-hook 'rust-ts-mode-hook 'eglot-ensure)
  :mode "\\.rs\\'")

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
