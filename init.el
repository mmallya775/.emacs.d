;; Emacs setup

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Refresh contents only if the list is empty
(unless package-archive-contents
  (package-refresh-contents))

;; Automatically download and install any package specified using the use-package macro
(setq use-package-always-ensure t)

;;-------------------------------------------------------------------
;; Gets rid of the annoying ~ or # backup files when working with emacs
(setq make-backup-files nil)
;; Prompt to delete autosaves when killing buffers.
(setf kill-buffer-delete-auto-save-files t)

;; Hide the unnecessary stuff from projectile
;; (with-eval-after-load 'projectile
;;   ;; Ignore common Clojure/LSP tooling dirs everywhere
;;   (dolist (dir '(".clj-kondo" ".lsp" ".cache" ".cpcache" ".shadow-cljs"
;;                  ".idea" ".vscode" "node_modules" "target" "build"))
;;     (add-to-list 'projectile-globally-ignored-directories dir))

;;   ;; If you have specific files you want ignored:
;;   (dolist (file '(".lsp-session-v1" ".DS_Store"))
;;     (add-to-list 'projectile-globally-ignored-files file)))

;;----------------------------------------------------------------
;; Mac Specific Options, uncomment when using on macOS

;; Make Command (⌘) the Meta key
(setq mac-command-modifier 'meta)
;; Option (⌥) no longer acts as Meta
(setq mac-option-modifier 'none)

;;-----------------------------------------------------------


;;-------------------------------------------------------------------
;;; Emacs customizing

;;; Disabling tooltip mode and let lsp-ui show docs on hover
(tooltip-mode -1)

;;; Fullscreen stuff
;; (set-frame-parameter nil 'fullscreen 'fullboth)
(add-to-list 'default-frame-alist '(fullscreen . maximized))



;; Prevent Line Wrapping
(setq-default truncate-lines t)

;; Enable Line Numbers
(global-display-line-numbers-mode)


;; Suppress native compiler warnings
(setq native-comp-async-report-warnings-errors 'silent)
(setq compilation-display-buffer-action '(nil . "*compilation*"))

;;;; Append the homebrew installed stuffs into the exec-path
;;;; otherwise emacs wouldn't see lein or npm installed from homebrew

;; (let ((path (getenv "PATH")))
;;   (setenv "PATH" (concat "/home/linuxbrew/.linuxbrew/bin:" path))
;;   (setq exec-path (split-string (getenv "PATH") path-separator)))


;; Use 'exec-path-from-shell' package for reliable path handling on macOS
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;;Disable toolbar on top
;; (ns-toggle-toolbar nil)
(tool-bar-mode -1)

;; scroll one line at a time (less "jumpy" than defaults)

;; (setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time


;;;----------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-operandi))
 '(custom-safe-themes
   '("8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "10e330880269244ae45ae9e02fe6f55766da9e15036e7c7f07d7ce228195deb5"
     "516ec39655c85f346393f5d93e0f03602b6bfc33335bf2fd673016c9c4cdc69e"))
 '(lsp-go-use-placeholders t nil nil "Customized with use-package lsp-mode")
 '(package-selected-packages
   '(all-the-icons-dired auto-package-update blacken browse-kill-ring
			 cape catppuccin-theme cider-hydra
			 clang-format clj-refactor
			 clojure-mode-extra-font-locking
			 cmake-font-lock cmake-ide company-box
			 company-prescient consult-lsp corfu-terminal
			 dap-mode dashboard deadgrep direnv docker
			 docker-compose-mode dockerfile-mode
			 doom-modeline doom-themes ef-themes
			 eval-sexp-fu exec-path-from-shell
			 expand-region flycheck git-gutter-fringe
			 go-mode kind-icon ligature lsp-pyright lsp-ui
			 magit-delta magit-todos marginalia
			 monokai-pro-theme monokai-theme orderless
			 paren-face py-isort python-docstring pyvenv
			 rainbow-delimiters rustic smartparens
			 solarized-theme surround symbol-overlay
			 toml-mode transpose-frame
			 treemacs-icons-dired treemacs-magit
			 treemacs-projectile undo-tree vertico
			 vterm-toggle yasnippet-snippets))
 '(safe-local-variable-values
   '((eval progn
	   (make-variable-buffer-local
	    'cider-jack-in-nrepl-middlewares)
	   (add-to-list 'cider-jack-in-nrepl-middlewares
			"shadow.cljs.devtools.server.nrepl/middleware")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;;------------------------------------------------------------
;; Add Cider for Clojure(script)

(use-package cider
  :ensure t
  :config
  (define-key cider-mode-map (kbd "C-M-x") 'cider-eval-dwim)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 3000)
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-repl-result-prefix ";; => ")
  (setq cider-repl-history-file "~/.cider-repl-history")
  (setq cider-eval-spinner-type 'vertical-breathing))


(use-package clojure-mode
  :ensure t
  :init
  ;; Disable auto ns template insertion (clojure-lsp will handle it)
  (setq clojure-insert-namespace-template nil)
  :config
  (define-clojure-indent
   (reg-sub '(1))
   (reg-event-db '(1))
   (reg-event-fx '(1))
   (reg-fx '(1))
   (reg-cofx '(1))))

;;<> Better sexp evals

(use-package eval-sexp-fu
  :ensure t)

;;<> Extra font locking
(use-package clojure-mode-extra-font-locking
  :ensure t)


;;; -------------------------------------------------------------
;;;; CLJ Refactor

(use-package clj-refactor
  :ensure t
  :hook (clojure-mode . clj-refactor-mode)
  :init
  (setq cljr-add-ns-to-blank-clj-files nil)
  (cljr-add-keybindings-with-prefix "C-c C-r"))

;;--------------------------------------------------------------------------------
;; LSP Mode setup

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  ;; set prefix for lsp-mode commands
  (setq lsp-keymap-prefix "C-c l"
	;; company-minimum-prefix-length 1
	lsp-enable-snippet t
	lsp-enable-indentation t
	lsp-file-watch-threshold 5000
	lsp-enable-on-type-formatting nil
	lsp-completion-no-cache nil
	;; lsp-enable-file-watchers t
	lsp-idle-delay 0.1
	lsp-headerline-breadcrumb-enable t
	lsp-signature-auto-activate nil
	lsp-completion-provider :none
	lsp-completion-enable-additional-text-edit t
        lsp-enable-completion-at-point t)
  :config
  (setq gc-cons-threshold (* 300 1024 1024)
                read-process-output-max (* 1024 1024))

  (define-key lsp-mode-map (kbd "C-c l r") #'lsp-workspace-restart)
  
  (setq lsp-file-watch-ignored-directories
	'("[/\\\\]\\.shadow-cljs\\'" "[/\\\\]\\.git\\'" "[/\\\\]\\.hg\\'" "[/\\\\]\\.bzr\\'" "[/\\\\]_darcs\\'" "[/\\\\]\\.svn\\'" "[/\\\\]_FOSSIL_\\'" "[/\\\\]\\.idea\\'" "[/\\\\]\\.ensime_cache\\'" "[/\\\\]\\.eunit\\'" "[/\\\\]node_modules" "[/\\\\]\\.fslckout\\'" "[/\\\\]\\.tox\\'" "[/\\\\]dist\\'" "[/\\\\]dist-newstyle\\'" "[/\\\\]\\.stack-work\\'" "[/\\\\]\\.bloop\\'" "[/\\\\]\\.metals\\'" "[/\\\\]target\\'" "[/\\\\]\\.ccls-cache\\'" "[/\\\\]\\.vscode\\'" "[/\\\\]\\.deps\\'" "[/\\\\]build-aux\\'" "[/\\\\]autom4te.cache\\'" "[/\\\\]\\.reference\\'" "[/\\\\]\\.lsp\\'" "[/\\\\]\\.clj-kondo\\'" "[/\\\\]\\.cpcache\\'" "[/\\\\]bin/Debug\\'" "[/\\\\]obj\\'"))
  :custom
        (lsp-gopls-staticcheck t)   ;; enable extra analysis
        (lsp-gopls-complete-unimported t)
        (lsp-gopls-use-placeholders t)
	(lsp-clients-clangd-args
	 '("--header-insertion=never"
	   "--cross-file-rename"
	   "--clang-tidy"
	   "--completion-style=detailed"))
	(lsp-clojure-custom-settings 
            '(:dependency-scheme "jar"
              :show-docs-arity-on-same-line? t))
   :hook ((clojure-mode . lsp)
	       (clojurescript-mode . lsp)
               (clojurec-mode . lsp)
               (lisp-mode . lsp)
	       (go-mode . lsp)
	       (rustic-mode . lsp)
	       (c-mode . lsp)
	       (c++-mode . lsp)
	       (cmake-mode . lsp)))

;; Disables the auto namespace so that the lsp takes care of it. Otherwise leading to two
;; namespace declarations
;; (with-eval-after-load 'clojure-mode
;;   (remove-hook 'clojure-mode-hook #'clojure-insert-ns-template))


;;<> LSP UI for improved references
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode) ;; always enable lsp-ui with lsp-mode
  :bind (("C-c r" . lsp-ui-peek-find-references))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-peek-enable t)
  (lsp-ui-sideline-enable t)
  (setq lsp-headerline-breadcrumb-enable-icons t))

;;<> Headerline icons
(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :config
  ;; Enable the breadcrumb with icons
  (lsp-headerline-breadcrumb-mode))


;;;------------------------------------------------------------
;; Paredit for parens

(use-package paredit
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'clojurescript-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  :bind (:map paredit-mode-map
              ("M-<right>" . paredit-forward-slurp-sexp)
              ("M-<left>" . paredit-forward-barf-sexp)
              ("C-M-<left>" . paredit-backward-slurp-sexp)
              ("C-M-<right>" . paredit-backward-barf-sexp)))



;;;------------------------------------------------------------
;; Company mode for the suggestions and completions (apparently corfu is better so commenting this out 2026-03-13)

;; (use-package company
;;   :ensure t
;;   :init
;;   ;; The :init keyword is for code that needs to run before the package is loaded.
;;   ;; We can enable global-company-mode here, as it's a very light operation.
;;   (global-company-mode 1)
;;   :custom
;;   ;; ;; Disabling the grammar autocomplete
;;   ;; (company-backends '(
;;   ;;                     (company-lsp company-yasnippet company-capf)
;;   ;;                     ))
;;   ;; The :custom keyword is perfect for setting variables
;;   (company-idle-delay 0)
;;   (company-minimum-prefix-length 1)
;;   (company-tooltip-align-annotations t)
;;   (company-selection-wrap-around t)
;;   (company-show-numbers t)
;;   (company-dabbrev-downcase nil)
;;   (company-require-match nil)
;;   :config
;;   ;; The :config keyword runs after the package has been loaded.
;;   ;; This is where you put package-specific function calls and keybindings.
;;   (when (fboundp 'company-tng-configure-default)
;;     (company-tng-configure-default))
;;   :bind
;;   ;; The :bind keyword provides a clean, declarative way to set keybindings.
;;   (:map company-active-map
;;         ("C-n" . company-select-next)
;;         ("C-p" . company-select-previous)
;;         ("<tab>" . company-complete-selection)
;;         ("TAB" . company-indent-or-complete-common)
;;         ("C-s" . company-filter-candidates)
;;         ("C-d" . company-show-doc-buffer))
;;   (:map global-map
;;         ("M-/" . company-complete)))

;; ;;;!!! Experimental stuff for better sorting and fuzziness
;; (use-package prescient
;;   :ensure t
;;   :config
;;   (prescient-persist-mode 1))

;; (use-package company-prescient
;;   :ensure t
;;   :config
;;   (company-prescient-mode 1))

;; ;;<> Better UI for company mode
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; ------------------------------------------------------------
;; CORFU - modern in-buffer completion
;; ------------------------------------------------------------

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)                    ;; auto popup
  (corfu-auto-delay 0.1)            ;; slight delay to avoid jitter while typing
  (corfu-auto-prefix 1)             ;; start completing after 1 char
  (corfu-cycle t)                   ;; wrap around candidates
  (corfu-preselect 'prompt)         ;; don't auto-select first candidate
  (corfu-quit-no-match 'separator)  ;; quit if no match
  (corfu-scroll-margin 3)
  :bind (:map corfu-map
              ("C-n"   . corfu-next)
              ("C-p"   . corfu-previous)
              ("<tab>" . corfu-insert)
              ("C-g"   . corfu-quit)
              ("M-d"   . corfu-show-documentation))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

;; Corfu in terminal (if you ever use Emacs -nw)
(use-package corfu-terminal
  :ensure t
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode +1))

;; Icons in the popup (replaces company-box icons)
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Cape - completion-at-point extensions (feeds extra sources into corfu)
;; Gives you file path completion, dabbrev, etc. on top of LSP
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :bind ("M-/" . cape-dabbrev))  ;; preserves your muscle memory for M-/

;; ---------------------------------------------------------------------------
;; Flycheck for syntax checking on the fly

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled new-line))
  (flycheck-idle-change-delay 0.5))

;; ;;<> clj-kondo support for flycheck

;; (use-package flycheck-clj-kondo
;;   :ensure t
;;   :after flycheck)


;;; ----------------------------------------------------------------------
;; Snippet related stuff

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;;; -------------------------------------------------------------------------
;;; Completion frameworks

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;;<> For better matching
(use-package orderless
  :ensure t
  :custom
  ;; Enable a completion style that allows for out-of-order,
  ;; space-separated fuzzy matching.
  (completion-styles '(orderless basic))
  ;; Fallback to basic completion if orderless doesn't find a match.
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion)))))
  )

(defun my/lsp-mode-setup-completion ()
  "Use orderless for LSP completion categories."
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))

(add-hook 'lsp-completion-mode-hook #'my/lsp-mode-setup-completion)

;;<> Adds annotations
(use-package marginalia
  :ensure t
  :after vertico
  :init
  ;; Turn on Marginalia, which provides annotations in the minibuffer.
  (marginalia-mode))


(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)
   ("C-x C-b" . consult-buffer)
   ("C-c s r" . consult-ripgrep)
   ("M-i" . consult-imenu)
   ("M-I" . consult-imenu-multi))
  :init
  ;; Set up Consult to use the Vertico preview feature.
  (setq completion-in-region-function
        #'consult-completion-in-region))

;;<> Consult-lsp
;; (use-package consult-lsp
;;   :ensure t
;;   :after (consult lsp-mode)) ;; references at point

;;;; --------------------------------------------------------------------------
;;; Magit to handle git stuff
(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status))

;; <> Syntax aware diffs
(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode))


;;; -------------------------------------------------------------------------
;; Which key package for keymaps
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))


;;;; --------------------------------------------------------------------------
;; Better icons

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (all-the-icons-dired-mode))


;;;; ------------------------------------------------------------------------
;;;; Doom modeline

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-file-name-style 'file-name-with-project))



;;;;; ----------------------------------------------------------------------
;;;; Projectile

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))




;;;; -------------------------------------------------------------------------
;; Add treemacs for better navigation

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (("C-c t" . treemacs)) ;; Toggle Treemacs
  :config
  ;; Optional quality-of-life tweaks
  (setq treemacs-width  40) ;; default width (change as desired)
  (setq treemacs-follow-after-init t)
  (setq treemacs-is-never-other-window t)
  (setq treemacs-recenter-after-file-follow t
        treemacs-recenter-after-tag-follow  t
        treemacs-is-never-other-window     nil))


(use-package treemacs-projectile    ;; Optional, if you use Projectile
  :after (treemacs projectile)
  :ensure t)


(use-package treemacs-magit         ;; Optional: integration with Magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config
  (treemacs-icons-dired-mode))

;;;; -----------------------------------------------------------------------
;;; Format on save using LSP


(defun my/clojure-lsp-format ()
  (when (and (derived-mode-p 'clojure-mode)
             (bound-and-true-p lsp-mode))
    (ignore-errors (lsp-format-buffer))))
;; Uncomment if you want auto-format:
;; (add-hook 'before-save-hook #'my/clojure-lsp-format)

(global-set-key (kbd "C-c f") #'my/clojure-lsp-format)



;;; -----------------------------------------------------------------------
;; Transpose buffers for easier buffer movements along window

(use-package transpose-frame
  :ensure t
  :bind ("C-x t" . transpose-frame))




;;; ---------------------------------------------------------------------
;;; Git gutter

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 5))

(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))



;; -------------------------------------------------------------------
;; A better dashboard for emacs

(use-package dashboard
  :ensure t
  :after all-the-icons
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                           (projects . 5)
                           (bookmarks . 5)
			   (agenda . 5)))
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-show-shortcuts t))


;;; --------------------------------------------------------------------
;;; Docker container management

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker-compose-mode
  :ensure t
  :mode ("docker-compose\\.yml\\'" . docker-compose-mode))


(use-package docker
  :ensure t
  :bind ("C-c d" . docker))



;;;; -------------------------------------------------------------------
;;; Rainbow Parens

(use-package rainbow-delimiters
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))


;;; Themes

(use-package monokai-pro-theme
  :ensure t)

(use-package doom-themes
  :ensure t)

(use-package monokai-theme
  :ensure t)


;;;; ---------------------------------------------------------------
;; Autoupdate packages

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 7) 
  (auto-package-update-maybe))


;;;;; ----------------------------------------------------------------
;;; Adjust font for a bigger screen

;;(set-face-attribute 'default nil :font "Monospace-15")




;;; -----------------------------------------------------------------
;; Org mode config

(use-package org
  :config
  (setq org-log-done 'time))

;; -----------------------------------------------------------------
;; Surrounding like vim surround

(use-package surround
  :ensure t
  :bind-keymap ("M-8" . surround-keymap))


;;-----------------------------------------------------------------
;; Consult LSP for navigation
(use-package consult-lsp
  :ensure t
  :after lsp-mode
  :bind (("C-c l s" . consult-lsp-symbols)))


;; ---------------------------------------------------------------
;; Hydra stuff

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-expand-region (:hint nil)
    "
^Expand Region^
---------------------------------------------------------
  _+_: expand     _-_: contract     _q_: quit
"
    ("+" er/expand-region)
    ("-" er/contract-region)
    ("q" nil :color red)))

(use-package cider-hydra
  :after cider
  :ensure t)



;; --------------------------------------------------------------
;; Nice ef-themes

(use-package ef-themes
  :ensure t)



;; ------------------------------------------------------------
;; Highlight current line

(use-package hl-line
  :hook (after-init . global-hl-line-mode))


;;-------------------------------------------------------------
;; winner mode for window layout recovery
(use-package winner
  :config
  (winner-mode 1))


;;------------------------------------------------------------
;; Expand region for better region selection in clojure

(use-package expand-region
  :ensure t
  :bind
  ;; Entry point: Control-Meta-Space (easy on German Mac layout)
  ("C-M-SPC" . hydra-expand-region/body))




;; ==============================================================
;; A bit of golang stuff below

;; Major mode for Go
(use-package go-mode
  :ensure t
  :hook ((before-save . gofmt-before-save)
	  (go-mode . (lambda ()
                      (setq tab-width 4)
                      (setq indent-tabs-mode t))))
  :config
  ;; Use goimports instead of gofmt if available
  (setq gofmt-command "goimports"))


;; (use-package gotest
;;   :ensure t
;;   :after go-mode
;;   :bind (:map go-mode-map
;;               ("C-c t t" . go-test-current-test)
;;               ("C-c t f" . go-test-current-file)
;;               ("C-c t p" . go-test-current-project)))

(with-eval-after-load 'dap-mode
  (require 'dap-dlv-go))

;; Smartparens for selected languages
(use-package smartparens
  :ensure t
  :hook ((go-mode . smartparens-mode)
	 (rustic-mode . smartparens-mode)
	 (c++-mode . smartparens-mode)
	 (c-mode . smartparens-mode)
	 (cmake-mode . smartparens-mode)
	 (python-mode . smartparens-mode))
  :config
  (require 'smartparens-config))



;; (use-package catppuccin-theme
;;   :ensure t
;;   :config
;;   ;; Choose one of the available flavors: 'latte, 'frappe, 'macchiato, or 'mocha
;;   (setq catppuccin-flavor 'macchiato)
;;   (load-theme 'catppuccin t))



;; ==============================================================
;; Rust programming setup

(use-package rustic
  :ensure t
  :config
  ;; Use rust-analyzer with lsp-mode
  (setq rustic-lsp-client 'lsp)
  ;; Format on save
  (setq rustic-format-on-save t)
  ;; Faster completions
  (setq lsp-idle-delay 0.2
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; Better rust-analyzer settings
  (setq lsp-rust-analyzer-display-chaining-hints t
	lsp-rust-analyzer-display-parameter-hints t
	lsp-rust-analyzer-display-closure-return-type-hints t
	lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
	lsp-rust-analyzer-cargo-watch-enable t
	lsp-rust-analyzer-proc-macro-enable t
	lsp-rust-analyzer-experimental-proc-attr-macros t
	lsp-rust-analyzer-import-enforce-granularity t
	lsp-rust-analyzer-import-prefix "by_self")
  ;; inline error display
  (setq lsp-ui-sideline-show-diagnostics t
      lsp-ui-sideline-show-hover nil  ;; hover noise gets old fast
      lsp-ui-sideline-show-code-actions nil)
  )

(with-eval-after-load 'dap-mode
  (require 'dap-cpptools))

;;--------------------------------------------------------------------
;; Highlighting for toml files

(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" . toml-mode))


;; ==============================================================

;; CMake mode for CMakeLists.txt and *.cmake
(use-package cmake-mode
  :ensure t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; Optional: CMake font-lock and extras
(use-package cmake-font-lock
  :ensure t
  :hook (cmake-mode . cmake-font-lock-activate))

;; LSP mode integration for C++
;; (use-package lsp-mode
;;   :ensure t
;;   :hook ((c++-mode . lsp)
;;          (c-mode . lsp)
;;          (cmake-mode . lsp))
;;   :custom
;;   (lsp-clients-clangd-args
;;    '("--header-insertion=never"
;;      "--cross-file-rename"
;;      "--clang-tidy"
;;      "--completion-style=detailed"))
;;   :config
;;   (setq lsp-prefer-flymake nil))

;; LSP UI (already loaded for other languages)
;; Will automatically enhance C++ hover docs, references, etc.

;; Company mode completions (already active globally)

;; ==============================================================
;; CMake project building and running

(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup)
  ;; Optional: automatically run cmake when opening a CMakeLists.txt
  (setq cmake-ide-run-cmake t))

;; ==============================================================
;; Code formatting

(use-package clang-format
  :ensure t
  :bind (("C-c C-f" . clang-format-buffer)
         ("C-c f" . clang-format-region))
  :hook ((c-mode c++-mode) . (lambda ()
                               (add-hook 'before-save-hook
                                         'clang-format-buffer nil t))))


(defun cmake-run ()
  "Automatically configure, build, and run the main executable for a CMake project."
  (interactive)
  (let* ((default-directory (locate-dominating-file buffer-file-name "CMakeLists.txt"))
         (build-dir (expand-file-name "build" default-directory))
         (cmake-cmd (format "cmake -S %s -B %s" default-directory build-dir))
         (build-cmd (format "cmake --build %s" build-dir))
         (exe (car (directory-files build-dir t "^[^.]+" t))) ;; first file (usually executable)
         (run-cmd (if (and exe (file-executable-p exe))
                      (format "%s" exe)
                    (format "echo 'No executable found in %s'" build-dir)))
         (full-cmd (format "%s && %s && %s" cmake-cmd build-cmd run-cmd)))
    (compile full-cmd)))


;; ==========================================================================
;;Fira code font
;; (set-face-attribute 'default nil
;;   :family "JetBrains Mono"
;;   :height 120
;;   :weight 'medium)


;; (use-package ligature
;;   :ensure t
;;   :config
;;   ;; Enable ligatures in programming modes
;;   (ligature-set-ligatures 'prog-mode
;;     '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
;;       "{-" "-}" "::" ":::" ":=" "!!" "!=" "!==" "-}"
;;       "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
;;       "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
;;       ".-" ".." "..." ".?" ".=" ".-" ".*"
;;       "\\\\" "\\\\\\" "\\/" "\\/-" "\\->"
;;       "/*" "*/" "/**" "//" "///" "//$"
;;       "==" "===" "=>" "=>>" "=/=" "=!=" "=>" ">=" ">-" ">=>" ">>" ">>-" ">>=" ">>>"
;;       "<$>" "<$" "<$$" "<*" "<*>" "<+>" "<-" "<--" "<->" "<!--" "<-->" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
;;       "<~" "<~~" "</" "</>" "</" "</>" "<|" "<|>" "<||" "<||>" "<:" "<::" "<:::"
;;       "</" "</>" "<!--" "<!" "<-!" "<->" "<-~" "<~" "<~~" "<$" "<$$" "<+>" "<*>"))
;;   (global-ligature-mode t))


;; === =======
;; Nice parens highlight
;; (use-package highlight-parentheses
;;   :ensure t
;;   :hook ((clojure-mode clojurescript-mode emacs-lisp-mode) . highlight-parentheses-mode)
;;   :config
;;   ;; Strong visible face - override theme
;;   (set-face-attribute 'highlight-parentheses-highlight nil
;;                       :weight 'bold
;;                       :underline t
;;                       :background "#3a3f5a")
;;   )

(use-package paren-face
  :ensure t
  :hook (prog-mode . paren-face-mode))

(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

(set-face-attribute 'show-paren-match nil
                    :weight 'ultra-bold
                    :background "#3a3f5a"
                    :foreground "#ffffff"
                    :underline t)


;; Nice markdown stuff
;; -------------------------------
;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  ;; Use GitHub-flavored markdown for README.md by default
  (setq markdown-command "pandoc"))


;; Nice python stuff
(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred



(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode)
  :custom
  (blacken-line-length 88))

(use-package py-isort
  :ensure t
  :hook (before-save . py-isort-before-save))

;; --- Virtualenv management -------------------------------------
;; Auto-activate venv when you enter a project (works great with projectile)
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

;; If you use direnv (recommended), Emacs will pick up env vars (incl venv)
(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package python
  :ensure nil
  :hook (python-mode . (lambda ()
                         (setq-local tab-width 4)
                         (setq-local python-indent-offset 4))))

;; Optional: docstring helpers, etc.
(use-package python-docstring
  :ensure t
  :hook (python-mode . python-docstring-mode))

;; --- Debugging: dap-mode (optional but powerful) ---------------
;; Works with lsp-mode and provides debugger UI.
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-python))

;; --- LSP Pyright tweaks ----------------------------------------
(with-eval-after-load 'lsp-pyright
  ;; Prefer your project venv; pyright will follow VIRTUAL_ENV / direnv
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-use-library-code-for-types t))


;; --- Nice highlighting stuff
(use-package symbol-overlay
  :ensure t
  :hook ((prog-mode . symbol-overlay-mode)
         (text-mode . symbol-overlay-mode))
  :bind (:map symbol-overlay-mode-map
              ;; highlight at point (sticky)
              ("C-c o h" . symbol-overlay-put)
              ;; remove highlight at point
              ("C-c o d" . symbol-overlay-remove)
              ;; remove all highlights
              ("C-c o c" . symbol-overlay-remove-all)
              ;; navigate between matches
              ("C-c o n" . symbol-overlay-jump-next)
              ("C-c o p" . symbol-overlay-jump-prev))
  :config
  ;; optional: nicer behavior
  (setq symbol-overlay-idle-time 0.3))


;; ------------------------------------------------------------------
;; Ace window for faster window jumping

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  (aw-dispatch-always nil))


;; ------------------------------------------------------------------
;; Deadgrep - better ripgrep frontend

(use-package deadgrep
  :ensure t
  :bind ("C-c s d" . deadgrep))


;; ------------------------------------------------------------------
;; Multiple cursors

(use-package multiple-cursors
  :ensure t
  :bind (("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-S-c C-S-c"   . mc/edit-lines)))


;; ------------------------------------------------------------------
;; Dimmer - dim inactive windows

;; (use-package dimmer
;;   :ensure t
;;   :custom
;;   (dimmer-fraction 0.3)
;;   (dimmer-adjustment-mode :foreground)
;;   (dimmer-use-colorspace :rgb)
;;   :config
;;   ;; Don't dim these buffers
;;   (dimmer-configure-which-key)
;;   (dimmer-configure-magit)
;;   (dimmer-configure-posframe)
;;   (dimmer-mode 1))

;; ------------------------------------------------------------------
;; Vterm - proper terminal inside Emacs

(use-package vterm
  :ensure t
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm %s"))

(use-package vterm-toggle
  :ensure t
  :after vterm
  :bind ("C-c v" . vterm-toggle)
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project)
  :config
  (setq vterm-toggle-cd-auto-create-buffer t))


;;; -----------------------------------------
;; Undo tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history"))))


;;; Browse killring easy
(use-package browse-kill-ring
  :ensure t
  :bind ("M-y" . browse-kill-ring))



;;; Avy jumping
(use-package avy
  :ensure t
  :bind (("M-g c" . avy-goto-char-2)
         ("M-g l" . avy-goto-line)))


;;; Show TODO in magit status
(use-package magit-todos
  :ensure t
  :after magit
  :config
  (magit-todos-mode 1))


;; Visit recent files
(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 500)
  (setq recentf-max-menu-items 60))


