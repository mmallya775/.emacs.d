(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-dracula))
 '(custom-safe-themes
   '("5a0ddbd75929d24f5ef34944d78789c6c3421aa943c15218bac791c199fc897d"
     "d5fd482fcb0fe42e849caba275a01d4925e422963d1cd165565b31d3f4189c87"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "8363207a952efb78e917230f5a4d3326b2916c63237c1f61d7e5fe07def8d378"
     "01f347a923dd21661412d4c5a7c7655bf17fb311b57ddbdbd6fce87bd7e58de6"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     default))
 '(package-selected-packages
   '(cider cider-eval-sexp-fu clj-refactor clojure-snippets
	   clojure-ts-mode company dashboard doom-modeline doom-themes
	   flycheck gruvbox-theme lsp-mode lsp-treemacs magit prettier
	   rainbow-delimiters smartparens spacemacs-theme
	   transpose-frame tree-sitter treemacs treemacs-all-the-icons
	   treemacs-icons-dired treemacs-nerd-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 

;; Enable company-mode globally
(add-hook 'after-init-hook 'global-company-mode)

;; Use company-mode for Clojure buffers and CIDER REPL
(add-hook 'clojure-mode-hook 'company-mode)
(add-hook 'cider-repl-mode-hook 'company-mode)

;; Optionally, use company for completion in CIDER
(setq cider-repl-completion-use-company t)
(setq cider-completion-use-company t)


(setq company-idle-delay 0.2)     ;; Show suggestions quickly
(setq company-minimum-prefix-length 1) ;; Start suggesting after one character

;; Enable smartparens in Clojure buffers
(add-hook 'clojure-mode-hook #'smartparens-mode)

;; Enable flycheck (linting) in Clojure buffers
(add-hook 'clojure-mode-hook #'flycheck-mode)



;; Enable the Doom-Line
(require 'doom-modeline)
(doom-modeline-mode 1)


;; Set C-x t to toggle Treemacs sidebar
(global-set-key (kbd "C-x t") 'treemacs)


;; Ensure lsp-mode and lsp-treemacs are loaded
(require 'lsp-mode)
(require 'lsp-treemacs)


;; Enable lsp-mode automatically for supported major modes
;; For Clojure (clojure-mode)
(add-hook 'clojure-mode-hook #'lsp)


;; Optional: lsp-treemacs integration
;; This lets you use lsp-treemacs commands (like lsp-treemacs-errors-list)
;; Example keybinding to view diagnostics:
;; (global-set-key (kbd "C-x l e") #'lsp-treemacs-errors-list) ;; C-x l e opens errors list

;; Optional: UI tweaks for lsp-mode
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-enable-symbol-highlighting t)
(setq lsp-modeline-diagnostics-enable t)
(setq lsp-lens-enable t) ;; Enable code lens (inline info)


;; Make Command (⌘) the Meta key
(setq mac-command-modifier 'meta)
;; Option (⌥) no longer acts as Meta
(setq mac-option-modifier 'none)


;; Stop Backups completely
(setq make-backup-files nil)

;; Enable Line Numbers
(global-display-line-numbers-mode)

;; Prevent Line Wrapping
(setq-default truncate-lines t)



;; Configure the clojure-mode
(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)))

(use-package cider
  :ensure t
  :defer t)


;; Format Clojure code on save using LSP
(add-hook 'clojure-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

;; ;; Optional: Configure LSP formatting options
;; (setq lsp-enable-on-type-formatting nil)  ; Disable auto-formatting while typing
;; (setq lsp-enable-indentation t           ; Enable LSP indentation
;;
;;   ;; require or autoload paredit-mode
;;   (add-hook 'clojure-mode-hook #'paredit-mode)
;;
;;   ;; require or autoload smartparens
;;   (add-hook 'clojure-mode-hook #'smartparens-strict-mode))
;;

(use-package rainbow-delimiters
  :ensure t
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (cider-repl-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)))



;; Enable cider-mode for all Clojure buffers
(add-hook 'clojure-mode-hook #'cider-mode)

;; Also enable for CIDER REPL buffers
(add-hook 'cider-repl-mode-hook #'cider-mode)


;; Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)


;; clojure-ts-mode
  ;; require or autoload paredit-mode
  (add-hook 'clojure-ts-mode-hook #'paredit-mode)

;; require or autoload smartparens
(add-hook 'clojure-ts-mode-hook #'smartparens-strict-mode)
