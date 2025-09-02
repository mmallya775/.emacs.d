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
   '("268ffd888ba4ffacb351b8860c8c1565b31613ecdd8908675d571855e270a12b"
     "088cd6f894494ac3d4ff67b794467c2aa1e3713453805b93a8bcb2d72a0d1b53"
     "a368631abdadffb6882f9994637d7216167912311447f1ec02f9dc58e9cc62a9"
     "fffef514346b2a43900e1c7ea2bc7d84cbdd4aa66c1b51946aade4b8d343b55a"
     "b7a09eb77a1e9b98cafba8ef1bd58871f91958538f6671b22976ea38c2580755"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "9b9d7a851a8e26f294e778e02c8df25c8a3b15170e6f9fd6965ac5f2544ef2a9"
     "6963de2ec3f8313bb95505f96bf0cf2025e7b07cefdb93e3d2e348720d401425"
     "e8ceeba381ba723b59a9abc4961f41583112fc7dc0e886d9fc36fa1dc37b4079"
     "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
     "22a0d47fe2e6159e2f15449fcb90bbf2fe1940b185ff143995cc604ead1ea171"
     "e4d4cc443964b8a64defc06de3edb2363f7cb1b3c3ae2272b2c1487f626e4318"
     "4d714a034e7747598869bef1104e96336a71c3d141fa58618e4606a27507db4c"
     "19d62171e83f2d4d6f7c31fc0a6f437e8cec4543234f0548bad5d49be8e344cd"
     "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725"
     "2f7fa7a92119d9ed63703d12723937e8ba87b6f3876c33d237619ccbd60c96b9"
     "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "d97ac0baa0b67be4f7523795621ea5096939a47e8b46378f79e78846e0e4ad3d"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "5a0ddbd75929d24f5ef34944d78789c6c3421aa943c15218bac791c199fc897d"
     "d5fd482fcb0fe42e849caba275a01d4925e422963d1cd165565b31d3f4189c87"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "8363207a952efb78e917230f5a4d3326b2916c63237c1f61d7e5fe07def8d378"
     "01f347a923dd21661412d4c5a7c7655bf17fb311b57ddbdbd6fce87bd7e58de6"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     default))
 '(package-selected-packages
   '(cider cider-eval-sexp-fu clj-refactor clojure-snippets
	   clojure-ts-mode company dashboard doom-modeline doom-themes
	   flycheck gruvbox-theme hugsql-ghosts lsp-mode lsp-treemacs
	   magit prettier rainbow-delimiters smartparens
	   spacemacs-theme transpose-frame tree-sitter treemacs
	   treemacs-all-the-icons treemacs-icons-dired
	   treemacs-nerd-icons which-key)))
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
 

;; Maximize on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

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

