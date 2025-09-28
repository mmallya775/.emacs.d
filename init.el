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

;; Prevent Line Wrapping
(setq-default truncate-lines t)

;; Enable Line Numbers
(global-display-line-numbers-mode)


;; Suppress native compiler warnings
(setq native-comp-async-report-warnings-errors 'silent)
(setq compilation-display-buffer-action '(nil . "*compilation*"))

;;;; Append the homebrew installed stuffs into the exec-path
;;;; otherwise emacs wouldn't see lein or npm installed from homebrew

(let ((path (getenv "PATH")))
  (setenv "PATH" (concat "/home/linuxbrew/.linuxbrew/bin:" path))
  (setq exec-path (split-string (getenv "PATH") path-separator)))


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
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(custom-enabled-themes '(spacemacs-dark))
 '(custom-safe-themes
   '("3061706fa92759264751c64950df09b285e3a2d3a9db771e99bcbb2f9b470037"
     "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "42a6583a45e0f413e3197907aa5acca3293ef33b4d3b388f54fa44435a494739"
     "2f7fa7a92119d9ed63703d12723937e8ba87b6f3876c33d237619ccbd60c96b9"
     "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad"
     "9af2b1c0728d278281d87dc91ead7f5d9f2287b1ed66ec8941e97ab7a6ab73c0"
     "21d2bf8d4d1df4859ff94422b5e41f6f2eeff14dd12f01428fa3cb4cb50ea0fb"
     "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
     "b6269b0356ed8d9ed55b0dcea10b4e13227b89fd2af4452eee19ac88297b0f99"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "22a0d47fe2e6159e2f15449fcb90bbf2fe1940b185ff143995cc604ead1ea171"
     "daa27dcbe26a280a9425ee90dc7458d85bd540482b93e9fa94d4f43327128077"
     "c20728f5c0cb50972b50c929b004a7496d3f2e2ded387bf870f89da25793bb44"
     "d2ab3d4f005a9ad4fb789a8f65606c72f30ce9d281a9e42da55f7f4b9ef5bfc6"
     "01f347a923dd21661412d4c5a7c7655bf17fb311b57ddbdbd6fce87bd7e58de6"
     "aa545934ce1b6fd16b4db2cf6c2ccf126249a66712786dd70f880806a187ac0b"
     "a372fd35724ebb25694e8f977fde62af3e9dd5e31d71005968545042419fa47d"
     "bf4d25079f7d052cb656e099d9c2af9fb61ee377e8e72b7f13cecf8dffb74f92"
     "f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326"
     "b7a09eb77a1e9b98cafba8ef1bd58871f91958538f6671b22976ea38c2580755"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "72d9086e9e67a3e0e0e6ba26a1068b8b196e58a13ccaeff4bfe5ee6288175432"
     "4d714a034e7747598869bef1104e96336a71c3d141fa58618e4606a27507db4c"
     "1bc640af8b000ae0275dbffefa2eb22ec91f6de53aca87221c125dc710057511"
     "e4d4cc443964b8a64defc06de3edb2363f7cb1b3c3ae2272b2c1487f626e4318"
     "a368631abdadffb6882f9994637d7216167912311447f1ec02f9dc58e9cc62a9"
     "19d62171e83f2d4d6f7c31fc0a6f437e8cec4543234f0548bad5d49be8e344cd"
     "e1df746a4fa8ab920aafb96c39cd0ab0f1bac558eff34532f453bd32c687b9d6"
     "166a2faa9dc5b5b3359f7a31a09127ebf7a7926562710367086fcc8fc72145da"
     "5244ba0273a952a536e07abaad1fdf7c90d7ebb3647f36269c23bfd1cf20b0b8"
     "87fa3605a6501f9b90d337ed4d832213155e3a2e36a512984f83e847102a42f4"
     "5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5"
     "599f72b66933ea8ba6fce3ae9e5e0b4e00311c2cbf01a6f46ac789227803dd96"
     "8dbbcb2b7ea7e7466ef575b60a92078359ac260c91fe908685b3983ab8e20e3f"
     "65057902bcd51d84e0e28036f4759295e08f57b1ba94b9ae10a8d5ffde5f154f"
     "fb83a50c80de36f23aea5919e50e1bccd565ca5bb646af95729dc8c5f926cbf3"
     "e3a1b1fb50e3908e80514de38acbac74be2eb2777fc896e44b54ce44308e5330"
     "b02eae4d22362a941751f690032ea30c7c78d8ca8a1212fdae9eecad28a3587f"
     "c8b83e7692e77f3e2e46c08177b673da6e41b307805cd1982da9e2ea2e90e6d7"
     "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476"
     "5c8a1b64431e03387348270f50470f64e28dfae0084d33108c33a81c1e126ad6"
     "4d5d11bfef87416d85673947e3ca3d3d5d985ad57b02a7bb2e32beaf785a100e"
     default))
 '(package-selected-packages
   '(all-the-icons-dired auto-package-update cider-hydra clj-refactor
			 clojure-mode-extra-font-locking company-box
			 company-prescient consult-lsp dashboard
			 docker docker-compose-mode dockerfile-mode
			 doom-modeline doom-themes eval-sexp-fu
			 flycheck-clj-kondo git-gutter-fringe
			 indent-bars kanagawa-themes lsp-treemacs
			 lsp-ui magit-delta marginalia
			 monokai-pro-theme monokai-theme orderless
			 rainbow-delimiters spacemacs-theme
			 standard-themes surround transpose-frame
			 treemacs-icons-dired treemacs-magit
			 treemacs-projectile vertico
			 yasnippet-snippets)))
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
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-repl-result-prefix ";; => ")
  (setq cider-eval-spinner-type 'vertical-breathing))


(use-package clojure-mode
  :ensure t
  :init
  ;; Disable auto ns template insertion (clojure-lsp will handle it)
  (setq clojure-insert-namespace-template nil))

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
	lsp-prefer-capf t
	lsp-enable-snippet t
	lsp-enable-indentation t
	lsp-enable-on-type-formatting nil
	lsp-enable-file-watch t
	lsp-idle-delay 0.2
	lsp-log-io nil			; set to t to debug protocol
	lsp-headerline-breadcrumb-enable t
	lsp-signature-auto-activate nil)
  (setq lsp-file-watch-ignored-directories
      '("[/\\\\]\\.shadow-cljs\\'"
        "[/\\\\]\\.git\\'"
        "[/\\\\]\\.clj-kondo\\'"
        "[/\\\\]\\.cpcache\\'"
        "[/\\\\]target\\'"
        "[/\\\\]node_modules"))
  :config
  (setq gc-cons-threshold (* 100 1024 1024)
                read-process-output-max (* 1024 1024))
  :hook ((clojure-mode . lsp)
	 (clojurescript-mode . lsp)
         (clojurec-mode . lsp)
         (lisp-mode . lsp)))

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
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  :bind (:map paredit-mode-map
              ("M-<right>" . paredit-forward-slurp-sexp)
              ("M-<left>" . paredit-forward-barf-sexp)
              ("C-M-<left>" . paredit-backward-slurp-sexp)
              ("C-M-<right>" . paredit-backward-barf-sexp)))



;;;------------------------------------------------------------
;; Company mode for the suggestions and completions

(use-package company
  :ensure t
  :init
  ;; The :init keyword is for code that needs to run before the package is loaded.
  ;; We can enable global-company-mode here, as it's a very light operation.
  (global-company-mode 1)
  :custom
  ;; ;; Disabling the grammar autocomplete
  ;; (company-backends '(
  ;;                     (company-lsp company-yasnippet company-capf)
  ;;                     ))
  ;; The :custom keyword is perfect for setting variables
  (company-idle-delay 0.15)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-dabbrev-downcase nil)
  (company-require-match nil)
  :config
  ;; The :config keyword runs after the package has been loaded.
  ;; This is where you put package-specific function calls and keybindings.
  (when (fboundp 'company-tng-configure-default)
    (company-tng-configure-default))
  :bind
  ;; The :bind keyword provides a clean, declarative way to set keybindings.
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<tab>" . company-complete-selection)
        ("TAB" . company-indent-or-complete-common)
        ("C-s" . company-filter-candidates)
        ("C-d" . company-show-doc-buffer))
  (:map global-map
        ("M-/" . company-complete)))

;;;!!! Experimental stuff for better sorting and fuzziness
(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode 1))

(use-package company-prescient
  :ensure t
  :config
  (company-prescient-mode 1))

;;<> Better UI for company mode
(use-package company-box
  :hook (company-mode . company-box-mode))

;; ---------------------------------------------------------------------------
;; Flycheck for syntax checking on the fly

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;;<> clj-kondo support for flycheck

(use-package flycheck-clj-kondo
  :ensure t
  :after flycheck)


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
(use-package consult-lsp
  :ensure t
  :after (consult lsp-mode)) ;; references at point

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
  (doom-modeline-height 15)
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
  (setq treemacs-width  30) ;; default width (change as desired)
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
(add-hook 'before-save-hook #'my/clojure-lsp-format)

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
  (setq git-gutter:update-interval 0.02))

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
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                         (bookmarks . 5)
                         (projects . 5)
                         (agenda . 5)))
  ;; Configure the appearance
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-icon-type 'all-the-icons)
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
        auto-package-update-interval 3) 
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
  :ensure t)



;;-----------------------------------------------------------------
;; Consult LSP for navigation
(use-package consult-lsp
  :ensure t
  :after lsp-mode
  :bind (("C-c l s" . consult-lsp-symbols)))


;; ---------------------------------------------------------------
;; Hydra stuff

(use-package hydra
  :ensure t)

(use-package cider-hydra
  :after cider
  :ensure t)
