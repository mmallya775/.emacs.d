;;; init.el --- Personal Emacs configuration for Clojure + LSP

;;; ---------------------------------------------------------------------------
;;; Package archives & initialization
;;; ---------------------------------------------------------------------------

(require 'package)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu". "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
;; Initialize
(unless package--initialized
  (package-initialize))

;; Refresh once if nothing installed yet
(unless package-archive-contents
  (package-refresh-contents))

;;; ---------------------------------------------------------------------------
;;; Ensure required packages are installed
;;; (You had these in package-selected-packages already.)
;;; ---------------------------------------------------------------------------

(defvar my/package-list
  '(doom-themes
    clojure-mode
    clojure-mode-extra-font-locking
    clojure-snippets
    cider
    lsp-mode
    flycheck
    company
    rainbow-delimiters
    smartparens
    hugsql-ghosts
    ;; add lsp-ui or company-box if you decide later
    ))

(dolist (pkg my/package-list)
  (unless (package-installed-p pkg)
    (ignore-errors (package-install pkg))))

;;; ---------------------------------------------------------------------------
;;; Native compilation warnings
;;; ---------------------------------------------------------------------------
(setq native-comp-async-report-warnings-errors 'silent)

;;; ---------------------------------------------------------------------------
;;; Theme
;;; ---------------------------------------------------------------------------
(load-theme 'doom-dracula t)

;;; ---------------------------------------------------------------------------
;;; Company (global completion)
;;; ---------------------------------------------------------------------------

(require 'company)
(setq company-idle-delay 0.05          ; set to 0.2 if too aggressive
      company-minimum-prefix-length 1
      company-tooltip-align-annotations t
      company-selection-wrap-around t
      company-show-numbers t
      company-dabbrev-downcase nil
      company-require-match nil)
(global-company-mode 1)

;;; Tab-and-go style (optional)
(with-eval-after-load 'company
  (when (fboundp 'company-tng-configure-default)
    (company-tng-configure-default))
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "TAB")   #'company-complete-selection)
  (define-key company-active-map (kbd "C-s")   #'company-filter-candidates)
  (define-key company-active-map (kbd "C-d")   #'company-show-doc-buffer))

;;; ---------------------------------------------------------------------------
;;; Flycheck
;;; ---------------------------------------------------------------------------
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; ---------------------------------------------------------------------------
;;; Rainbow delimiters & smartparens in programming buffers
;;; ---------------------------------------------------------------------------
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'smartparens-mode)

(with-eval-after-load 'smartparens
  (require 'smartparens-config))

;;; ---------------------------------------------------------------------------
;;; Clojure / LSP configuration
;;; ---------------------------------------------------------------------------

(require 'clojure-mode)
;; clojure-mode already associates .clj .cljc .cljs .edn. If you want:
;; (add-to-list 'auto-mode-alist '("\\.bb\\'" . clojure-mode))

(require 'lsp-mode)

;; Explicit server install dir to avoid nil path issues
(setq lsp-server-install-dir (expand-file-name "lsp-servers/" user-emacs-directory))

;; If you have a system clojure-lsp binary installed and want to bypass auto-download:
;; (setq lsp-clojure-custom-server-command '("clojure-lsp"))

;; General LSP tuning
(setq lsp-keymap-prefix "C-c l"
      lsp-prefer-capf t
      lsp-enable-snippet t
      lsp-enable-indentation t
      lsp-enable-on-type-formatting nil
      lsp-enable-file-watch t
      lsp-idle-delay 0.1
      lsp-log-io nil          ; set to t to debug protocol
      lsp-headerline-breadcrumb-enable t
      lsp-signature-auto-activate nil)

;; Hook LSP into Clojure buffers
(add-hook 'clojure-mode-hook #'lsp-deferred)
;; (add-hook 'clojurec-mode-hook #'lsp-deferred)         ; if using clojurec-mode (rare)
;; (add-hook 'clojurescript-mode-hook #'lsp-deferred)    ; if separate mode variant installed

;;; Optional: format on save via clojure-lsp
(defun my/clojure-lsp-format ()
  (when (and (derived-mode-p 'clojure-mode)
             (bound-and-true-p lsp-mode))
    (ignore-errors (lsp-format-buffer))))
;; Uncomment if you want auto-format:
(add-hook 'before-save-hook #'my/clojure-lsp-format)

(global-set-key (kbd "C-c f") #'my/clojure-lsp-format)

;;; ---------------------------------------------------------------------------
;;; CIDER (interactive REPL & eval)
;;; ---------------------------------------------------------------------------
(with-eval-after-load 'cider
  (setq cider-repl-display-help-banner nil
        cider-eldoc-display-context-dependent-info t))

;;; ---------------------------------------------------------------------------
;;; Hugsql Ghosts (you listed it) – load after clojure
;;; ---------------------------------------------------------------------------
(with-eval-after-load 'clojure-mode
  (ignore-errors (require 'hugsql-ghosts)))

;;; ---------------------------------------------------------------------------
;;; PATH adjustments (if Emacs can't find clojure-lsp or clj-kondo)
;;; Adjust /usr/local/bin if needed (macOS/Homebrew typical)
;;; ---------------------------------------------------------------------------
(let ((local-bin "/usr/local/bin"))
  (when (and (file-directory-p local-bin)
             (not (member local-bin exec-path)))
    (add-to-list 'exec-path local-bin)
    (setenv "PATH" (concat local-bin ":" (getenv "PATH")))))

;; Check inside Emacs (should not be nil):
;; M-: (executable-find "clojure-lsp")
;; M-: (executable-find "clj-kondo")

;;; ---------------------------------------------------------------------------
;;; (Optional) Quick diagnostics key
;;; ---------------------------------------------------------------------------
(defun my/lsp-dump-session ()
  (interactive)
  (if (fboundp 'lsp-describe-session)
      (lsp-describe-session)
    (message "lsp-describe-session not available.")))
(global-set-key (kbd "C-c l s") #'my/lsp-dump-session)

;;; ---------------------------------------------------------------------------
;;; Custom section (leave as last; Emacs will modify it)
;;; ---------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-monokai-pro))
 '(custom-safe-themes
   '("3f24dd8f542f4aa8186a41d5770eb383f446d7228cd7a3413b9f5e0ec0d5f3c0"
     "7771c8496c10162220af0ca7b7e61459cb42d18c35ce272a63461c0fc1336015"
     "d12b1d9b0498280f60e5ec92e5ecec4b5db5370d05e787bc7cc49eae6fb07bc0"
     "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478"
     "e14289199861a5db890065fdc5f3d3c22c5bac607e0dbce7f35ce60e6b55fc52"
     "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
     "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725"
     "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290"
     "4d714a034e7747598869bef1104e96336a71c3d141fa58618e4606a27507db4c"
     "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518"
     "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a"
     "3061706fa92759264751c64950df09b285e3a2d3a9db771e99bcbb2f9b470037"
     "3793b8a8e626a24a8e6aaba21a055473990bd7f2fb69c23e75bb07014d1007c8"
     "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
     "72d9086e9e67a3e0e0e6ba26a1068b8b196e58a13ccaeff4bfe5ee6288175432"
     "a368631abdadffb6882f9994637d7216167912311447f1ec02f9dc58e9cc62a9"
     "f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326"
     "5c7720c63b729140ed88cf35413f36c728ab7c70f8cd8422d9ee1cedeb618de5"
     "01f347a923dd21661412d4c5a7c7655bf17fb311b57ddbdbd6fce87bd7e58de6"
     "9af2b1c0728d278281d87dc91ead7f5d9f2287b1ed66ec8941e97ab7a6ab73c0"
     "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad"
     "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0"
     "87fa3605a6501f9b90d337ed4d832213155e3a2e36a512984f83e847102a42f4"
     "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4"
     "e8ceeba381ba723b59a9abc4961f41583112fc7dc0e886d9fc36fa1dc37b4079"
     "b99ff6bfa13f0273ff8d0d0fd17cc44fab71dfdc293c7a8528280e690f084ef0"
     "0f1341c0096825b1e5d8f2ed90996025a0d013a0978677956a9e61408fcd2c77"
     "5c8a1b64431e03387348270f50470f64e28dfae0084d33108c33a81c1e126ad6"
     "4d5d11bfef87416d85673947e3ca3d3d5d985ad57b02a7bb2e32beaf785a100e"
     "1bc640af8b000ae0275dbffefa2eb22ec91f6de53aca87221c125dc710057511"
     "19d62171e83f2d4d6f7c31fc0a6f437e8cec4543234f0548bad5d49be8e344cd"
     "2f8af2a3a2fae6b6ea254e7aab6f3a8b5c936428b67869cef647c5f8e7985877"
     "4b88b7ca61eb48bb22e2a4b589be66ba31ba805860db9ed51b4c484f3ef612a7"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "22a0d47fe2e6159e2f15449fcb90bbf2fe1940b185ff143995cc604ead1ea171"
     "fffef514346b2a43900e1c7ea2bc7d84cbdd4aa66c1b51946aade4b8d343b55a"
     "e1df746a4fa8ab920aafb96c39cd0ab0f1bac558eff34532f453bd32c687b9d6"
     "ba4f725d8e906551cfab8c5f67e71339f60fac11a8815f51051ddb8409ea6e5c"
     "bb0f3ae2f6f6f6dbbbe03df66d74ca0aecefa6723ac1686f421dd1ffe26b71c3"
     "166a2faa9dc5b5b3359f7a31a09127ebf7a7926562710367086fcc8fc72145da"
     "7de64ff2bb2f94d7679a7e9019e23c3bf1a6a04ba54341c36e7cf2d2e56e2bcc"
     "9b9d7a851a8e26f294e778e02c8df25c8a3b15170e6f9fd6965ac5f2544ef2a9"
     "088cd6f894494ac3d4ff67b794467c2aa1e3713453805b93a8bcb2d72a0d1b53"
     "fd22a3aac273624858a4184079b7134fb4e97104d1627cb2b488821be765ff17"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "2f7fa7a92119d9ed63703d12723937e8ba87b6f3876c33d237619ccbd60c96b9"
     "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "d97ac0baa0b67be4f7523795621ea5096939a47e8b46378f79e78846e0e4ad3d"
     "7c3d62a64bafb2cc95cd2de70f7e4446de85e40098ad314ba2291fc07501b70c"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "dfcd2b13f10da4e5e26eb1281611e43a134d4400b06661445e7cbb183c47d2ec"
     "e4d4cc443964b8a64defc06de3edb2363f7cb1b3c3ae2272b2c1487f626e4318"
     "09b833239444ac3230f591e35e3c28a4d78f1556b107bafe0eb32b5977204d93"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     default))
 '(package-selected-packages
   '(ag all-the-icons-completion all-the-icons-dired
	all-the-icons-ibuffer catppuccin-theme
	clojure-mode-extra-font-locking clojure-snippets company
	dashboard dashboard-hackernews dashboard-ls
	dashboard-project-status doom-modeline doom-themes flycheck
	german-holidays hugsql-ghosts lsp-mode paredit-menu pdf-tools
	rainbow-delimiters smartparens spacemacs-theme transpose-frame
	treemacs-icons-dired treemacs-magit treemacs-projectile
	vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el end


(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))


(setq lsp-clojure-custom-server-command '("/opt/homebrew/bin/clojure-lsp"))

(setq cider-font-lock-dynamically '(macro core function var))


;; Prevent Line Wrapping
(setq-default truncate-lines t)

;; Enable Line Numbers
(global-display-line-numbers-mode)

;; Optional: ag is nice alternative to using grep with Projectile
(use-package ag
  :ensure t)

;; Optional: Enable vertico as the selection framework to use with Projectile
(use-package vertico
  :ensure t
  :init
  (vertico-mode +1))

;; Optional: which-key will show you options for partially completed keybindings
;; It's extremely useful for packages with many keybindings like Projectile.
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/projects/" "~/work/" "~/playground"))
  :config
  ;; I typically use this keymap prefix on macOS
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; On Linux, however, I usually go with another one
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))



(use-package treemacs
  :ensure t
  :defer t
  :bind
  (("C-c t" . treemacs))          ;; Toggle Treemacs
  :config
  ;; Optional quality-of-life tweaks
  (setq treemacs-width  30)        ;; default width (change as desired)
  (setq treemacs-recenter-after-file-follow t
        treemacs-recenter-after-tag-follow  t
        treemacs-is-never-other-window     nil))

(use-package treemacs-projectile    ;; Optional, if you use Projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired   ;; Optional: nice icons in dired
  :after (treemacs dired)
  :ensure t)

(use-package treemacs-magit         ;; Optional: integration with Magit
  :after (treemacs magit)
  :ensure t)

;; Make Command (⌘) the Meta key
(setq mac-command-modifier 'meta)
;; Option (⌥) no longer acts as Meta
(setq mac-option-modifier 'none)


;; Setup dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                         (bookmarks . 5)
                         (projects . 5)
                         (agenda . 5))))


;; Dired icons
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)


;; PDF Reader
(pdf-tools-install)

;; Paredit
(use-package paredit
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  ;; Add other hooks as needed
  )

;; Simpler scheme
(define-key smartparens-mode-map (kbd "M-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "M-<left>")  'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-<left>")  'sp-backward-barf-sexp)

(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)     ;; replace default switch-to-buffer
   ("C-x C-b" . consult-buffer))) ;; also on C-x C-b if you like


(use-package transpose-frame
  :ensure t
  :bind ("C-x t" . transpose-frame))
