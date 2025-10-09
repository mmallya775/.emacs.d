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
(ns-toggle-toolbar nil)

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
 '(custom-enabled-themes '(ef-cyprus))
 '(custom-safe-themes
   '("75eef60308d7328ed14fa27002e85de255c2342e73275173a14ed3aa1643d545"
     "3613617b9953c22fe46ef2b593a2e5bc79ef3cc88770602e7e569bbd71de113b"
     "f253a920e076213277eb4cbbdf3ef2062e018016018a941df6931b995c6ff6f6"
     "2ab8cb6d21d3aa5b821fa638c118892049796d693d1e6cd88cb0d3d7c3ed07fc"
     "2a1d0e4e6bd665fb532cb07cdf466e3bba1e4f2e5e6e01eee1fa913edabe8759"
     "e14991397ba1341f1240216392f48889a343506b1f9c8e9c37ed391151f87463"
     "5a548c9d5a6ca78d13283ed709bddf3307b65a7695e1b2e2b7e0a9dde45e8599"
     "bb0f3ae2f6f6f6dbbbe03df66d74ca0aecefa6723ac1686f421dd1ffe26b71c3"
     "ba4f725d8e906551cfab8c5f67e71339f60fac11a8815f51051ddb8409ea6e5c"
     "83550d0386203f010fa42ad1af064a766cfec06fc2f42eb4f2d89ab646f3ac01"
     "3f24dd8f542f4aa8186a41d5770eb383f446d7228cd7a3413b9f5e0ec0d5f3c0"
     "921f165deb8030167d44eaa82e85fcef0254b212439b550a9b6c924f281b5695"
     "0f1341c0096825b1e5d8f2ed90996025a0d013a0978677956a9e61408fcd2c77"
     "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d"
     "0b41a4a9f81967daacd737f83d3eac7e3112d642e3f786cf7613de4da97a830a"
     "b9c002dc827fb75b825da3311935c9f505d48d7ee48f470f0aa7ac5d2a595ab2"
     "cee5c56dc8b95b345bfe1c88d82d48f89e0f23008b0c2154ef452b2ce348da37"
     "aff0396925324838889f011fd3f5a0b91652b88f5fd0611f7b10021cc76f9e09"
     "d6b369a3f09f34cdbaed93eeefcc6a0e05e135d187252e01b0031559b1671e97"
     "1ad12cda71588cc82e74f1cabeed99705c6a60d23ee1bb355c293ba9c000d4ac"
     "f9d423fcd4581f368b08c720f04d206ee80b37bfb314fa37e279f554b6f415e9"
     "09b833239444ac3230f591e35e3c28a4d78f1556b107bafe0eb32b5977204d93"
     "d5fd482fcb0fe42e849caba275a01d4925e422963d1cd165565b31d3f4189c87"
     "51fa6edfd6c8a4defc2681e4c438caf24908854c12ea12a1fbfd4d055a9647a3"
     "5a0ddbd75929d24f5ef34944d78789c6c3421aa943c15218bac791c199fc897d"
     "8363207a952efb78e917230f5a4d3326b2916c63237c1f61d7e5fe07def8d378"
     "d97ac0baa0b67be4f7523795621ea5096939a47e8b46378f79e78846e0e4ad3d"
     "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290"
     "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0"
     "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725"
     "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176"
     "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f"
     "088cd6f894494ac3d4ff67b794467c2aa1e3713453805b93a8bcb2d72a0d1b53"
     "1f8bd4db8280d5e7c5e6a12786685a7e0c6733b0e3cf99f839fb211236fb4529"
     "4b88b7ca61eb48bb22e2a4b589be66ba31ba805860db9ed51b4c484f3ef612a7"
     "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     "720838034f1dd3b3da66f6bd4d053ee67c93a747b219d1c546c41c4e425daf93"
     "9b9d7a851a8e26f294e778e02c8df25c8a3b15170e6f9fd6965ac5f2544ef2a9"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "6963de2ec3f8313bb95505f96bf0cf2025e7b07cefdb93e3d2e348720d401425"
     "b754d3a03c34cfba9ad7991380d26984ebd0761925773530e24d8dd8b6894738"
     "7c3d62a64bafb2cc95cd2de70f7e4446de85e40098ad314ba2291fc07501b70c"
     "d609d9aaf89d935677b04d34e4449ba3f8bbfdcaaeeaab3d21ee035f43321ff1"
     "268ffd888ba4ffacb351b8860c8c1565b31613ecdd8908675d571855e270a12b"
     "fffef514346b2a43900e1c7ea2bc7d84cbdd4aa66c1b51946aade4b8d343b55a"
     "211621592803ada9c81ec8f8ba0659df185f9dc06183fcd0e40fbf646c995f23"
     "ae20535e46a88faea5d65775ca5510c7385cbf334dfa7dde93c0cd22ed663ba0"
     "541282f66e5cc83918994002667d2268f0a563205117860e71b7cb823c1a11e9"
     "da69584c7fe6c0acadd7d4ce3314d5da8c2a85c5c9d0867c67f7924d413f4436"
     "4c16a8be2f20a68f0b63979722676a176c4f77e2216cc8fe0ea200f597ceb22e"
     "6af300029805f10970ebec4cea3134f381cd02f04c96acba083c76e2da23f3ec"
     "59c36051a521e3ea68dc530ded1c7be169cd19e8873b7994bfc02a216041bf3b"
     "a3a71b922fb6cbf9283884ac8a9109935e04550bcc5d2a05414a58c52a8ffc47"
     "296dcaeb2582e7f759e813407ff1facfd979faa071cf27ef54100202c45ae7d4"
     "e85a354f77ae6c2e47667370a8beddf02e8772a02e1f7edb7089e793f4762a45"
     "c038d994d271ebf2d50fa76db7ed0f288f17b9ad01b425efec09519fa873af53"
     "36c5acdaf85dda0dad1dd3ad643aacd478fb967960ee1f83981d160c52b3c8ac"
     "cd5f8f91cc2560c017cc9ec24a9ab637451e36afd22e00a03e08d7b1b87c29ca"
     "19b62f442479efd3ca4c1cef81c2311579a98bbc0f3684b49cdf9321bd5dfdbf"
     "71b688e7ef7c844512fa7c4de7e99e623de99a2a8b3ac3df4d02f2cd2c3215e7"
     "b41d0a9413fb0034cea34eb8c9f89f6e243bdd76bccecf8292eb1fefa42eaf0a"
     "3d9938bbef24ecee9f2632cb25339bf2312d062b398f0dfb99b918f8f11e11b1"
     "fae5872ff90462502b3bedfe689c02d2fa281bc63d33cb007b94a199af6ccf24"
     "ffa78fc746f85d1c88a2d1691b1e37d21832e9a44a0eeee114a00816eabcdaf9"
     "ac893acecb0f1cf2b6ccea5c70ea97516c13c2b80c07f3292c21d6eb0cb45239"
     "b1791a921c4f38cb966c6f78633364ad880ad9cf36eef01c60982c54ec9dd088"
     "df39cc8ecf022613fc2515bccde55df40cb604d7568cb96cd7fe1eff806b863b"
     "00d7122017db83578ef6fba39c131efdcb59910f0fac0defbe726da8072a0729"
     "ea4dd126d72d30805c083421a50544e235176d9698c8c541b824b60912275ba1"
     "b3ba955a30f22fe444831d7bc89f6466b23db8ce87530076d1f1c30505a4c23b"
     "3061706fa92759264751c64950df09b285e3a2d3a9db771e99bcbb2f9b470037"
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
 '(package-selected-packages nil))
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
	company-minimum-prefix-length 1
	lsp-enable-snippet t
	lsp-enable-indentation t
	lsp-file-watch-threshold 5000
	lsp-enable-on-type-formatting nil
	lsp-completion-no-cache t
	;; lsp-enable-file-watchers t
	lsp-idle-delay 0.2
	lsp-headerline-breadcrumb-enable t
	lsp-signature-auto-activate nil)
  :config
  (setq gc-cons-threshold (* 100 1024 1024)
                read-process-output-max (* 1024 1024))

  (define-key lsp-mode-map (kbd "C-c l r") #'lsp-workspace-restart)
  
  (setq lsp-file-watch-ignored-directories
	'("[/\\\\]\\.shadow-cljs\\'" "[/\\\\]\\.git\\'" "[/\\\\]\\.hg\\'" "[/\\\\]\\.bzr\\'" "[/\\\\]_darcs\\'" "[/\\\\]\\.svn\\'" "[/\\\\]_FOSSIL_\\'" "[/\\\\]\\.idea\\'" "[/\\\\]\\.ensime_cache\\'" "[/\\\\]\\.eunit\\'" "[/\\\\]node_modules" "[/\\\\]\\.fslckout\\'" "[/\\\\]\\.tox\\'" "[/\\\\]dist\\'" "[/\\\\]dist-newstyle\\'" "[/\\\\]\\.stack-work\\'" "[/\\\\]\\.bloop\\'" "[/\\\\]\\.metals\\'" "[/\\\\]target\\'" "[/\\\\]\\.ccls-cache\\'" "[/\\\\]\\.vscode\\'" "[/\\\\]\\.deps\\'" "[/\\\\]build-aux\\'" "[/\\\\]autom4te.cache\\'" "[/\\\\]\\.reference\\'" "[/\\\\]\\.lsp\\'" "[/\\\\]\\.clj-kondo\\'" "[/\\\\]\\.cpcache\\'" "[/\\\\]bin/Debug\\'" "[/\\\\]obj\\'"))
  :custom
        (lsp-gopls-staticcheck t)   ;; enable extra analysis
        (lsp-gopls-complete-unimported t)
        (lsp-gopls-use-placeholders t)
  :hook ((clojure-mode . lsp)
	 (clojurescript-mode . lsp)
         (clojurec-mode . lsp)
         (lisp-mode . lsp)
	 (go-mode . lsp)
	 (rustic-mode . lsp)))

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
  :hook ((go-mode . lsp)
         (before-save . gofmt-before-save))
  :config
  ;; Use goimports instead of gofmt if available
  (setq gofmt-command "goimports"))


;; Smartparens only for Go & Rust
(use-package smartparens
  :ensure t
  :hook ((go-mode . smartparens-mode)
	 (rustic-mode . smartparens-mode))
  :config
  (require 'smartparens-config))



;; (use-package catppuccin-theme
;;   :ensure t
;;   :config
;;   ;; Choose one of the available flavors: 'latte, 'frappe, 'macchiato, or 'mocha
;;   (setq catppuccin-flavor 'mocha)
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
        lsp-rust-analyzer-cargo-watch-command "clippy"))

;;--------------------------------------------------------------------
;; Highlighting for toml files

(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" . toml-mode))


