;; load emacs 24's package system. Add MELPA repository.
(define-key special-event-map [config-changed-event] #'ignore)

;; unset C-m, seperate it with the RET key
;; (define-key input-decode-map [?\C-m] [C-m])

;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold #x40000000)


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)


;; (setq  x-meta-keysym 'super
;;         x-super-keysym 'meta)
;;
;; (when (eq system-type 'darwin)
;;    (setq mac-option-modifier 'super
;;          mac-command-modifier 'meta))

(setq visible-bell t)
(setq ring-bell-function #'ignore)



(require 'expand-region)



(toggle-truncate-lines t)


;; (load-theme 'doom-molokai t)
;; (load-theme 'doom-palenight t)
;; (load-theme 'doom-material t)
;; (load-theme 'doom-dracula t)
;; (load-theme 'kaolin-aurora t)
;; (load-theme 'challenger-deep t)
;; (load-theme 'kaolin-temple t)
(load-theme 'kaolin-ocean t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-mc-cursor-default-face ((t (:inherit cursor :background "firebrick1" :inverse-video nil))))
 '(hydra-face-red ((t (:foreground "chocolate" :weight bold)))))



(unless (display-graphic-p)
  (set-face-attribute 'default nil :background "nil")
  (set-face-attribute 'line-number nil :background "nil")
  (set-face-attribute 'line-number-current-line nil :background "nil")
)


(setq-default line-spacing 0)
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "Dejavu Sans Mono for Powerline-14")
  (set-cursor-color "red")
)





(use-package lsp-mode
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l" )
  (setq lsp-signature-auto-activate nil)
  (setq lsp-diagnostics-provider :none)
  :config
  (set-face-attribute 'lsp-face-highlight-textual nil :foreground "purple" :background "white")
)

(use-package lsp-ui
  :defer t
  :init
  (setq lsp-ui-doc-enable                 nil
        lsp-ui-doc-include-signature      t
        lsp-ui-doc-position               'top
        lsp-ui-doc-header                 nil
        lsp-ui-doc-border                 "white"
        lsp-ui-sideline-enable            nil
        lsp-ui-peek-enable                nil
        lsp-ui-sideline-delay             1
        lsp-ui-sideline-ignore-duplicate  t
        lsp-ui-peek-always-show           nil
        lsp-ui-flycheck-enable            nil
        lsp-enable-snippet                nil
   )
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-c u" . lsp-ui-imenu)
        ("C-c k" . lsp-ui-doc-show)
   )
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
)

(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'rust-mode-hook 'lsp-deferred)
(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)
(add-hook 'vue-mode-hook 'lsp-deferred)

(use-package lsp-python-ms
  :defer t
  :init
  (setq lsp-python-ms-auto-install-server t)
  :hook
  (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp-deferred))))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#14141e" "#e84c58" "#35BF88" "#dbac66" "#4ca6e8" "#c79af4" "#6bd9db" "#e6e6e8"])
 '(custom-safe-themes
   '("25a62bce420d4964a8c5c8079d989d33e708bd70c90977041dce9da10c41ab4a" "9faadda7354abf39736f1f70a0b671219c20406f3c83c76162bc2f5256319ff5" "aae54abad4ea9b61e6ce2591732331d93c2cac7a154d11fd1b44cdd0be69b4e4" "a61f08cfc7728d2cb21e12132acb05b21ed6e9f14e1342b936e9c03616a2b401" "2cc34c4e0033e1dd26c41c9f2dc0acd8bcfbb3edeb30c686c941cc4fa540c5ab" "626492d87426dbe828dc3ed886fe913c13600c55c04b1d62bdb1680869633785" "3080956d3b44a537fa2af292806c239304acb84959be129f8014c9470f8a3ca6" "d2b3341ed2c786cefe1b9a4b9d4a023b68e3f2c3f2ace7f2a4cdaa5021c35c57" "e09b0d90563545be26823d77b303d7f862d4e298374d7903fbf310c102192add" "b5f8f2440106661f5a29695602f867c61b015bce8add3eb79ddfc8f6592e723d" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "3ab20589e7267ac9d2762402c794c9d9038c1c14c74361265283caf3b367efea" "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52" "e266d44fa3b75406394b979a3addc9b7f202348099cfde69e74ee6432f781336" "e8567ee21a39c68dbf20e40d29a0f6c1c05681935a41e206f142ab83126153ca" "d516f1e3e5504c26b1123caa311476dc66d26d379539d12f9f4ed51f10629df3" "2050674326d536ddd3dcea87e077d27071cfbbe974a4540b1a57b6b672f64c51" "f00a605fb19cb258ad7e0d99c007f226f24d767d01bf31f3828ce6688cbdeb22" "6128465c3d56c2630732d98a3d1c2438c76a2f296f3c795ebda534d62bb8a0e3" "11cc65061e0a5410d6489af42f1d0f0478dbd181a9660f81a692ddc5f948bf34" "3c7a784b90f7abebb213869a21e84da462c26a1fda7e5bd0ffebf6ba12dbd041" "249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b" "733ef3e3ffcca378df65a5b28db91bf1eeb37b04d769eda28c85980a6df5fa37" "c95813797eb70f520f9245b349ff087600e2bd211a681c7a5602d039c91a6428" "d9a28a009cda74d1d53b1fbd050f31af7a1a105aa2d53738e9aa2515908cac4c" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "5f19cb23200e0ac301d42b880641128833067d341d22344806cdad48e6ec62f6" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "da53441eb1a2a6c50217ee685a850c259e9974a8fa60e899d393040b4b8cc922" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "23c806e34594a583ea5bbf5adf9a964afe4f28b4467d28777bcba0d35aa0872e" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(helm-minibuffer-history-key "M-p")
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(all-the-icons evil-search-highlight-persist evil-visualstar evil-collection evil-surround evil-leader undo-tree evil-mc evil nimbus-theme challenger-deep-theme kaolin-themes spacemacs-theme afternoon-theme ivy golden-ratio-scroll-screen smooth-scrolling yaml-mode projectile-mode doom-themes smart-mode-line cyberpunk-theme cmake-mode magit lsp-python-ms protobuf-mode vue-mode web-mode centaur-tabs xclip smartparens god-mode rust-mode flycheck mwim which-key deadgrep ripgrep lsp-ui neotree expand-region easy-kill projectile helm-rg helm-ag use-package helm fzf company lsp-mode go-mode))
 '(pos-tip-background-color "#1d1d2b")
 '(pos-tip-foreground-color "#d4d4d6")
 '(safe-local-variable-values '((eval progn (pp-buffer) (indent-buffer)))))




(global-whitespace-mode -1)
(setq whitespace-style '(face trailing tabs tab-mark))
(setq whitespace-line-column 85)




(add-hook 'prog-mode-hook 'display-line-numbers-mode)



(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window)
  :delight
  :config
  (ace-window-display-mode 1)
  )


;; (use-package centaur-tabs
;;   :ensure t
;;   :defer t
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   :custom
;;   (centaur-tabs-gray-out-icons 'buffer)
;;   (centaur-tabs-style "rounded")
;;   (centaur-tabs-height 36)
;;   (centaur-tabs-set-icons t)
;;   (centaur-tabs-set-modified-marker t)
;;   (centaur-tabs-modified-marker "?")
;;   (centaur-tabs-buffer-groups-function #'centaur-tabs-projectile-buffer-groups)
;;   :bind
;;   ("M-h" . centaur-tabs-backward)
;;   ("M-l" . centaur-tabs-forward)
;; )



(use-package rust-mode
  :defer t
  :init
)



(use-package yaml-mode
    :defer t
    :init
    :config
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
)



;; delete all other buffers, only keep current one.
(defun my-only-current-buffer ()
  (interactive)
    (mapc 'kill-buffer (cdr (buffer-list (current-buffer))))
    (message "killed other buffers")
    )


(defun my-join-lines (arg)
  (interactive "p")
  (end-of-line)
  (delete-char 1)
  (delete-horizontal-space)
  (insert " "))



(add-hook 'before-save-hook #'delete-trailing-whitespace)


(global-auto-revert-mode t)
(global-hl-line-mode t)


(defun my-go-mode-hook ()
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)




;; put this to .clang-format
;; --
;;   BasedOnStyle: LLVM
;;   UseTab: Never
;;   IndentWidth: 8
;;   TabWidth: 8
(defun my-c-mode-common-hook ()
 (c-set-offset 'substatement-open 0)
 (setq c++-tab-always-indent t)
 (setq c-basic-offset 8)                  ;; Default is 2
 (setq c-indent-level 8)                  ;; Default is 2
 (setq tab-stop-list '(8 12 16 20 24 28 32 36 40 44 48 52 56 60))
 (setq tab-width 8)
 (setq indent-tabs-mode nil)  ; use spaces only if nil
 )
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)



(defun indent-between-pair (&rest _ignored)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(use-package smartparens
  :ensure t
  :defer t
  :pin melpa-stable
  :init (smartparens-global-mode t)
  :config
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))
)




(helm-mode 1)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)





;; (use-package ivy
;;   :ensure t
;;   :diminish ivy-mode
;;   :hook (after-init . ivy-mode))



(delete-selection-mode 1)




;; (require 'smooth-scrolling)
;; (smooth-scrolling-mode 1)


;; (require 'golden-ratio-scroll-screen)
;; (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
;; (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)

 (setq scroll-margin 0
       scroll-conservatively 101
       scroll-up-aggressively 0.01
       scroll-down-aggressively 0.01
       scroll-preserve-screen-position 'always
       auto-window-vscroll nil)

(defun scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

(global-set-key [remap scroll-down-command] 'scroll-half-page-down)
(global-set-key [remap scroll-up-command] 'scroll-half-page-up)


(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))




;; (setq sml/theme 'dark)
;; (sml/setup)


;; (require 'doom-modeline)
;; (doom-modeline-mode 1)



(projectile-mode 1)
(setq projectile-enable-caching t)



;; scroll with cursor not move
(defun gcm-scroll-down ()
      (interactive)
      (scroll-up 1))
(defun gcm-scroll-up ()
      (interactive)
      (scroll-down 1))




(defun flip-buffer-to-window ()
  "Flips to the last-visited buffer in this window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))



(defun json-to-single-line (beg end)
  "Collapse prettified json in region between BEG and END to a single line"
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "[[:space:]\n]+" nil t)
            (replace-match " "))))
    (print "This function operates on a region")))


(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))




(defun my-hs-toggle-all ()
  "If anything isn't hidden, run `hs-hide-all', else run `hs-show-all'."
  (interactive)
  (let ((starting-ov-count (length (overlays-in (point-min) (point-max)))))
    (hs-hide-all)
    (when (equal (length (overlays-in (point-min) (point-max))) starting-ov-count)
        (hs-show-all)
        (recenter)
      )))



(defun my-toggle-fold ()
  (interactive)
  (save-excursion
    (sp-backward-sexp)
    (hs-toggle-hiding)))



(defun my-hide-all()
  (interactive)
  (hs-minor-mode)
  ; (hs-hide-all)
)
(add-hook 'prog-mode-hook 'my-hide-all)



(defun my-show-file-name ()
  (interactive)
  (message (buffer-file-name)))



(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))





;; Have to use require, not use-package
(require 'hydra)
(require 'avy)
(set-face-attribute 'avy-lead-face nil :foreground "white" :background "red")
(set-face-attribute 'avy-lead-face-0 nil :foreground "white" :background "red")
(setq avy-keys (list ?a ?c ?d ?e ?f ?h ?i ?j ?k ?l ?m ?n ?o ?s ?v ?w ?\;))






(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))


(which-key-mode 1)


(xclip-mode 1)


(smerge-mode -1)




;;  ;;;;;; catch ESC in terminal(-nw) ;;;;;;;;;;;;
;;  (defvar personal/fast-keyseq-timeout 50)
;;
;;  (defun personal/-tty-ESC-filter (map)
;;    (if (and (equal (this-single-command-keys) [?\e])
;;             (sit-for (/ personal/fast-keyseq-timeout 1000.0)))
;;        [escape] map))
;;
;;  (defun personal/-lookup-key (map key)
;;    (catch 'found
;;      (map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))
;;
;;  (defun personal/catch-tty-ESC ()
;;    "Setup key mappings of current terminal to turn a tty's ESC into `escape'."
;;    (when (memq (terminal-live-p (frame-terminal)) '(t pc))
;;      (let ((esc-binding (personal/-lookup-key input-decode-map ?\e)))
;;        (define-key input-decode-map
;;          [?\e] `(menu-item "" ,esc-binding :filter personal/-tty-ESC-filter)))))
;;
;;  (personal/catch-tty-ESC)




(menu-bar-mode -1)
(tool-bar-mode -1)
(tab-bar-mode -1)
(scroll-bar-mode -1)


(toggle-truncate-lines t)


(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-confirm-create-file 'off-p)
  (setq neo-confirm-create-directory 'off-p)
  (setq neo-smart-open 't)
)
(global-set-key [f8] 'neotree-toggle)

(defun my-neotree-find()
  (interactive)
  (unless (neo-global--window-exists-p) (neotree-show))
  (neotree-find)
)



(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-want-fine-undo t)
  :config
  (evil-mode 1))

(evil-define-state deadgrep "Evil deadgrep state" :cursor 'bar :enable (motion)) ;; add a deadgrep state
(add-hook 'deadgrep-mode-hook #'evil-deadgrep-state)


(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))


(use-package evil-leader
  :after evil
  :ensure t
  :config
  (global-evil-leader-mode 1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "f" 'projectile-find-file
    "p" 'helm-find-files
    "b" 'switch-to-buffer
    "o" 'deadgrep
    "n" 'neotree-toggle
    "t" 'flip-buffer-to-window
    "l" 'evil-search-highlight-persist-remove-all
    "k" 'kill-this-buffer)
  )


(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))




(setq visible-cursor nil)
(blink-cursor-mode -1)

(setq evil-normal-state-tag   (propertize "  NORMAL " 'face '((:background "dark khaki" :foreground "black")))
      evil-emacs-state-tag    (propertize "  EMACS  " 'face '((:background "turquoise" :foreground "black")))
      evil-insert-state-tag   (propertize "  INSERT " 'face '((:background "dark sea green" :foreground "black")))
      evil-replace-state-tag  (propertize " REPLACE " 'face '((:background "dark orange" :foreground "black")))
      evil-motion-state-tag   (propertize "  MOTION " 'face '((:background "khaki" :foreground "black")))
      evil-visual-state-tag   (propertize "  VISUAL " 'face '((:background "light salmon" :foreground "black")))
      evil-operator-state-tag (propertize " OPERATE " 'face '((:background "sandy brown" :foreground "black"))))

(global-undo-tree-mode)
(evil-set-undo-system 'undo-tree)


(use-package evil-mc
  :ensure t
  :defer t
  :diminish evil-mc-mode "ⓒ"
  ;; :init (global-evil-mc-mode t)
  :init (add-hook 'after-init-hook #'global-evil-mc-mode)
  :config
  (progn
    (defhydra maple/evil-mc ()
      ("C-n" evil-mc-make-and-goto-next-match "next")
      ("C-x" evil-mc-skip-and-goto-next-match "skip")
      ("C-p" evil-mc-make-and-goto-prev-match "prev"))
    (setq evil-mc-enable-bar-cursor nil)
    (evil-define-key 'normal evil-mc-key-map (kbd "<escape>") 'evil-mc-undo-all-cursors))
  :bind (:map evil-mc-key-map
           ("C-g" . evil-mc-undo-all-cursors)
))


(defun my-deadgrep-visit-result ()
  (interactive)
  (deadgrep-visit-result)
  (hs-show-all)
  (recenter)
  )

(defun my-deadgrep-visit-file-other-window ()
  (interactive)
  (deadgrep-visit-result-other-window)
  (hs-show-all)
  (recenter)
  )

(defun my-deadgrep-view-file ()
  "View result under cursor in other window."
  (interactive)
  (deadgrep-visit-result-other-window)
  (hs-show-all)
  (recenter)
  (other-window 1)
  )

(use-package deadgrep
  :ensure t
  ;; :bind
  ;; (:map deadgrep-mode-map
  ;;       ("RET"  . my-deadgrep-visit-result))
  :config
  (setq-default deadgrep--context (cons 3 3))
)


;; disable evil in deadgrep buffer, we define our own keys
;; (evil-set-initial-state 'deadgrep-mode 'emacs)


(global-evil-visualstar-mode 1)


(require 'highlight)
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

(set-face-attribute 'evil-search-highlight-persist-highlight-face nil :background "yellow" :foreground "black")
(set-face-attribute 'isearch                                      nil :background "yellow" :foreground "black")
(set-face-attribute 'lazy-highlight                               nil :background "yellow" :foreground "black")
(set-face-attribute 'highlight                                    nil :background "yellow" :foreground "black" :underline nil)



(setq original-background (face-attribute 'mode-line :background))
(add-hook 'evil-normal-state-entry-hook
          (lambda ()
	    (set-face-attribute 'mode-line-buffer-id nil :foreground "black")
	    (set-face-attribute 'mode-line nil :background "#e6e600" :foreground "black")
	    (set-face-attribute 'mode-line-inactive nil :background "#565063" :foreground "black")
	    ))
(add-hook 'evil-normal-state-exit-hook
          (lambda ()
	    (set-face-attribute 'mode-line-buffer-id nil :foreground "white")
	    (set-face-attribute 'mode-line nil :foreground "#F5F5F5" :background "#1B1E1C")
	    (set-face-attribute 'mode-line-inactive nil :foreground "#8B8878" :background "#1B1E1C")
	    ))



;; (add-hook 'post-command-hook 'my-god-mode-update-mode-line)

(with-eval-after-load 'subr-x
        (setq-default mode-line-buffer-identification
                '(:eval (format-mode-line (propertized-buffer-identification (or (when-let* ((buffer-file-truename buffer-file-truename)
                (prj (cdr-safe (project-current)))
                (prj-parent (file-name-directory (directory-file-name (expand-file-name prj)))))
                        (concat (file-relative-name (file-name-directory buffer-file-truename) prj-parent) (file-name-nondirectory buffer-file-truename)))
"%b"))))))


;; (global-set-key (kbd "M-c") 'kill-ring-save)
;; (global-set-key (kbd "M-v") 'yank)
;; (global-set-key (kbd "s-<backspace>") 'backward-kill-word)
;; (global-set-key (kbd "M-k") 'kill-region)
;; (global-set-key (kbd "C-j") 'save-buffer)
;; (global-set-key (kbd "C-z") 'undo)


;; must be set as global
(global-set-key (kbd "M-k") '(lambda () (interactive) (kill-line 0)) )


;; disable default key bindins in insert mode, but ESC still go to normal
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)


;; use esc to quit, like C-g, esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key helm-map (kbd "ESC") 'keyboard-escape-quit)
(global-set-key [escape] 'evil-exit-emacs-state)



(defun my-last-in-word ()
  "Move to the next 'last character' of a word."
  (interactive)
  (forward-char)
  (re-search-forward "\\w\\b" nil t)
  (goto-char (match-beginning 0)))


(defun my-save-buffer-and-goto-normal-state()
  (interactive)
  (save-buffer)
  (evil-normal-state))


(evil-define-operator my-wrap-with-parens (beg end) (goto-char beg) (insert "(") (goto-char (1+ end)) (insert ")"))
(evil-define-operator my-wrap-with-brackets (beg end) (goto-char beg) (insert "[") (goto-char (1+ end)) (insert "]"))
(evil-define-operator my-wrap-with-parentheses (beg end) (goto-char beg) (insert "{") (goto-char (1+ end)) (insert "}"))


(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-x") #'helm-M-x)
    (define-key map (kbd "C-M-f") #'projectile-find-file)
    (define-key map (kbd "C-M-b") #'switch-to-buffer)

    (define-key map (kbd "C-x C-f") #'helm-find-files)
    ;; (define-key map (kbd "C-x C-p") #'projectile-find-file)

    (define-key map (kbd "C-x C-b") #'switch-to-buffer)
    (define-key map (kbd "C-x C-k") #'kill-buffer)

    (define-key map (kbd "C-a") 'mwim-beginning-of-code-or-line)
    (define-key map (kbd "C-e") 'mwim-end-of-code-or-line)
    (define-key map (kbd "<home>") 'mwim-beginning-of-line-or-code)
    (define-key map (kbd "<end>") 'mwim-end-of-line-or-code)

    (define-key map (kbd "C-c .") 'er/expand-region)

    ;; (define-key map (kbd "C-M-k") 'sp-beginning-of-sexp)
    ;; (define-key map (kbd "C-M-j") 'sp-end-of-sexp)

    (define-key map (kbd "C-j") 'my-save-buffer-and-goto-normal-state)

    ;; (define-key map (kbd "C-M-u") 'backward-sexp)
    ;; (define-key map (kbd "C-M-d") 'forward-sexp)

    ;; (define-key map (kbd "C-M-u") 'my-backward-sexp)
    ;; (define-key map (kbd "C-M-d") 'my-forward-sexp)

    (define-key map (kbd "C-M-m") 'deadgrep)

    (define-key map (kbd "M-{") 'un-indent-by-removing-4-spaces)
    ;; (define-key map (kbd "M-}") 'indent-region)

    (define-key map (kbd "C-o") 'my-toggle-fold)
    (define-key map (kbd "C-M-o") 'my-hs-toggle-all)

    ;(define-key map (kbd "C-c C-p") 'my-show-file-name)

    ;; (define-key map (kbd "C-:") 'avy-goto-char)
    (define-key map (kbd "M-;") 'avy-goto-word-0)

    ;; (define-key map (kbd "C-c C-j") 'lsp-find-definition)

    ;; (define-key map (kbd "C-c d s") 'sp-rewrap-sexp)
    ;; (define-key map (kbd "C-c d d") 'sp-splice-sexp)

    (define-key map (kbd "M-t") 'flip-buffer-to-window)

    (define-key map (kbd "M-n") 'gcm-scroll-down)
    (define-key map (kbd "M-p") 'gcm-scroll-up)

    ;; God mode key mappings
    (define-key evil-normal-state-map (kbd "L") #'mwim-end-of-code-or-line)
    (define-key evil-normal-state-map (kbd "H") #'mwim-beginning-of-code-or-line)
    (define-key evil-visual-state-map (kbd "L") #'mwim-end-of-code-or-line)
    (define-key evil-visual-state-map (kbd "H") #'mwim-beginning-of-code-or-line)
    (define-key evil-operator-state-map (kbd "L") #'mwim-end-of-code-or-line)
    (define-key evil-operator-state-map (kbd "H") #'mwim-beginning-of-code-or-line)

    (define-key evil-visual-state-map (kbd "C-g") #'keyboard-escape-quit)
    (define-key evil-emacs-state-map  (kbd "C-g") #'keyboard-escape-quit)

    (define-key evil-normal-state-map (kbd ";") #'scroll-up-command)
    (define-key evil-normal-state-map (kbd "'") #'scroll-down-command)
    (define-key evil-normal-state-map (kbd "\\") #'evil-scroll-line-to-center)
    (define-key evil-normal-state-map (kbd "C-n") #'er/expand-region)  ;; will goto visual mode

    (define-key evil-normal-state-map (kbd "@") #'my-neotree-find)
    (define-key evil-normal-state-map (kbd "f") #'avy-goto-word-0)

    (define-key evil-normal-state-map (kbd "C-w C-h") #'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-w C-l") #'evil-window-right)
    (define-key evil-normal-state-map (kbd "C-w h") #'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-w l") #'evil-window-right)

    (define-key evil-visual-state-map (kbd ")") #'my-wrap-with-parens)
    (define-key evil-visual-state-map (kbd "(") #'my-wrap-with-parens)
    (define-key evil-visual-state-map (kbd "[") #'my-wrap-with-brackets)
    (define-key evil-visual-state-map (kbd "]") #'my-wrap-with-brackets)
    (define-key evil-visual-state-map (kbd "{") #'my-wrap-with-parentheses)
    (define-key evil-visual-state-map (kbd "}") #'my-wrap-with-parentheses)

    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)


    (define-key evil-visual-state-map (kbd "n")   'maple/evil-mc/body)
    (define-key evil-visual-state-map (kbd "C-n") 'maple/evil-mc/body)

    ;; neotree bindings
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "o") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
    (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
    (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
    (evil-define-key 'normal neotree-mode-map (kbd "j") 'neotree-next-line)
    (evil-define-key 'normal neotree-mode-map (kbd "k") 'neotree-previous-line)
    (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

    ;; deadgrep bindings
    (define-key evil-deadgrep-state-map (kbd "RET") 'my-deadgrep-visit-result)
    (define-key evil-deadgrep-state-map (kbd "o") 'my-deadgrep-visit-file-other-window)
    (define-key evil-deadgrep-state-map (kbd "v") 'my-deadgrep-view-file)
    (define-key evil-deadgrep-state-map (kbd ";") 'scroll-up-command)
    (define-key evil-deadgrep-state-map (kbd "'") 'scroll-down-command)
    (define-key evil-deadgrep-state-map (kbd "C-w C-h") 'evil-window-left)
    (define-key evil-deadgrep-state-map (kbd "C-w C-l") 'evil-window-right)

    ;; projectile
    (define-key projectile-mode-map (kbd "C-c f") 'projectile-command-map)


    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode 1)

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
