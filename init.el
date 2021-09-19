;; load emacs 24's package system. Add MELPA repository.
(define-key special-event-map [config-changed-event] #'ignore)

;; unset C-m, seperate it with the RET key
;; (define-key input-decode-map [?\C-m] [C-m])

;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold #x40000000)


(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)


;; (setq url-proxy-services
;;    '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;;      ("http" . "192.168.31.60")
;;      ("https" . "192.168.31.60")))

;; (setq  x-meta-keysym 'super
;;         x-super-keysym 'meta)
;;
;; (when (eq system-type 'darwin)
;;    (setq mac-option-modifier 'super
;;          mac-command-modifier 'meta))

(setq visible-bell t)
(setq ring-bell-function #'ignore)

(require 'expand-region)


(defun jrh-isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))

(add-hook 'isearch-mode-hook #'jrh-isearch-with-region)



(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t))
(load-theme 'cyberpunk t)



(use-package lsp-mode
  :init (setq
         lsp-keymap-prefix "C-c l"
  ))

(use-package lsp-ui
  :ensure t
  :init (setq lsp-ui-doc-enable                 nil
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
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu)
              ("C-c k" . lsp-ui-doc-show)
        )
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
)


;; go-mode.el with lsp
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'rust-mode-hook 'lsp-deferred)
(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)
(add-hook 'vue-mode-hook 'lsp-deferred)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp-deferred))))



;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(cyberpunk-theme cmake-mode magit lsp-python-ms monokai-theme protobuf-mode vue-mode web-mode centaur-tabs xclip smartparens god-mode rust-mode flycheck mwim which-key treemacs-projectile deadgrep ripgrep lsp-ui treemacs neotree expand-region easy-kill multiple-cursors powerline projectile evil-easymotion evil-collection evil helm-rg helm-ag use-package helm fzf spacemacs-theme sublime-themes company lsp-mode golden-ratio-scroll-screen go-mode))
 '(safe-local-variable-values '((eval progn (pp-buffer) (indent-buffer)))))


(global-whitespace-mode t)
;; (setq whitespace-style '(face space-mark tab-mark newline-mark) )
(setq whitespace-style '(face trailing tabs tab-mark))
;; (setq whitespace-display-mappings
;;         '(
;;           (space-mark 32 [183] [46]) ; SPACE 32 ? ?, 183 MIDDLE DOT ???, 46 FULL STOP ?.?
;;           (newline-mark 10 [182 10]) ; LINE FEED,
;;           (tab-mark 9 [707 9] [92 9]) ; tab
;;           ))
;; indentation-space indentation indentation-tab tabs spaces
(setq whitespace-line-column 85)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(whitespace-tab ((t (:foreground "#797979")))))



(set-face-attribute 'region nil :background "white" :foreground "black")
(set-face-attribute 'isearch nil :foreground "black" :background "yellow")
(set-face-attribute 'lazy-highlight nil :foreground "black" :background "yellow")
(set-face-attribute 'lsp-face-highlight-textual nil :foreground "black" :background "greenyellow")
;; (set-face-attribute 'lsp-face-highlight-read nil :background "limegreen" :foreground "black")
;; (set-face-attribute 'lsp-face-highlight-write nil :background "limegreen" :foreground "black")




(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;  (add-hook 'prog-mode-hook 'linum-mode)
;  (setq linum-format "%4d\u2502")


(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window)
  :delight
  :config (ace-window-display-mode 1)
  )


(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-style "rounded")
  (centaur-tabs-height 36)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "?")
  (centaur-tabs-buffer-groups-function #'centaur-tabs-projectile-buffer-groups)

  :bind
  ("M-h" . centaur-tabs-backward)
  ("M-l" . centaur-tabs-forward)
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



;; (defun my-nuke-traling ()
;;   (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
;; (add-hook 'prog-mode-hook #'my-nuke-traling)

(add-hook 'before-save-hook #'delete-trailing-whitespace)


(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  ; Call Gofmt before saving
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
 ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
 (c-set-offset 'substatement-open 0)
 ;; other customizations can go here

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
:pin melpa-stable
:init (smartparens-global-mode t)
:config
(sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
(sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
(sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET")))
)


;; (electric-pair-mode 1)



(defun pp/deadgrep-view-file ()
  "View result under cursor in other window."
  (interactive)
  (deadgrep-visit-result-other-window)
  (other-window 1)
  (hs-show-all)
  )

(use-package deadgrep
  :ensure t
  :bind
  (:map deadgrep-mode-map ("v" . pp/deadgrep-view-file))
)



(helm-mode 1)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)



(delete-selection-mode 1)


(require 'golden-ratio-scroll-screen)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)



(defun move-and-recenter ()
  (interactive)
  (next-line)
  (recenter-top-bottom 0)
  )



(require 'powerline)
(powerline-default-theme)



(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c f") 'projectile-command-map)
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



;; (defun new-line-dwim ()
;;   (interactive)
;;   (let ((break-open-pair (or (and (looking-back "{") (looking-at "}"))
;;                              (and (looking-back ">") (looking-at "<"))
;;                              (and (looking-back "(") (looking-at ")"))
;;                              (and (looking-back "\\[") (looking-at "\\]")))))
;;     (newline)
;;     (when break-open-pair
;;       (save-excursion
;;         (newline)
;;         (indent-for-tab-command)))
;;     (indent-for-tab-command)))


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
  (hs-hide-all))
;; (add-hook 'prog-mode-hook 'my-hide-all)



(defun my-forward-sexp ()
  (interactive)
    (sp-backward-up-sexp)
    (forward-sexp)
)


(defun my-backward-sexp ()
  (interactive)
  (backward-sexp)
  (forward-char 1)
)


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



;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;     :config
;;     (progn
;;       (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
;;             treemacs-deferred-git-apply-delay      0.5
;;             treemacs-directory-name-transformer    #'identity
;;             treemacs-display-in-side-window        t
;;             treemacs-eldoc-display                 t
;;             treemacs-file-event-delay              5000
;;             treemacs-file-extension-regex          treemacs-last-period-regex-value
;;             treemacs-file-follow-delay             0.2
;;             treemacs-file-name-transformer         #'identity
;;             treemacs-follow-after-init             t
;;             treemacs-git-command-pipe              ""
;;             treemacs-goto-tag-strategy             'refetch-index
;;             treemacs-indentation                   2
;;             treemacs-indentation-string            " "
;;             treemacs-is-never-other-window         nil
;;             treemacs-max-git-entries               5000
;;             treemacs-missing-project-action        'ask
;;             treemacs-no-png-images                 nil
;;             treemacs-no-delete-other-windows       t
;;             treemacs-project-follow-cleanup        nil
;;             treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;             treemacs-position                      'left
;;             treemacs-recenter-distance             0.1
;;             treemacs-recenter-after-file-follow    nil
;;             treemacs-recenter-after-tag-follow     nil
;;             treemacs-recenter-after-project-jump   'always
;;             treemacs-recenter-after-project-expand 'on-distance
;;             treemacs-show-cursor                   nil
;;             treemacs-show-hidden-files             t
;;             treemacs-silent-filewatch              nil
;;             treemacs-silent-refresh                nil
;;             treemacs-sorting                       'alphabetic-asc
;;             treemacs-space-between-root-nodes      t
;;             treemacs-tag-follow-cleanup            t
;;             treemacs-tag-follow-delay              1.5
;;             treemacs-user-mode-line-format         nil
;;             treemacs-width                         35)
;;       ;; The default width and height of the icons is 22 pixels. If you are
;;       ;; using a Hi-DPI display, uncomment this to double the icon size.
;;       ;;(treemacs-resize-icons 44)
;;       (treemacs-follow-mode t)
;;       (treemacs-filewatch-mode t)
;;       (treemacs-fringe-indicator-mode t)
;;       (pcase (cons (not (null (executable-find "git")))
;;                    (not (null treemacs-python-executable)))
;;         (`(t . t)
;;          (treemacs-git-mode 'deferred))
;;         (`(t . _)
;;          (treemacs-git-mode 'simple))))
;;     :bind
;;     (:map global-map
;;           ("M-0"       . treemacs-select-window)
;;           ("C-c t 1"   . treemacs-delete-other-windows)
;;           ("C-c t t"   . treemacs)
;;           ("C-c t B"   . treemacs-bookmark)
;;           ("C-c t C-t" . treemacs-find-file)
;;           ("C-c t M-t" . treemacs-find-tag)))





(add-to-list 'default-frame-alist
             '(font . "Droid Sans Mono-13.2"))







;; (require 'origami)
;; (global-set-key
;;  (kbd "C-c C-o")
;;  (defhydra hydra-folding (:color red)
;;    "
;;   _o_pen node    _n_ext fold       toggle _f_orward  _s_how current only
;;   _c_lose node   _p_revious fold   toggle _a_ll      _r_ecursively toggle node
;;   "
;;    ("r" origami-recursively-toggle-node)
;;    ("o" origami-open-node-recursively)
;;    ("c" origami-close-node-recursively)
;;    ("n" origami-next-fold)
;;    ("p" origami-previous-fold)
;;    ("f" origami-forward-toggle-node)
;;    ("a" origami-toggle-all-nodes)
;;    ("s" origami-show-only-node)))
;; (add-hook 'prog-mode-hook (lambda () (interactive)
;;                                   (origami-mode 0)
;;                                   (call-interactively 'origami-close-all-nodes)) t)
;; (global-set-key (kbd "C-o") 'origami-recursively-toggle-node)

;;  (global-set-key
;;   (kbd "C-c C-l")
;;   (defhydra hydra-folding (:color red)
;;     "
;;     hs-t_o_ggle-hiding hs-show-_a_ll hs-_h_ide-all
;;    "
;;     ("o" my-toggle-fold)
;;     ("a" hs-show-all)
;;     ("h" hs-hide-all)))


(defun mc-mark-next-like-this-then-cycle-forward (arg)
  "Mark next like this then cycle forward, take interactive ARG."
  (interactive "p")
  (call-interactively 'mc/mark-next-like-this)
  (call-interactively 'mc/cycle-forward))

(defun mc-skip-to-next-like-this-then-cycle-forward (arg)
  "Skip to next like this then cycle forward, take interactive ARG."
  (interactive "p")
  (call-interactively 'mc/cycle-backward)
  (call-interactively 'mc/skip-to-next-like-this)
  (call-interactively 'mc/cycle-forward))

(defun mc-mark-previous-like-this-then-cycle-backward (arg)
  "Mark previous like this then cycle backward take interactive ARG."
  (interactive "p")
  (call-interactively 'mc/mark-previous-like-this)
  (call-interactively 'mc/cycle-backward))

(defun mc-skip-to-previous-like-this-then-cycle-backward (arg)
  "Skip to previous like this then cycle backward take interactive ARG."
  (interactive "p")
  (call-interactively 'mc/cycle-forward)
  (call-interactively 'mc/skip-to-previous-like-this)
  (call-interactively 'mc/cycle-backward))


(require 'multiple-cursors)
(global-set-key
 (kbd "C-c m")
    (defhydra hydra-multiple-cursors (:hint nil)
      "
     Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
    ------------------------------------------------------------------
     [_p_]   Next     [_m_]   Next     [_l_] Edit lines  [_0_] Insert numbers
     [_P_]   Skip     [_M_]   Skip     [_a_] Mark all    [_A_] Insert letters
     [_M-p_] Unmark   [_M-m_] Unmark   [_s_] Search      [_q_] Quit
     [_|_] Align with input CHAR       [Click] Cursor at point"
      ("l" mc/edit-lines :exit t)
      ("a" mc/mark-all-like-this :exit t)
      ("m" mc-mark-next-like-this-then-cycle-forward)
      ("M" mc-skip-to-next-like-this-then-cycle-forward)
      ("p" mc-mark-previous-like-this-then-cycle-backward)
      ("P" mc-skip-to-previous-like-this-then-cycle-backward)
      ("M-m" mc/unmark-next-like-this)
      ("M-p" mc/unmark-previous-like-this)
      ("|" mc/vertical-align)
      ("s" mc/mark-all-in-region-regexp :exit t)
      ("0" mc/insert-numbers :exit t)
      ("A" mc/insert-letters :exit t)
      ("<mouse-1>" mc/add-cursor-on-click)
      ;; Help with click recognition in this hydra
      ("<down-mouse-1>" ignore)
      ("<drag-mouse-1>" ignore)
      ("q" nil))
    )


;; (global-set-key [(meta shift n)] 'mc/mark-next-like-this)
;; (global-set-key [(meta shift p)] 'mc/mark-previous-like-this)
;; (global-set-key [(meta shift d)] 'mc/skip-to-next-like-this)
;; (global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c m s") 'mc/skip-to-next-like-this)
;; (global-set-key (kbd "C-c m d") 'mc/mark-all-like-this)

(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))


(which-key-mode 1)


(xclip-mode 1)




;;;;;; catch ESC in terminal(-nw) ;;;;;;;;;;;;

(defvar personal/fast-keyseq-timeout 50)

(defun personal/-tty-ESC-filter (map)
  (if (and (equal (this-single-command-keys) [?\e])
           (sit-for (/ personal/fast-keyseq-timeout 1000.0)))
      [escape] map))

(defun personal/-lookup-key (map key)
  (catch 'found
    (map-keymap (lambda (k b) (if (equal key k) (throw 'found b))) map)))

(defun personal/catch-tty-ESC ()
  "Setup key mappings of current terminal to turn a tty's ESC into `escape'."
  (when (memq (terminal-live-p (frame-terminal)) '(t pc))
    (let ((esc-binding (personal/-lookup-key input-decode-map ?\e)))
      (define-key input-decode-map
        [?\e] `(menu-item "" ,esc-binding :filter personal/-tty-ESC-filter)))))

(personal/catch-tty-ESC)




(unless (display-graphic-p)
   (menu-bar-mode -1))


(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-confirm-create-file 'off-p)
(setq neo-confirm-create-directory 'off-p)




(require 'god-mode)
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)

(defun my-god-mode ()
  (interactive)
  (if (not god-global-mode)
      (god-mode-all))
  )
(global-set-key (kbd "<escape>") #'my-god-mode) ; force use god-mode globally

(defun my-god-mode-update-mode-line ()
  (cond
   (god-global-mode
    (set-face-attribute 'mode-line nil
                        :foreground "#604000"
                        :background "#fff29a")
    (set-face-attribute 'mode-line-inactive nil
                        :foreground "#3f3000"
                        :background "#fff3da"))
   ;; below, the default color is borrowed from monokai theme
   (t
    (set-face-attribute 'mode-line nil
                        :foreground "#F5F5F5"
                        :background "#1B1E1C")
    (set-face-attribute 'mode-line-inactive nil
                        :foreground "#8B8878"
                        :background "#1B1E1C"))
   ))
(add-hook 'post-command-hook 'my-god-mode-update-mode-line)

(define-key god-local-mode-map (kbd "z") #'repeat)
(define-key god-local-mode-map (kbd "i") #'god-mode-all) ; toggle to disable god-mod globally
(define-key god-local-mode-map (kbd "f") #'forward-word)
(define-key god-local-mode-map (kbd "b") #'backward-word)
(define-key god-local-mode-map (kbd "m") #'golden-ratio-scroll-screen-up)
(define-key god-local-mode-map (kbd ",") #'golden-ratio-scroll-screen-down)
(define-key god-local-mode-map (kbd ";") #'recenter)




(require 'rust-mode)



;; (global-set-key (kbd "M-c") 'kill-ring-save)
;; (global-set-key (kbd "M-v") 'yank)
;; (global-set-key (kbd "s-<backspace>") 'backward-kill-word)
;; (global-set-key (kbd "M-k") 'kill-region)
;; (global-set-key (kbd "C-j") 'save-buffer)
;; (global-set-key (kbd "C-z") 'undo)



;; must be set as global
(global-set-key (kbd "M-k") '(lambda () (interactive) (kill-line 0)) )

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-x") #'helm-M-x)
    (define-key map (kbd "C-M-f") #'helm-find-files)
    (define-key map (kbd "C-M-p") #'projectile-find-file)
    (define-key map (kbd "C-M-b") #'switch-to-buffer)

    (define-key map (kbd "C-x C-f") #'helm-find-files)
    (define-key map (kbd "C-x C-p") #'projectile-find-file)

    (define-key map (kbd "C-x C-b") #'switch-to-buffer)

    (define-key map (kbd "C-M-j") #'my-join-lines)

    (define-key map (kbd "C-a") 'mwim-beginning-of-code-or-line)
    (define-key map (kbd "C-e") 'mwim-end-of-code-or-line)
    (define-key map (kbd "<home>") 'mwim-beginning-of-line-or-code)
    (define-key map (kbd "<end>") 'mwim-end-of-line-or-code)

    (define-key map (kbd "C-c .") 'er/expand-region)

    ;; (define-key map (kbd "C-M-k") 'sp-beginning-of-sexp)
    ;; (define-key map (kbd "C-M-j") 'sp-end-of-sexp)

    (define-key map (kbd "C-j") 'save-buffer)
    (define-key map (kbd "M-j") 'mark-paragraph)

    ;; (define-key map (kbd "C-M-u") 'backward-sexp)
    ;; (define-key map (kbd "C-M-d") 'forward-sexp)

    (define-key map (kbd "C-M-u") 'my-backward-sexp)
    (define-key map (kbd "C-M-d") 'my-forward-sexp)

    (define-key map (kbd "C-M-m") 'deadgrep)

    (define-key map (kbd "M-{") 'un-indent-by-removing-4-spaces)
    ;; (define-key map (kbd "M-}") 'indent-region)

    (define-key map (kbd "C-o") 'my-toggle-fold)
    (define-key map (kbd "C-M-o") 'my-hs-toggle-all)

    ;(define-key map (kbd "C-c C-p") 'my-show-file-name)

    ;; (define-key map (kbd "C-:") 'avy-goto-char)
    (define-key map (kbd "M-;") 'avy-goto-word-0)

    (define-key map (kbd "C-c C-j") 'lsp-find-definition)

    (define-key map (kbd "C-c d s") 'sp-rewrap-sexp)
    (define-key map (kbd "C-c d d") 'sp-splice-sexp)

    (define-key map (kbd "M-t") 'flip-buffer-to-window)

    (define-key map (kbd "M-n") 'gcm-scroll-down)
    (define-key map (kbd "M-p") 'gcm-scroll-up)

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
