;; load emacs 24's package system. Add MELPA repository.
(define-key special-event-map [config-changed-event] #'ignore)

;; unset C-m, seperate it with the RET key
;; (define-key input-decode-map [?\C-m] [C-m])

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; (setq url-proxy-services
;;    '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;;      ("http" . "192.168.31.60")
;;      ("https" . "192.168.31.60")))

(setq  x-meta-keysym 'super
       x-super-keysym 'meta)

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta))


(require 'expand-region)
(global-set-key (kbd "C-c =") 'er/expand-region)

(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t))


;; (setq evil-want-C-u-scroll t)
;; (require 'evil)
;; (evil-mode 1)



(use-package lsp-mode
   :init
   (setq
    lsp-keymap-prefix "C-c l"
    lsp-ui-doc-enable nil
    ))
(use-package lsp-ui)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(deadgrep ripgrep persp-mode treemacs-persp lsp-ui treemacs neotree expand-region easy-kill multiple-cursors powerline projectile evil-easymotion evil-collection evil helm-rg helm-ag use-package helm fzf spacemacs-theme sublime-themes company lsp-mode golden-ratio-scroll-screen go-mode))
 '(spacemacs-theme-custom-colors '((bg1 . "#171421"))))





(global-whitespace-mode 1)
(setq whitespace-style '(face trailing tabs tab-mark))
(set-face-attribute 'whitespace-space nil :background nil :foreground "gray30")
(setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46]) ; SPACE 32 ? ?, 183 MIDDLE DOT ???, 46 FULL STOP ?.?
          (newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [707 9] [92 9]) ; tab
          ))



(add-hook 'prog-mode-hook 'linum-mode)



(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)



(require 'golden-ratio-scroll-screen)
(global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
(global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)



(defun move-and-recenter ()
  (interactive)
  (next-line)
  (recenter-top-bottom 0)
  )



;; go-mode.el with lsp
(add-hook 'go-mode-hook 'lsp-deferred)

(require 'powerline)
(powerline-default-theme)



(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c f") 'projectile-command-map)



(add-to-list 'default-frame-alist
             '(font . "Ubuntu Mono derivative Powerline-13.2"))


(defun gcm-scroll-down ()
      (interactive)
      (scroll-up 1))
(defun gcm-scroll-up ()
      (interactive)
      (scroll-down 1))
;; scroll with cursor not move
(global-set-key "\M-n" 'gcm-scroll-down)
(global-set-key "\M-p" 'gcm-scroll-up)


;; (global-set-key (kbd "M-c") 'kill-ring-save)
;; (global-set-key (kbd "M-v") 'yank)
;; (global-set-key (kbd "s-<backspace>") 'backward-kill-word)
;; (global-set-key (kbd "M-k") 'kill-region)
;; (global-set-key (kbd "C-j") 'save-buffer)

(global-set-key (kbd "C-z") 'undo)

(defun only-current-buffer ()
  (interactive)
    (mapc 'kill-buffer (cdr (buffer-list (current-buffer))))
    (message "killed other buffers")
    )
(global-set-key (kbd "C-c b l") 'only-current-buffer)

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






;; scroll with C-m  C-,
;; (global-set-key (kbd "<C-m>") 'golden-ratio-scroll-screen-up)
;; (global-set-key (kbd "C-,") 'golden-ratio-scroll-screen-down)

;; (global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "M-;") 'avy-goto-word-0)

(setq-default cursor-type 'bar)
(set-cursor-color "#00ff00")


(require 'multiple-cursors)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m s") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-c m d") 'mc/mark-all-like-this)


(with-eval-after-load "persp-mode-autoloads"
      (setq wg-morph-on nil) ;; switch off animation
      (setq persp-autokill-buffer-on-remove 'kill-weak)
      (add-hook 'window-setup-hook #'(lambda () (persp-mode 1))))



(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-persp
  :after treemacs persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))


;; (global-set-key (kbd "C-x C-b") 'ibuffer)



;; (require 'neotree)
;; (global-set-key [f8] 'neotree-toggle)
;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
