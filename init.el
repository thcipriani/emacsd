;; Packages
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(menu-bar-mode -1)

(setq my-packages
    '(
      use-package
      evil
      evil-leader
      org
      magit
      deft
      helm
      markdown-mode
      neotree
      monokai-theme
      flycheck
      puppet-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'use-package)

(setq inhibit-splash-screen t)

(when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-default 'line-spacing 5)
;; (setq visible-bell t)
;; no bell
(setq ring-bell-function 'ignore)

;; Display current column
(column-number-mode 1)

;; Show parentheses matching
(show-paren-mode 1)

;; Use C-u as scroll up in evil mode
(setq evil-want-C-u-scroll t)

(use-package evil)
(use-package evil-leader)
(use-package evil-jumper)
(evil-leader/set-leader ",")
(setf sentence-end-double-space nil)
(global-evil-leader-mode)
(evil-mode 1)

(evil-leader/set-key "q" 'delete-window)

(require 'helm-config)

;; a buffer
(evil-leader/set-key "ab" 'helm-mini)
(global-set-key (kbd "C-x b") 'helm-mini)

(evil-leader/set-key "ho" 'helm-mini)
(global-set-key (kbd "C-c h o") 'helm-occur)

;; a file
(evil-leader/set-key "af" 'helm-find-files)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(evil-leader/set-key "hx" 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)

(setq helm-quick-update                   t ; do not display invisible candidates
    helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
    helm-buffers-fuzzy-matching           t) ; fuzzy matching buffer names when non--nil
;; (helm-mode 1)

;; Deft - note taking
(use-package deft)
(setq deft-extension "org")
(setq deft-directory "~/Documents/notes")
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)
(setq deft-use-filter-string-for-filename t)
(setq deft-file-naming-rules '((noslash . "-")
                               (nospace . "-")))
;; a note
(evil-leader/set-key "an" 'deft)

;; neotree
(use-package neotree)
(evil-leader/set-key "t" 'neotree-toggle)

(evil-leader/set-key "v" 'split-window-right)
(evil-leader/set-key "w" 'other-window)

(add-hook 'neotree-mode-hook
    (lambda ()
        (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
        (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
        (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
        (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; Customize styles
(global-visual-line-mode 1)
(global-whitespace-mode 1)

(setq whitespace-line-column 80)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq whitespace-style
      (quote (face
              trailing
              tabs
              newline
              newline-mark)))

(setq whitespace-display-mappings
      '((newline-mark ?\n   [?\xAC ?\n] [?¬ ?\n])
        (tab-mark     ?\t   [?\x25B8 ?\t] [?▸ ?\t])))

(global-linum-mode 1)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(server-start)

(load-theme 'monokai t)

;; separate line numbers a bit
(setq linum-format "%4d \u2502 ")

;; TODO: No recommended for some reason?
(setq-default truncate-lines t)
(set-face-font 'default "-*-terminesspowerline-*-*-*-*-28-*-*-*-*-*-*-*")
(add-hook 'after-init-hook #'global-flycheck-mode)
(use-package puppet-mode)
