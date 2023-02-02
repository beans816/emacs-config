;; use-package install and adding melpa archives
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)
(require 'package)
 
(add-to-list 'package-archives
'("melpa" . "http://melpa.org/packages/") t)
 (package-initialize)
(package-refresh-contents)

;; favourite theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wheatgrass))
 '(package-selected-packages
   '(package-filter company package+ vertico-posframe marginalia swiper company-try-hard elpygen use-package vertico melpa-upstream-visit ipython-shell-send elpy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview-common ((t (:foreground unspecified :background "#199919991999"))))
 '(company-tooltip ((t (:inherit default :background "#051e051e051e"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-scrollbar-thumb ((t (:background "#0ccc0ccc0ccc"))))
 '(company-tooltip-scrollbar-track ((t (:background "#199919991999"))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))

;; company
(use-package company
  :ensure t
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.25
        company-minimum-prefix-length 2
        company-selection-wrap-around t)
  (if (display-graphic-p)
      (define-key company-active-map [tab] 'company-select-next)
    (define-key company-active-map (kbd "C-i") 'company-select-next))
  (require 'color)
  (let ((bg (face-attribute 'default :background))
        (ac (face-attribute 'match :foreground)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
     `(company-preview-common ((t (:foreground ,ac :background ,(color-lighten-name bg 10))))))))
;;company-math
(use-package company-math
  :ensure t)

;; REALLY NICE PLUGINS

;; emacs wouldnt be nice without this

(use-package marginalia
  :after vertico
  :ensure t
  :init (marginalia-mode))

;; use C-s to search through your code
(use-package swiper
  :ensure t
  :bind ("C-s" . swiper-isearch)
  :commands (swiper-isearch))

(use-package vertico
  :ensure t
  :custom (vertico-cycle t)
  :init (vertico-mode))

;; Parentheses - make sure you are not missing a ) !
(use-package highlight-parentheses
  :ensure t
  :config
  (progn
    (highlight-parentheses-mode)
    (global-highlight-parentheses-mode))
  )



;; CODING ENVIRONMENTS
;; emacs speacs statistics
;;ess
(use-package ess
  :ensure t
  :init(require 'ess-site))


;; python - mode

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(setq inhibit-startup-message t)           
(global-linum-mode t)

;; python elpy enabled

(use-package elpy
  :ensure t
  :init
  (elpy-enable))
;; ipython

(elpy-enable)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")







;; c and c++

(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;;to use company mode with clang
(require 'cc-mode)
(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)

;; company mode for more c stuff
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; company c headers
(require 'company-c-headers)
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8/")

;;fixes default for default .h header files
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; fix for non standard c++/c file extentions see: https://www.emacswiki.org/emacs/CPlusPlusMode
(require 'cl)

(defun file-in-directory-list-p (file dirlist)
  "Returns true if the file specified is contained within one of
the directories in the list. The directories must also exist."
  (let ((dirs (mapcar 'expand-file-name dirlist))
        (filedir (expand-file-name (file-name-directory file))))
    (and
     (file-directory-p filedir)
     (member-if (lambda (x) ; Check directory prefix matches
                  (string-match (substring x 0 (min(length filedir) (length x))) filedir))
                dirs))))

(defun buffer-standard-include-p ()
  "Returns true if the current buffer is contained within one of
the directories in the INCLUDE environment variable."
  (and (getenv "INCLUDE")
       (file-in-directory-list-p buffer-file-name (split-string (getenv "INCLUDE") path-separator))))

(add-to-list 'magic-fallback-mode-alist '(buffer-standard-include-p . c++-mode))
