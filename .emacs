;; My central Emacs configuration file
;; Uwe Ziegenhagen, www.uweziegenhagen.de

;; Some general Configuration
;; ###########################################

;; make UTF-8 the default encoding
(set-language-environment "UTF-8")

;; Autoload buffers if they were changed outside Emacs
(global-auto-revert-mode t)  

;; no more "yes-or-no"-questions, "y-or-n" is sufficent
(defalias 'yes-or-no-p 'y-or-n-p)

;; stop creating backup~ files
(setq make-backup-files nil) 

;; stop creating #autosave# files
(setq auto-save-default nil) 

;; MELPA & Package Management 
;; ###########################################
;; for Windows: install dependencies file
;; emacs-25-x86_64-deps.zip from FTP server

;; configure MELPA
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

; configure recent-files
(use-package recentf  
 :init   
 (recentf-mode 1)  
 :config   
 (setq recentf-max-menu-items 15))


;;  Custom set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
