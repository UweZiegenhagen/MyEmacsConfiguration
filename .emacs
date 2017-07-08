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

;; configure MELPA
;; without https Win Emacs showed issues
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
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
