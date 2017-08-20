;; My central Emacs configuration file
;; Uwe Ziegenhagen, www.uweziegenhagen.de

;; Some general Configuration
;; ###########################################

;; Do no show the startup screen
(setq inhibit-startup-screen t)

;; Make UTF-8 the default encoding
(set-language-environment "UTF-8")

;; Automatically reload buffers if they were changed outside Emacs
(global-auto-revert-mode t)  

;; No more "yes-or-no"-questions, "y-or-n" is sufficent
(defalias 'yes-or-no-p 'y-or-n-p)

;; Stop creating backup~ files
(setq make-backup-files nil) 

;; Stop creating #autosave# files
(setq auto-save-default nil) 

;; Define a function to duplicate the current line and bind command to Ctrl-d
;; from http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point))) eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let
  ;; put the point in the lowest line and return
  (next-line arg))
(global-set-key "\C-d" 'duplicate-line)


;; MELPA & Package Management 
;; ###########################################
;; for Windows: install the dependencies file
;; emacs-25-x86_64-deps.zip from the GNU Emacs  FTP server

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
  (setq package-enable-at-startup nil)


(eval-when-compile
  (require 'use-package))
  (require 'diminish)
  (require 'bind-key)

;; should automatically install required packages
(setq use-package-always-ensure t)

; Configure the recent-files
(use-package recentf
 :init
 (recentf-mode 1) 
 :config
 (setq recentf-max-menu-items 10))


;; https://emacs.stackexchange.com/questions/18881/use-package-for-a-mode
;;(require 'yasnippet)
;;(yas-global-mode 1)

(use-package yasnippet
 :commands (yas-minor-mode) ; autoload `yasnippet' when `yas-minor-mode' is called
                                        ; using any means: via a hook or by user
                                        ; Feel free to add more commands to this
                                        ; list to suit your needs.
 :init ; stuff to do before requiring the package
   (progn
     (add-hook 'prog-mode-hook #'yas-minor-mode)
     (add-hook 'org-mode-hook #'yas-minor-mode)
     )
 :config ; stuff to do after requiring the package
  (progn
    (yas-reload-all))
  )


;; Helm configuration
;; based on https://github.com/senny/emacs.d/blob/master/init.el

(use-package helm-core
  :ensure t)
(use-package helm
  :ensure t
  :bind (("M-a" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("C-SPC" . helm-dabbrev)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list))
  :bind (:map helm-map
	      ("M-i" . helm-previous-line)
	      ("M-k" . helm-next-line)
	      ("M-I" . helm-previous-page)
	      ("M-K" . helm-next-page)
	      ("M-h" . helm-beginning-of-buffer)
	      ("M-H" . helm-end-of-buffer))
  :config (progn
	    (setq helm-buffers-fuzzy-matching t)
(helm-mode 1)))






;; Configure orgmode
;; following  https://github.com/cocreature/dotfiles/blob/master/emacs/.emacs.d/emacs.org
(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c C-w" . org-refile)
         ("C-c j" . org-clock-goto)
         ("C-c C-x C-o" . org-clock-out))
  :init
  :config
  ;; Start Calendar View with Monday
  (setq calendar-week-start-day 1)
  (setq org-todo-keyword-faces
      '(
        ("TODO" . (:foreground "red" :weight bold))
        ("PROGRESSING" . (:foreground "blue" :weight bold))
        ("FEEDBACK" . (:foreground "brown" :weight bold))
        ("VERIFY" . (:foreground "orange" :weight bold))
        ("POSTPONED" . (:foreground "LightSalmon4" :weight bold)) 
        ("DONE" . (:foreground "MediumSeaGreen" :weight bold))
        ("DELEGATED" . (:foreground "ForestGreen" :weight bold))      
        ("CANCELLED" . (:foreground "goldenrod" :weight bold))
        ))
  
  (setq 
   calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch" "Donnerstag" "Freitag" "Samstag"]
   calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai" "Juni" "Juli" "August" "September" "Oktober" "November" "Dezember"])

  ;; Define some additional status, see http://orgmode.org/org.html#TODO-extensions
  (setq org-todo-keywords '((sequence "TODO" "PROGRESSING" "FEEDBACK" "VERIFY" "POSTPONED" "|" "DELEGATED" "CANCELLED" "DONE")))

  ;; let's define that all embedded Python code is safe
  (defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "python")))
  (setq org-confirm-babel-evaluate nil))

;; Keep my personal settings not in the public .emacs file
;; http://www.mygooglest.com/fni/dot-emacs.html
;; load the personal.el if it exists
(let ((personal-settings (expand-file-name "~/personal.el")))
 (when (file-exists-p personal-settings)
   (load personal-settings)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (yasnippet use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Lucida Sans" :foundry "outline" :slant normal :weight normal :height 158 :width normal)))))
