;; My central Emacs configuration file
;; Uwe Ziegenhagen, www.uweziegenhagen.de

;; Some general Configuration
;; ###########################################



;; keep my personal settings not in the .emacs file
;; http://www.mygooglest.com/fni/dot-emacs.html
;; load it if it exists
(let ((personal-settings "~/personal.init"))
(when (file-exists-p personal-settings)
      (load personal-settings)))



;; make UTF-8 the default encoding
(set-language-environment "UTF-8")

;; duplicate current line, bind command to Ctrl-d
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


;; orgmode following  https://github.com/cocreature/dotfiles/blob/master/emacs/.emacs.d/emacs.org
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
  :init ()
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

  ;; define some additional status 
  ;; http://orgmode.org/org.html#TODO-extensions
  (setq org-todo-keywords '((sequence "TODO" "PROGRESSING" "FEEDBACK" "VERIFY" "POSTPONED" "|" "DELEGATED" "CANCELLED" "DONE")))

  ;; let's define all python code as safe
  (defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "python")))
  (setq org-confirm-babel-evaluate nil)
)





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
