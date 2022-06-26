;;; phd-modeline.el --- My modeline customization
;; ----------------------------------------------
;;; Author: phdenzel
;;
;;
;;; Commentary:
;; ------------
;; This is my module for customising the Emacs mode-line.
;;
;;; Installation (for example):
;; ----------------------------
;; (add-to-list 'load-path "~/phd-modeline/")
;; (require 'phd-modeline)
;; (phd-modeline-mode 1)
;; (global-set-key (kbd "C-x |") 'phd-modeline-mode)
;;
;; or
;;
;; (use-package phd-modeline
;;   :ensure nil
;;   :load-path "~/phd-modeline/"
;;   :after all-the-icons
;;   :hook (after-init . phd-modeline-mode)
;;   :bind (("C-x |" . phd-modeline-mode))
;;   :config (...))
;;
;;; Code:
(require 'all-the-icons)


(defgroup phd-modeline nil
  "A minimal mode-line."
  :group 'mode-line
  :link '(url-link :tag "Homepage" "https://github.com/phdenzel/phd-modeline"))


;; --- Mode
(defvar phd-modeline-mode-map (make-sparse-keymap))

(define-minor-mode phd-modeline-mode
  "Toggle phd-modeline on or off."
  :group 'phd-modeline
  :global t
  :keymap phd-modeline-mode-map
  (if phd-modeline-mode
      (if phd-modeline-format
          (setq-default mode-line-format phd-modeline-format)
        (setq-default mode-line-format phd-modeline-format-default))
    (setq-default mode-line-format phd-modeline-format-original)))


;; --- Style
(defface phd-modeline-buffer-name-face
  `((t (:inherit 'minibuffer-prompt)))
  "The phd-modeline normal buffer name face"
  :group 'phd-modeline)

(defface phd-modeline-space-face
  `((t (:inherit 'mode-line)))
  "The phd-modeline face of empty space."
  :group 'phd-modeline)

(defface phd-modeline-buffer-modified-face
  `((t (:inherit 'font-lock-function-name-face)))
  "The phd-modeline modified buffer name face"
  :group 'phd-modeline)

(defface phd-modeline-buffer-read-only-face
  `((t (:inherit 'font-lock-keyword-face)))
  "The phd-modeline read-only buffer name face"
  :group 'phd-modeline)

(defface phd-modeline-buffer-line-face
  `((t (:inherit 'mode-line)))
  "The phd-modeline buffer line face"
  :group 'phd-modeline)

(defface phd-modeline-buffer-column-face
  `((t (:inherit 'mode-line)))
  "The phd-modeline buffer column face"
  :group 'phd-modeline)

(defface phd-modeline-buffer-percentage-face
  `((t (:inherit 'mode-line)))
  "The phd-modeline buffer percentage face"
  :group 'phd-modeline)

(defface phd-modeline-mode-face
  `((t (:inherit 'mode-line)))
  "The phd-modeline buffer percentage face"
  :group 'phd-modeline)

(defface phd-modeline-inactive-face
  `((t (:inherit 'mode-line-inactive)))
  "The phd-modeline face for inactive frames"
  :group 'phd-modeline)

(defface phd-modeline-highlight-face
  `((t (:inherit 'mode-line-highlight)))
  "The phd-modeline highlight face"
  :group 'phd-modeline)

(defface phd-modeline-flycheck-success-face
  `((t (:inherit 'success)))
  "The phd-modeline flycheck face for success."
  :group 'phd-modeline)

(defface phd-modeline-flycheck-warning-face
  `((t (:inherit 'warning)))
  "The phd-modeline flycheck face for warnings."
  :group 'phd-modeline)

(defface phd-modeline-flycheck-error-face
  `((t (:inherit 'success)))
  "The phd-modeline flycheck face for errors."
  :group 'phd-modeline)

(defface phd-modeline-vc-icon-face
  `((t (:inherit 'font-lock-keyword-face)))
  "The phd-modeline version control icon face"
  :group 'phd-modeline)

(defface phd-modeline-vc-branch-face
  `((t (:inherit 'warning)))
  "The phd-modeline version control info face."
  :group 'phd-modeline)

(defface phd-modeline-vc-status-face
  `((t (:inherit 'font-lock-constant-face)))
  "The phd-modeline version control info face."
  :group 'phd-modeline)

(defface phd-modeline-mail-icon-face
  `((t (:inherit 'font-lock-string-face)))
  "The phd-modeline mail icon face"
  :group 'phd-modeline)

(defface phd-modeline-mail-status-face
  `((t (:inherit 'font-lock-string-face)))
  "The phd-modeline mail info face."
  :group 'phd-modeline)

(defface phd-modeline-bar-face
  `((t (:inherit 'phd-modeline-buffer-name-face)))
  "The phd-modeline bar face"
  :group 'phd-modeline)

(defcustom phd-modeline-height 25
  "The height of the mode-line (only in GUI)."
  :type 'integer
  :group 'phd-modeline)

(defcustom phd-modeline-bar-width 17
  "The width of the phd-modeline bar (only in GUI)."
  :type 'integer
  :set (lambda (sym val) (set sym (if (> val 0) val 1)))
  :group 'phd-modeline)

(defcustom phd-modeline-bar-data-func 'phd-ml/bar-tri-data
  "The data function for the bar image.
Choose between
- `phd-ml/bar-rec-data' - rectangular bar image
- `phd-ml/bar-tri-data' - triangular  bar image"
  :type 'function
  :set (lambda (sym val) (fset sym val))
  :group 'phd-modeline)

(defcustom phd-modeline-mail-update-interval 60
  "Update interval of phd-modeline's mail component."
  :type 'integer
  :group 'phd-modeline)

(defvar phd-modeline-mail-update-timer nil
  "Interval timer for phd-modeline's mail component.")

(defvar phd-modeline-mail-count-string ""
  "Count string for phd-modeline's mail component.")

(defun phd-ml/set-mu4e-command (&optional sym val op where)
  "Set `phd-modeline-mu4e-command' using custom variables:
- `phd-modeline-mu4e-mu-executable'
- `phd-modeline-mu4e-find'
- `phd-modeline-mu4e-unread-query'"
  (when (and (boundp 'phd-modeline-mu4e-mu-executable)
             (boundp 'phd-modeline-mu4e-find)
             (boundp 'phd-modeline-mu4e-unread-query))
    (let ((exec phd-modeline-mu4e-mu-executable)
          (subc phd-modeline-mu4e-find)
          (qry phd-modeline-mu4e-unread-query))
      (setq phd-modeline-mu4e-command
            (format "%s %s \"%s\"" exec subc qry)))))

(defcustom phd-modeline-mu4e-mu-executable (executable-find "mu")
  "The mu4e executable."
  :type 'string
  :set (lambda (sym val)
         (set sym val)
         (phd-ml/set-mu4e-command))
  :group 'phd-modeline)

(defcustom phd-modeline-mu4e-find "find"
  "The mu4e find sub-command."
  :type 'string
  :set (lambda (sym val)
         (set sym val)
         (phd-ml/set-mu4e-command))
  :group 'phd-modeline)

(defcustom phd-modeline-mu4e-unread-query "maildir:/INBOX AND flag:unread"
  "The mu4e query for counting the unread messages."
  :type 'string
  :set (lambda (sym val)
         (set sym val)
         (phd-ml/set-mu4e-command))
  :group 'phd-modeline)

(defcustom phd-modeline-mu4e-command (phd-ml/set-mu4e-command)
  "The full mu-find query built from custom variables:
- `phd-modeline-mu4e-mu-executable'
- `phd-modeline-mu4e-find'
- `phd-modeline-mu4e-unread-query'"
  :type 'string
  :group 'phd-modeline)

(defcustom phd-modeline-use-icons t
  "Enable all icon drawing functionality.
TODO: Not yet implemented."
  :type 'boolean
  :group 'phd-modeline)

(define-minor-mode phd-modeline-column-mode
  "Enable buffer column position functionality in phd-modeline."
  :group 'phd-modeline
  :global t
  :keymap phd-modeline-mode-map)

(define-minor-mode phd-modeline-percentage-mode
  "Enable buffer position percentage functionality in phd-modeline."
  :group 'phd-modeline
  :global t
  :keymap phd-modeline-mode-map)

(define-minor-mode phd-modeline-mail-mode
  "Enable unread mail indicator functionality in phd-modeline."
  :global t
  :group 'phd-modeline
  (setq phd-modeline-mail-count-string "")
  (and phd-modeline-mail-update-timer
       (cancel-timer phd-modeline-mail-update-timer))
  (when phd-modeline-mail-mode
    (setq phd-modeline-mail-update-timer
          (run-with-timer nil phd-modeline-mail-update-interval
                          'phd-modeline-mail-update-handler))
    (phd-modeline-mail-update-handler)))


(setq all-the-icons-scale-factor 1.1)
(setq all-the-icons-default-adjust 0)

;; for terminal mode
;; (unless window-system
;;   (defun all-the-icons-octicon (&rest _) "" "")
;;   (defun all-the-icons-faicon (&rest _) "" "")
;;   (defun all-the-icons-fileicon (&rest _) "" "")
;;   (defun all-the-icons-wicon (&rest _) "" "")
;;   (defun all-the-icons-alltheicon (&rest _) "" ""))


;; --- Utilities
;; Frame/window focussing
(defvar phd-ml/selected-window (frame-selected-window))

(defun phd-ml/set-selected-window (&rest _args)
  "Update the `phd-ml/selected-window' variable."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq phd-ml/selected-window (frame-selected-window))
    (force-mode-line-update)))

(defun phd-ml/reset-selected-window ()
  "Nil-ify the `phd-ml/selected-window' variable."
  (setq phd-ml/selected-window nil)
  (force-mode-line-update))

(defun phd-ml/selected-window-active-p ()
  "Return whether the current window is active."
  (eq phd-ml/selected-window (selected-window)))

(defadvice handle-switch-frame (after phd-ml/handle-switch-frame activate)
  "Update the `phd-ml/selected-window' variable."
  (phd-ml/set-selected-window))

(add-hook 'window-configuration-change-hook 'phd-ml/set-selected-window)
(add-hook 'buffer-list-update-hook #'phd-ml/set-selected-window)
(add-function
 :after after-focus-change-function
 (lambda ()
   (if (frame-focus-state)
       (phd-ml/set-selected-window)
     (phd-ml/reset-selected-window))))


;; Bitmapping
(defun phd-ml/bar-rec-data (width height)
  "Return PBM data for rectangular bar image with dimensions (WIDTH, HEIGHT)."
  (make-string (* width height) ?1))

(defun phd-ml/bar-tri-data (width height)
  "Return PBM data for triangular bar image with dimensions (WIDTH, HEIGHT)."
  (let ((data-list nil)
        (half-height (/ height 2))
        (repeats 1)
        (tri-width width)
        (block-width 0)
        (iter 0)
        (temp-str nil))
    (progn
      (if (< width height)
          (progn
            (setq repeats (/ half-height width))
            (when (> width half-height)
              (setq tri-width half-height)
              (setq block-width (- width half-height))))
        (setq tri-width half-height
              block-width (- width half-height)))
      ;; loop through upper half of the arrow
      (setq iter 0)
      (while (< iter tri-width)
        (setq temp-str (concat (make-string block-width ?1)
                               (make-string iter ?1)
                               (make-string (- tri-width iter) ?0))
              iter (1+ iter))
        (if (> repeats 1)
            (dotimes (number repeats)
              (push temp-str data-list))
          (push temp-str data-list)))
      ;; arrow tip
      (push (make-string width ?1) data-list)
      ;; loop through lower half of the arrow
      (setq iter 0)
      (while (< iter tri-width)
        (setq iter (1+ iter)
              temp-str (concat (make-string block-width ?1)
                               (make-string (- tri-width iter) ?1)
                               (make-string iter ?0)))
        (if (> repeats 1)
            (dotimes (number repeats)
              (push temp-str data-list))
          (push temp-str data-list))))
    (combine-and-quote-strings data-list "\n")))

(defun phd-ml/bar-image (face width height)
  "Create a PBM image of the modeline bar with dimensions (WIDTH, HEIGHT) using FACE."
  (when (and (display-graphic-p)
             (image-type-available-p 'pbm))
    (propertize
     " " 'display
     (let ((color (or (face-foreground face nil t) "None"))
           (bgcolor (or (face-background face nil t) "None")))
       (ignore-errors
         (create-image
          (concat (format "P1\n%i %i\n" width height)
                  (phd-modeline-bar-data-func width height)
                  "\n")
          'pbm t :foreground color :background bgcolor :ascent 'center))))))


;; Misc
(defun phd-ml/s-replace (old new s)
  "Replace OLD with NEW in string S."
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun phd-ml/s-count-regexp (regexp s &optional start end)
  "Count REGEXP expression in string S from START to END."
  (save-match-data
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (count-matches regexp (or start 1) (or end (point-max))))))


;; Mail
(defun phd-ml/mu4e-count-unread ()
  "Count all unread mails using `phd-modeline-mu4e-command'."
  (phd-ml/set-mu4e-command)
  (let* ((cmd phd-modeline-mu4e-command)
         (read (shell-command-to-string cmd)))
    (if (string-equal read "no matches for search expression\n")
        0
      (phd-ml/s-count-regexp "\n" read))
    ))

(defun phd-modeline-mail-update ()
  "Update unread mail count."
  (interactive)
  (let ((count (phd-ml/mu4e-count-unread)))
    (if (eq count 0)
        (setq count "")
      (setq count (format " · %s" count)))
    ;; (message "Mail count%s" count)
    (setq phd-modeline-mail-count-string count)))

(defun phd-modeline-mail-update-handler ()
  "Handler for updating unread mail count."
  (phd-modeline-mail-update)
  (sit-for 0))


;; --- Components (for phd-modeline-format)
;; Space fillers
(defun phd-modeline-whitespace (&optional number)
  "Add NUMBER of spaces in phd-modeline."
  (unless number (setq number 1))
  (list (propertize (make-string number ?\s)
                    'face (if (phd-ml/selected-window-active-p)
                              'phd-modeline-space-face
                            'phd-modeline-inactive-face))))

(defun phd-modeline-hairspace (&optional number)
  "Add NUMBER of spaces in phd-modeline."
  (unless number (setq number 1))
  (list (propertize (make-string number (string-to-char " ")) ;; " "
                    'face (if (phd-ml/selected-window-active-p)
                              'phd-modeline-space-face
                            'phd-modeline-inactive-face))))


(defun phd-modeline-space-between (&optional reserve)
  "Add spaces inbetween left-aligned and right-aligned components.
The right aligned components use pad RESERVE number of spaces on the right."
  (unless reserve (setq reserve 1))
  (propertize " "
   'face (if (phd-ml/selected-window-active-p)
             'phd-modeline-space-face
           'phd-modeline-inactive-face)
   'display `((space :align-to (- (+ right right-fringe right-margin)
                                  ,(+ reserve (string-width
                                         (if (listp mode-name)
                                             (car mode-name)
                                           mode-name))))))))

(defun phd-modeline-dot-separator (&optional pad-l pad-r)
  "Add dot character in phd-modeline.
Optionally pad the separator by PAD-L on the left, PAD-R on the right."
  (unless pad-l (setq pad-l 0))
  (unless pad-r (setq pad-r 0))
  (list
   (phd-modeline-whitespace pad-l)
   (propertize "·"
               'face (if (phd-ml/selected-window-active-p)
                         'phd-modeline-space-face
                       'phd-modeline-inactive-face))
   (phd-modeline-whitespace pad-r)))


;; Bar
(defvar phd-modeline-active-bar nil)
(defvar phd-modeline-inactive-bar nil)

(defsubst phd-modeline-bar ()
  "The default bar regulates the height of the mode-line in GUI."
  (unless (and phd-modeline-active-bar phd-modeline-inactive-bar)
    (let ((width phd-modeline-bar-width)
          (height phd-modeline-height ))
      (when (and (numberp width) (numberp height))
        (setq phd-modeline-active-bar
              (phd-ml/bar-image 'phd-modeline-bar-face width height)
              phd-modeline-inactive-bar
              (phd-ml/bar-image 'phd-modeline-inactive-face width height)))))
  (if (phd-ml/selected-window-active-p)
      phd-modeline-active-bar
    phd-modeline-inactive-bar))


;; Text info
(defun phd-modeline-buffer-name ()
  "Format buffer name in phd-modeline depending on its state."
  (list
   (cond
    (buffer-read-only
     (propertize "%b"
                 'face (if (phd-ml/selected-window-active-p)
                           'phd-modeline-buffer-read-only-face
                         'phd-modeline-inactive-face)
                 'mouse-face 'phd-modeline-highlight-face
                 'help-echo "Buffer name\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                 'local-map mode-line-buffer-identification-keymap))
    ((buffer-modified-p)
     (propertize "%b"
                 'face (if (phd-ml/selected-window-active-p)
                           'phd-modeline-buffer-modified-face
                         'phd-modeline-inactive-face)
                 'mouse-face 'phd-modeline-highlight-face
                 'help-echo "Buffer name\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                 'local-map mode-line-buffer-identification-keymap))
    (t
     (propertize "%b"
                 'face (if (phd-ml/selected-window-active-p)
                           'phd-modeline-buffer-name-face
                         'phd-modeline-inactive-face)
                 'mouse-face 'phd-modeline-highlight-face
                 'help-echo "Buffer name\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                 'local-map mode-line-buffer-identification-keymap)))))

(defun phd-modeline-buffer-position ()
  "Format buffer position in phd-modeline depending on its state."
  (list
   (propertize "L:[%3l]"
               'face (if (phd-ml/selected-window-active-p)
                         'phd-modeline-buffer-line-face
                       'phd-modeline-inactive-face))
   (when phd-modeline-column-mode
     (propertize " · C:[%3c]"
                 'face (if (phd-ml/selected-window-active-p)
                           'phd-modeline-buffer-column-face
                         'phd-modeline-inactive-face)))
   (when phd-modeline-percentage-mode
       (propertize " @"
                   'face (if (phd-ml/selected-window-active-p)
                             'mode-line
                           'phd-modeline-inactive-face)))
   (when phd-modeline-percentage-mode
       (propertize "%p "
                   'face (if (phd-ml/selected-window-active-p)
                             'phd-modeline-buffer-percentage-face
                           'phd-modeline-inactive-face)))))

(defun phd-modeline-media-info ()
  "Show image dimension when in `image-mode'."
  (when (eq major-mode 'image-mode)
    (let ((size (image-size (image-get-display-property) :pixels)))
      (list
       (phd-modeline-whitespace)
       (format " %dx%d " (car size) (cdr size))))))

(defun phd-modeline-major-mode ()
  "Format major mode name in phd-modeline."
  (list
   (propertize mode-name
               'face (if (phd-ml/selected-window-active-p)
                         'phd-modeline-mode-face
                       'phd-modeline-inactive-face))))

(defun phd-modeline-flycheck-status ()
  "Flycheck status info for phd-modeline."
  (let* ((text
          (pcase flycheck-last-status-change
            (`finished
             (if flycheck-current-errors
                 (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                (+ (or .warning 0) (or .error 0)))))
                   (propertize (format "⚠ %s" count)
                               'face (if (phd-ml/selected-window-active-p)
                                      'phd-modeline-flycheck-warning-face
                                   'phd-modeline-inactive-face)))
               (propertize "✔"
                           'face (if (phd-ml/selected-window-active-p)
                                      'phd-modeline-flycheck-success-face
                                   'phd-modeline-inactive-face))))
            (`running     "⟲")
            (`errored     (propertize "✖" 'face (if (phd-ml/selected-window-active-p)
                                                    'phd-modeline-flycheck-error-face
                                                  'phd-modeline-inactive-face)))
            (`interrupted (propertize "⛔" 'face (if (phd-ml/selected-window-active-p)
                                                    'phd-modeline-flycheck-error-face
                                                   'phd-modeline-inactive-face)))
            (`not-checked "")
            (`no-checker  "")
            (`suspicious  ""))))
    (list
     (propertize text
                'help-echo "Show Flycheck Errors"
                'mouse-face 'phd-modeline-highlight-face
                'local-map (make-mode-line-mouse-map
                            'mouse-1 (lambda () (interactive) (flycheck-list-errors))))
     (phd-modeline-whitespace))))

(defvar vc-status-symbol-alist
  '((up-to-date       . nil)
    (edited           . "!")
    (needs-update     . "@")
    (needs-merge      . "x")
    (unlocked-changes . "&")
    (added            . "+")
    (removed          . "-")
    (conflict         . "#")
    (missing          . "%")
    (ignored          . ";")
    (unregistered     . "?"))
  "String symbol map for `vs-state' output.")

(defun phd-modeline-vc-status ()
  "Fetch git repository status info for phd-modeline."
  (when-let ((vc vc-mode)
             (vcb (vc-backend buffer-file-name))
             (vcs (vc-state buffer-file-name)))
    (setq vcs-symbol (cdr (assoc vcs vc-status-symbol-alist)))
    (setq vcbranch (phd-ml/s-replace (format "%s" vcb) "" vc))
    (setq vcbranch (phd-ml/s-replace ":" "" vcbranch))
    (setq vcbranch (phd-ml/s-replace "-" "" vcbranch))
    (list
     (propertize vcbranch
                 'face (if (phd-ml/selected-window-active-p)
                           'phd-modeline-vc-branch-face
                         'phd-modeline-inactive-face))
     (when vcs-symbol
       (propertize (format "[%s]" vcs-symbol)
                   'face (if (phd-ml/selected-window-active-p)
                             'phd-modeline-vc-status-face
                           'phd-modeline-inactive-face)))
     (phd-modeline-whitespace))))

(defun phd-modeline-mail-status ()
  "Fetch mu4e status info for phd-modeline."
  (when phd-modeline-mail-mode
    (let ((count phd-modeline-mail-count-string))
      (list
       (propertize count
                   'face (if (phd-ml/selected-window-active-p)
                             'phd-modeline-mail-status-face
                           'phd-modeline-inactive-face))))))

 
;; Icons
(defun phd-modeline-buffer-lock-icon ()
  "Iconify read-only buffer in phd-modeline."
  (when buffer-read-only
    (list
     (all-the-icons-octicon "lock"
                            :face (if (phd-ml/selected-window-active-p)
                                      'phd-modeline-buffer-read-only-face
                                    'phd-modeline-inactive-face))
     (phd-modeline-whitespace))))

(defun phd-modeline-buffer-modified-icon ()
  "Iconify modified buffer in phd-modeline."
  (when (and (buffer-modified-p) (not buffer-read-only))
    (list
     (phd-modeline-whitespace)
     (all-the-icons-faicon "floppy-o"
                            :face (if (phd-ml/selected-window-active-p)
                                      'phd-modeline-buffer-modified-face
                                    'phd-modeline-inactive-face)))))

(defvar vc-status-icon-set-alist
  '((Git . all-the-icons-alltheicon)
    (SVN . all-the-icons-fileicon)
    (Hg . all-the-icons-fileicon))
  "Icon map for `vs-backend' icons.")

(defun phd-modeline-vc-icon (&optional with-logo with-sep with-branch)
  "Iconify git repository status in phd-modeline if WITH-LOGO is positive.
Include separator if WITH-SEP is positive.
Include branch icon if WITH-BRANCH is positive.
If no arguments are given, only return logo icon."
  (unless with-logo (setq with-logo 0))
  (unless with-sep (setq with-sep 0))
  (unless with-branch (setq with-branch 0))
  (when (and (not with-logo) (not with-branch)) (setq with-logo 1))
  (when-let ((vc vc-mode)
             (vcb (vc-backend buffer-file-name))
             (vcs (vc-state buffer-file-name)))
    (fset 'vcb-icon-set (cdr (assoc vcb vc-status-icon-set-alist)))
    (setq vc-icons nil)
    (when (> with-logo 0)
      (add-to-list 'vc-icons
                   (propertize (format "%s" (vcb-icon-set (downcase (format "%s" vcb))))
                               'face (if (phd-ml/selected-window-active-p)
                                         'phd-modeline-vc-icon-face
                                       'phd-modeline-inactive-face))
                   t))
    (when (> with-sep 0)
      (add-to-list 'vc-icons (phd-modeline-dot-separator 2 2) t))
    (when (> with-branch 0)
      (add-to-list 'vc-icons
                   (propertize (all-the-icons-octicon "git-branch")
                               'face (if (phd-ml/selected-window-active-p)
                                         'phd-modeline-vc-branch-face
                                       'phd-modeline-inactive-face))
                   t))
    vc-icons))

(defun phd-modeline-mail-icon ()
  "Iconify mail status in phd-modeline."
  (when phd-modeline-mail-mode
    (list
     (propertize (all-the-icons-octicon "mail")
                 'face (if (phd-ml/selected-window-active-p)
                           'phd-modeline-mail-icon-face
                         'phd-modeline-inactive-face)))))

(defun phd-modeline-mode-icon ()
  "Iconify major mode in phd-modeline."
  (list
   (if (phd-ml/selected-window-active-p)
       (all-the-icons-icon-for-mode major-mode)
     (all-the-icons-icon-for-mode major-mode
                                  :face 'phd-modeline-inactive-face))))


;; --- Formats
(defvar phd-modeline-format-original
  '("%e" mode-line-front-space
    mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
    mode-line-frame-identification mode-line-buffer-identification "   "
    mode-line-position
    (vc-mode vc-mode) "  "
    mode-name
    mode-line-misc-info mode-line-end-spaces)
  "Original format of the mode-line (except only the major mode is shown).")

(defvar phd-modeline-format-default
  (list
   '(:eval (phd-modeline-bar))
   '(:eval (phd-modeline-whitespace))
   '(:eval (phd-modeline-buffer-name))
   '(:eval (phd-modeline-whitespace))
   '(:eval (phd-modeline-buffer-position))
   '(:eval (phd-modeline-whitespace))
   '(:eval (phd-modeline-space-between 2))
   '(:eval (phd-modeline-major-mode))
   '(:eval (phd-modeline-whitespace)))
  "Default format of the phd-modeline.")

(defcustom phd-modeline-format nil
  "The customizable phd-modeline format."
  :type (list))


;; --- Customization
;; My personal configuration:
;; (setq phd-modeline-column-mode t
;;       phd-modeline-mail-mode t)
;; (setq phd-modeline-format
;;       (list
;;        '(:eval (phd-modeline-bar))
;;        '(:eval (phd-modeline-whitespace))
;;        '(:eval (phd-modeline-buffer-lock-icon))
;;        '(:eval (phd-modeline-buffer-name))
;;        '(:eval (phd-modeline-buffer-modified-icon))
;;        '(:eval (phd-modeline-whitespace))
;;        '(:eval (phd-modeline-buffer-position))
;;        '(:eval (phd-modeline-media-info))
;;        '(:eval (phd-modeline-whitespace))
;;        '(:eval (phd-modeline-flycheck-status))
;;        '(:eval (phd-modeline-whitespace 4))
;;        '(:eval (phd-modeline-vc-icon 1 1 1))
;;        '(:eval (phd-modeline-vc-status))
;;        '(:eval (phd-modeline-mail-icon))
;;        '(:eval (phd-modeline-mail-status))
;;        '(:eval (phd-modeline-whitespace))
;;        '(:eval (phd-modeline-space-between 4))
;;        '(:eval (phd-modeline-mode-icon))
;;        '(:eval (phd-modeline-whitespace))
;;        '(:eval (phd-modeline-major-mode))
;;        '(:eval (phd-modeline-whitespace))
;;        ))


(provide 'phd-modeline)
;;; phd-modeline.el ends here
