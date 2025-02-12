;;; phd-ark-modeline.el --- My modeline customization
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
;; (add-to-list 'load-path "~/phd-ark-modeline/")
;; (require 'phd-ark-modeline)
;; (phd-ark-modeline-mode 1)
;; (global-set-key (kbd "C-x |") 'phd-ark-modeline-mode)
;;
;; or
;;
;; (use-package phd-ark-modeline
;;   :ensure nil
;;   :load-path "~/phd-ark-modeline/"
;;   :after all-the-icons
;;   :hook (after-init . phd-ark-modeline-mode)
;;   :bind (("C-x |" . phd-ark-modeline-mode))
;;   :config (...))
;;
;;; Code:
(require 'all-the-icons-nerd-fonts)
(require 'flycheck)


(defgroup phd-ark-modeline nil
  "A minimal mode-line."
  :group 'mode-line
  :link '(url-link :tag "Homepage" "https://github.com/phdenzel/phd-ark-modeline"))


;; --- Mode map
(defvar phd-ark-modeline-mode-map (make-sparse-keymap))

;; --- Style
(defface phd-ark-modeline-buffer-name-face
  `((t (:inherit 'minibuffer-prompt)))
  "The phd-ark-modeline normal buffer name face."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-pad-face
  `((t (:inherit 'default)))
  "The phd-ark-modeline face of empty space."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-space-face
  `((t (:inherit 'mode-line)))
  "The phd-ark-modeline face of empty space."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-buffer-modified-face
  `((t (:inherit 'font-lock-function-name-face)))
  "The phd-ark-modeline modified buffer name face."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-buffer-read-only-face
  `((t (:inherit 'font-lock-keyword-face)))
  "The phd-ark-modeline read-only buffer name face."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-buffer-line-face
  `((t (:inherit 'mode-line)))
  "The phd-ark-modeline buffer line face."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-buffer-column-face
  `((t (:inherit 'mode-line)))
  "The phd-ark-modeline buffer column face."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-buffer-percentage-face
  `((t (:inherit 'mode-line)))
  "The phd-ark-modeline buffer percentage face."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-mode-face
  `((t (:inherit 'mode-line)))
  "The phd-ark-modeline buffer percentage face."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-inactive-face
  `((t (:inherit 'mode-line-inactive)))
  "The phd-ark-modeline face for inactive frames."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-highlight-face
  `((t (:inherit 'mode-line-highlight)))
  "The phd-ark-modeline highlight face."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-flycheck-success-face
  `((t (:inherit 'success)))
  "The phd-ark-modeline flycheck face for success."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-flycheck-warning-face
  `((t (:inherit 'warning)))
  "The phd-ark-modeline flycheck face for warnings."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-flycheck-error-face
  `((t (:inherit 'success)))
  "The phd-ark-modeline flycheck face for errors."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-vc-icon-face
  `((t (:inherit 'font-lock-keyword-face)))
  "The phd-ark-modeline version control icon face."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-vc-branch-face
  `((t (:inherit 'warning)))
  "The phd-ark-modeline version control info face."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-vc-status-face
  `((t (:inherit 'font-lock-constant-face)))
  "The phd-ark-modeline version control info face."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-mail-icon-face
  `((t (:inherit 'font-lock-string-face)))
  "The phd-ark-modeline mail icon face."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-mail-status-face
  `((t (:inherit 'font-lock-string-face)))
  "The phd-ark-modeline mail info face."
  :group 'phd-ark-modeline)

(defface phd-ark-modeline-bar-face
  `((t (:inherit 'minibuffer-prompt)))
  "The phd-ark-modeline bar face."
  :group 'phd-ark-modeline)


(defcustom phd-ark-modeline-height 25
  "The height of the mode-line (only in GUI)."
  :type 'integer
  :group 'phd-ark-modeline)

(defcustom phd-ark-modeline-bar-width 16
  "The width of the phd-ark-modeline bar (only in GUI)."
  :type 'integer
  :set (lambda (sym val) (set sym (if (> val 0) val 1)))
  :group 'phd-ark-modeline)

(defcustom phd-ark-modeline-mail-update-interval 60
  "Update interval of phd-ark-modeline's mail component."
  :type 'integer
  :group 'phd-ark-modeline)

(defvar phd-ark-modeline-mail-update-timer nil
  "Interval timer for phd-ark-modeline's mail component.")

(defvar phd-ark-modeline-mail-count-string ""
  "Count string for phd-ark-modeline's mail component.")

(defun phd-ark-ml/set-mu4e-command (&optional sym val op where)
  "Set `phd-ark-modeline-mu4e-command' using custom variables.
The arguments SYM VAL OP WHERE are dummy variables.
Choose from
- `phd-ark-modeline-mu4e-mu-executable'
- `phd-ark-modeline-mu4e-find'
- `phd-ark-modeline-mu4e-unread-query'"
  (when (and (boundp 'phd-ark-modeline-mu4e-mu-executable)
             (boundp 'phd-ark-modeline-mu4e-find)
             (boundp 'phd-ark-modeline-mu4e-unread-query))
    (let ((exec phd-ark-modeline-mu4e-mu-executable)
          (subc phd-ark-modeline-mu4e-find)
          (qry phd-ark-modeline-mu4e-unread-query))
      (setq phd-ark-modeline-mu4e-command
            (format "%s %s \"%s\"" exec subc qry)))))

(defcustom phd-ark-modeline-mu4e-mu-executable (executable-find "mu")
  "The mu4e executable."
  :type 'string
  :set (lambda (sym val)
         (set sym val)
         (phd-ark-ml/set-mu4e-command))
  :group 'phd-ark-modeline)

(defcustom phd-ark-modeline-mu4e-find "find"
  "The mu4e find sub-command."
  :type 'string
  :set (lambda (sym val)
         (set sym val)
         (phd-ark-ml/set-mu4e-command))
  :group 'phd-ark-modeline)

(defcustom phd-ark-modeline-mu4e-unread-query "maildir:/INBOX AND flag:unread"
  "The mu4e query for counting the unread messages."
  :type 'string
  :set (lambda (sym val)
         (set sym val)
         (phd-ark-ml/set-mu4e-command))
  :group 'phd-ark-modeline)

(defcustom phd-ark-modeline-mu4e-command (phd-ark-ml/set-mu4e-command)
  "The full mu-find query built from custom variables:
- `phd-ark-modeline-mu4e-mu-executable'
- `phd-ark-modeline-mu4e-find'
- `phd-ark-modeline-mu4e-unread-query'"
  :type 'string
  :group 'phd-ark-modeline)

(defcustom phd-ark-modeline-use-icons t
  "Enable all icon drawing functionality.
TODO: Not yet implemented."
  :type 'boolean
  :group 'phd-ark-modeline)

(define-minor-mode phd-ark-modeline-column-mode
  "Enable buffer column position functionality in phd-ark-modeline."
  :group 'phd-ark-modeline
  :global t
  :keymap phd-ark-modeline-mode-map)

(define-minor-mode phd-ark-modeline-percentage-mode
  "Enable buffer position percentage functionality in phd-ark-modeline."
  :group 'phd-ark-modeline
  :global t
  :keymap phd-ark-modeline-mode-map)

(define-minor-mode phd-ark-modeline-mail-mode
  "Enable unread mail indicator functionality in phd-ark-modeline."
  :group 'phd-ark-modeline
  :global t
  (setq phd-ark-modeline-mail-count-string "")
  (and phd-ark-modeline-mail-update-timer
       (cancel-timer phd-ark-modeline-mail-update-timer))
  (when phd-ark-modeline-mail-mode
    (setq phd-ark-modeline-mail-update-timer
          (run-with-timer nil phd-ark-modeline-mail-update-interval
                          'phd-ark-modeline-mail-update-handler))
    (phd-ark-modeline-mail-update-handler))
  )


;; --- Utilities
;; Frame/window focussing
(defvar phd-ark-ml/selected-window (frame-selected-window))

(defun phd-ark-ml/set-selected-window (&rest _args)
  "Update the `phd-ark-ml/selected-window' variable."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq phd-ark-ml/selected-window (frame-selected-window))
    (force-mode-line-update)))

(defun phd-ark-ml/reset-selected-window ()
  "Nil-ify the `phd-ark-ml/selected-window' variable."
  (setq phd-ark-ml/selected-window nil)
  (force-mode-line-update))

(defun phd-ark-ml/selected-window-active-p ()
  "Return whether the current window is active."
  (eq phd-ark-ml/selected-window (selected-window)))

(defadvice handle-switch-frame (after phd-ark-ml/handle-switch-frame activate)
  "Update the `phd-ark-ml/selected-window' variable."
  (phd-ark-ml/set-selected-window))

(add-hook 'window-configuration-change-hook 'phd-ark-ml/set-selected-window)
(add-hook 'buffer-list-update-hook #'phd-ark-ml/set-selected-window)
(add-function
 :after after-focus-change-function
 (lambda ()
   (if (frame-focus-state)
       (phd-ark-ml/set-selected-window)
     (phd-ark-ml/reset-selected-window))))


;; Bitmapping
(defun phd-ark-ml/bar-rec-data (width height)
  "Return PBM data for rectangular bar image with dimensions (WIDTH, HEIGHT)."
  (make-string (* width height) ?1))

(defun phd-ark-ml/bar-dia-data (width height)
  "Return PBM data for a bar image cut diagonally with dimensions (WIDTH, HEIGHT)."
  (let ((body ""))
    (dotimes (y height)
      (dotimes (x width)
        (setq body (concat body (if (<= (* x height) (* y width)) "1 " "0 "))))
      (setq body (concat body "\n")))
    body))

(defun phd-ark-ml/bar-sig-data (width height)
  "Return PBM data for a sigmoid-like bar with dimensions (WIDTH, HEIGHT)."
  (let ((body ""))
    (dotimes (y height)
      (dotimes (x width)
        (let ((normalized-x (/ (float x) width))
              (normalized-y (/ (float y) height)))
          (setq body (concat body (if (<= normalized-y (/ 1 (+ 1 (exp (- (* 10 (- normalized-x 0.5)))))))
                              "0 " "1 ")))))
      (setq body (concat body "\n")))
    body))

(defun phd-ark-ml/bar-tri-data (width height)
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

(defcustom phd-ark-modeline-bar-data-func 'phd-ark-ml/bar-tri-data
  "The data function for the bar image.
Choose between
- `phd-ark-ml/bar-rec-data' - rectangular bar image
- `phd-ark-ml/bar-dia-data' - diagonal bar image
- `phd-ark-ml/bar-sig-data' - sigmoid bar image
- `phd-ark-ml/bar-tri-data' - triangular  bar image"
  :type 'function
  :set (lambda (sym val) (fset sym val))
  :group 'phd-ark-modeline)

(defun phd-ark-ml/bar-image (face width height)
  "Create a PBM image of the modeline bar at (WIDTH, HEIGHT) using FACE."
  (when (and (display-graphic-p)
             (image-type-available-p 'pbm))
    (propertize
     " " 'display
     (let ((color (or (face-foreground face nil t) "None"))
           (bgcolor (or (face-background face nil t) "None")))
       (ignore-errors
         (create-image
          (concat (format "P1\n%i %i\n" width height)
                  (phd-ark-modeline-bar-data-func width height)
                  "\n")
          'pbm t :foreground color :background bgcolor :ascent 'center))))))

;; Misc
(defun phd-ark-ml/s-replace (old new s)
  "Replace OLD with NEW in string S."
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun phd-ark-ml/s-count-regexp (regexp s &optional start end)
  "Count REGEXP expression in string S from START to END."
  (save-match-data
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (count-matches regexp (or start 1) (or end (point-max))))))


;; Mail
(defun phd-ark-ml/mu4e-count-unread ()
  "Count all unread mails using `phd-ark-modeline-mu4e-command'."
  (phd-ark-ml/set-mu4e-command)
  (let* ((cmd phd-ark-modeline-mu4e-command)
         (read (shell-command-to-string cmd)))
    (if (string-equal read "no matches for search expression\n")
        0
      (phd-ark-ml/s-count-regexp "\n" read))
    ))

(defun phd-ark-modeline-mail-update ()
  "Update unread mail count."
  (interactive)
  (let ((count (phd-ark-ml/mu4e-count-unread)))
    (if (eq count 0)
        (setq count "")
      (setq count (format " · %s" count)))
    ;; (message "Mail count%s" count)
    (setq phd-ark-modeline-mail-count-string count))
  )

(defun phd-ark-modeline-mail-update-handler ()
  "Handler for updating unread mail count."
  (phd-ark-modeline-mail-update))


;; --- Components (for phd-ark-modeline-format)
;; Space fillers
(defun phd-ark-modeline-padding (&optional number)
  "Add NUMBER of padding spaces in phd-ark-modeline."
  (unless number (setq number 1))
  (list (propertize (make-string number ?\s)
                    'face 'phd-ark-modeline-pad-face)))

(defun phd-ark-modeline-whitespace (&optional number)
  "Add NUMBER of spaces in phd-ark-modeline."
  (unless number (setq number 1))
  (list (propertize (make-string number ?\s)
                    'face (if (phd-ark-ml/selected-window-active-p)
                              'phd-ark-modeline-space-face
                            'phd-ark-modeline-inactive-face))))

(defun phd-ark-modeline-hairspace (&optional number)
  "Add NUMBER of spaces in phd-ark-modeline."
  (unless number (setq number 1))
  (list (propertize (make-string number (string-to-char " ")) ;; or hairspace " "
                    'face (if (phd-ark-ml/selected-window-active-p)
                              'phd-ark-modeline-space-face
                            'phd-ark-modeline-inactive-face))))


(defun phd-ark-modeline-space-between (&optional reserve)
  "Add spaces inbetween left-aligned and right-aligned components.
The right aligned components use pad RESERVE number of spaces on the right."
  (unless reserve (setq reserve 1))
  (propertize " "
   'face (if (phd-ark-ml/selected-window-active-p)
             'phd-ark-modeline-space-face
           'phd-ark-modeline-inactive-face)
   'display `((space :align-to (- (+ right right-fringe right-margin)
                                  ,(+ reserve (string-width
                                         (if (listp mode-name)
                                             (car mode-name)
                                           mode-name))))))))

(defun phd-ark-modeline-dot-separator (&optional pad-l pad-r)
  "Add dot character in phd-ark-modeline.
Optionally pad the separator by PAD-L on the left, PAD-R on the right."
  (unless pad-l (setq pad-l 0))
  (unless pad-r (setq pad-r 0))
  (list
   (phd-ark-modeline-whitespace pad-l)
   (propertize "·"
               'face (if (phd-ark-ml/selected-window-active-p)
                         'phd-ark-modeline-space-face
                       'phd-ark-modeline-inactive-face))
   (phd-ark-modeline-whitespace pad-r)))


;; Bar
(defvar phd-ark-modeline-active-bar nil)
(defvar phd-ark-modeline-inactive-bar nil)

(defsubst phd-ark-modeline-bar ()
  "The default bar regulates the height of the mode-line in GUI."
  (unless (and phd-ark-modeline-active-bar phd-ark-modeline-inactive-bar)
    (let ((width phd-ark-modeline-bar-width)
          (height phd-ark-modeline-height ))
      (when (and (numberp width) (numberp height))
        (setq phd-ark-modeline-active-bar (phd-ark-ml/bar-image 'phd-ark-modeline-bar-face width height)
              phd-ark-modeline-inactive-bar (phd-ark-ml/bar-image 'phd-ark-modeline-inactive-face width height)))))
  (if (phd-ark-ml/selected-window-active-p)
      phd-ark-modeline-active-bar
    phd-ark-modeline-inactive-bar))

(defun phd-ark-modeline-marker (&optional fnt height)
  "A font-based bar for the mode-line with FNT."
  (unless fnt (setq fnt (all-the-icons-nerd-pl "left-hard-divider")))
  (unless height (setq height phd-ark-modeline-height))
  (list
   (propertize fnt
	       'height height
	       'face (if (phd-ark-ml/selected-window-active-p)
			 'phd-ark-modeline-buffer-name-face
		       'phd-ark-modeline-inactive-face))
   (phd-ark-modeline-whitespace)
   ))

;; Text info
(defun phd-ark-modeline-buffer-name ()
  "Format buffer name in phd-ark-modeline depending on its state."
  (list
   (cond
    (buffer-read-only
     (propertize "%b"
                 'face (if (phd-ark-ml/selected-window-active-p)
                           'phd-ark-modeline-buffer-read-only-face
                         'phd-ark-modeline-inactive-face)
                 'mouse-face 'phd-ark-modeline-highlight-face
                 'help-echo "Buffer name\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                 'local-map mode-line-buffer-identification-keymap))
    ((buffer-modified-p)
     (propertize "%b"
                 'face (if (phd-ark-ml/selected-window-active-p)
                           'phd-ark-modeline-buffer-modified-face
                         'phd-ark-modeline-inactive-face)
                 'mouse-face 'phd-ark-modeline-highlight-face
                 'help-echo "Buffer name\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                 'local-map mode-line-buffer-identification-keymap))
    (t
     (propertize "%b"
                 'face (if (phd-ark-ml/selected-window-active-p)
                           'phd-ark-modeline-buffer-name-face
                         'phd-ark-modeline-inactive-face)
                 'mouse-face 'phd-ark-modeline-highlight-face
                 'help-echo "Buffer name\nmouse-1: Previous buffer\nmouse-3: Next buffer"
                 'local-map mode-line-buffer-identification-keymap)))))

(defun phd-ark-modeline-buffer-position ()
  "Format buffer position in phd-ark-modeline depending on its state."
  (list
   (propertize "L:[%3l]"
               'face (if (phd-ark-ml/selected-window-active-p)
                         'phd-ark-modeline-buffer-line-face
                       'phd-ark-modeline-inactive-face))
   ;; (when phd-ark-modeline-column-mode
   ;;   (phd-ark-modeline-dot-separator 1 1))
   (when phd-ark-modeline-column-mode
     (propertize "·C:[%3c]"
                 'face (if (phd-ark-ml/selected-window-active-p)
                           'phd-ark-modeline-buffer-column-face
                         'phd-ark-modeline-inactive-face)))
   (when phd-ark-modeline-percentage-mode
       (propertize " @"
                   'face (if (phd-ark-ml/selected-window-active-p)
                             'mode-line
                           'phd-ark-modeline-inactive-face)))
   (when phd-ark-modeline-percentage-mode
       (propertize "%p "
                   'face (if (phd-ark-ml/selected-window-active-p)
                             'phd-ark-modeline-buffer-percentage-face
                           'phd-ark-modeline-inactive-face)))))

(defun phd-ark-modeline-media-info ()
  "Show image dimension when in `image-mode'."
  (when (eq major-mode 'image-mode)
    (let ((size (image-size (image-get-display-property) :pixels)))
      (list
       (phd-ark-modeline-whitespace)
       (format " %dx%d " (car size) (cdr size))))))

(defun phd-ark-modeline-major-mode ()
  "Format major mode name in phd-ark-modeline."
  (let* ((modename-string
          (pcase mode-name
            ((pred stringp) mode-name)
            ((pred listp) (car mode-name)))))
    (list
     (propertize modename-string
                 'face (if (phd-ark-ml/selected-window-active-p)
                           'phd-ark-modeline-mode-face
                         'phd-ark-modeline-inactive-face)))))

(defun phd-ark-modeline-flycheck-status ()
  "Flycheck status info for phd-ark-modeline."
  (let* ((text
          (pcase flycheck-last-status-change
            (`finished
             (if flycheck-current-errors
                 (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                (+ (or .warning 0) (or .error 0)))))
                   (propertize (format "⚠ %s" count)
                               'face (if (phd-ark-ml/selected-window-active-p)
                                      'phd-ark-modeline-flycheck-warning-face
                                   'phd-ark-modeline-inactive-face)))
               (propertize "✔"
                           'face (if (phd-ark-ml/selected-window-active-p)
                                      'phd-ark-modeline-flycheck-success-face
                                   'phd-ark-modeline-inactive-face))))
            (`running     "⟲")
            (`errored     (propertize "✖" 'face (if (phd-ark-ml/selected-window-active-p)
                                                    'phd-ark-modeline-flycheck-error-face
                                                  'phd-ark-modeline-inactive-face)))
            (`interrupted (propertize "⛔" 'face (if (phd-ark-ml/selected-window-active-p)
                                                    'phd-ark-modeline-flycheck-error-face
                                                   'phd-ark-modeline-inactive-face)))
            (`not-checked "")
            (`no-checker  "")
            (`suspicious  ""))))
    (list
     (propertize text
                'help-echo "Show Flycheck Errors"
                'mouse-face 'phd-ark-modeline-highlight-face
                'local-map (make-mode-line-mouse-map
                            'mouse-1 (lambda () (interactive) (flycheck-list-errors))))
     (phd-ark-modeline-whitespace))))

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

(defun phd-ark-modeline-vc-status ()
  "Fetch git repository status info for phd-ark-modeline."
  (when-let ((vc vc-mode)
             (vcb (vc-backend buffer-file-name))
             (vcs (vc-state buffer-file-name)))
    (defvar vcs-symbol (cdr (assoc vcs vc-status-symbol-alist)))
    (defvar phd-ark-ml/vcbranch (phd-ark-ml/s-replace (format "%s" vcb) "" vc))
    (setq phd-ark-ml/vcbranch (phd-ark-ml/s-replace ":" "" phd-ark-ml/vcbranch))
    (setq phd-ark-ml/vcbranch (phd-ark-ml/s-replace "-" "" phd-ark-ml/vcbranch))
    (list
     (propertize phd-ark-ml/vcbranch
                 'face (if (phd-ark-ml/selected-window-active-p)
                           'phd-ark-modeline-vc-branch-face
                         'phd-ark-modeline-inactive-face))
     (when vcs-symbol
       (propertize (format "[%s]" vcs-symbol)
                   'face (if (phd-ark-ml/selected-window-active-p)
                             'phd-ark-modeline-vc-status-face
                           'phd-ark-modeline-inactive-face)))
     (phd-ark-modeline-whitespace))))

(defun phd-ark-modeline-mail-status ()
  "Fetch mu4e status info for phd-ark-modeline."
  (when phd-ark-modeline-mail-mode
    (let ((count phd-ark-modeline-mail-count-string))
      (list
       (propertize count
                   'face (if (phd-ark-ml/selected-window-active-p)
                             'phd-ark-modeline-mail-status-face
                           'phd-ark-modeline-inactive-face))))))

 
;; Icons
(defun phd-ark-modeline-buffer-lock-icon ()
  "Iconify read-only buffer in phd-ark-modeline."
  (when buffer-read-only
    (list
     (all-the-icons-nerd-oct "lock"
                            :face (if (phd-ark-ml/selected-window-active-p)
                                      'phd-ark-modeline-buffer-read-only-face
                                    'phd-ark-modeline-inactive-face))
     (phd-ark-modeline-whitespace))))

(defun phd-ark-modeline-buffer-modified-icon ()
  "Iconify modified buffer in phd-ark-modeline."
  (when (and (buffer-modified-p) (not buffer-read-only))
    (list
     (phd-ark-modeline-whitespace)
     (all-the-icons-nerd-fa "floppy-o"
                            :face (if (phd-ark-ml/selected-window-active-p)
                                      'phd-ark-modeline-buffer-modified-face
                                    'phd-ark-modeline-inactive-face)))))

(defvar vc-status-icon-set-alist
  '((Git . all-the-icons-nerd-md)
    (SVN . all-the-icons-fileicon)
    (Hg . all-the-icons-fileicon))
  "Icon map for `vs-backend' icons.")

(defun phd-ark-modeline-vc-icon (&optional with-logo with-sep with-branch)
  "Iconify git repository status in phd-ark-modeline if WITH-LOGO is positive.
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
    (defvar vc-icons nil)
    (when (> with-logo 0)
      (add-to-list 'vc-icons
                   (propertize (format "%s" (vcb-icon-set (downcase (format "%s" vcb))))
                               'face (if (phd-ark-ml/selected-window-active-p)
                                         'phd-ark-modeline-vc-icon-face
                                       'phd-ark-modeline-inactive-face))
                   t))
    (when (> with-sep 0)
      (add-to-list 'vc-icons (phd-ark-modeline-dot-separator 2 2) t))
    (when (> with-branch 0)
      (add-to-list 'vc-icons
                   (propertize (all-the-icons-nerd-oct "git-branch")
                               'face (if (phd-ark-ml/selected-window-active-p)
                                         'phd-ark-modeline-vc-branch-face
                                       'phd-ark-modeline-inactive-face))
                   t))
    vc-icons))

(defun phd-ark-modeline-mail-icon ()
  "Iconify mail status in phd-ark-modeline."
  (when phd-ark-modeline-mail-mode
    (list
     (propertize (all-the-icons-nerd-oct "mail")
                 'face (if (phd-ark-ml/selected-window-active-p)
                           'phd-ark-modeline-mail-icon-face
                         'phd-ark-modeline-inactive-face)))))

(defun phd-ark-modeline-mode-icon ()
  "Iconify major mode in phd-ark-modeline."
  (list
   (if (phd-ark-ml/selected-window-active-p)
       (all-the-icons-icon-for-mode major-mode)
     (all-the-icons-icon-for-mode major-mode
                                  :face 'phd-ark-modeline-inactive-face))))


;; --- Formats
(defvar phd-ark-modeline-format-original
  '("%e" mode-line-front-space
    mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
    mode-line-frame-identification mode-line-buffer-identification "   "
    mode-line-position
    (vc-mode vc-mode) "  "
    mode-name
    mode-line-misc-info mode-line-end-spaces)
  "Original format of the mode-line (except only the major mode is shown).")

(defvar phd-ark-modeline-format-default
  (list
   '(:eval (phd-ark-modeline-padding))
   '(:eval (phd-ark-modeline-marker))
   '(:eval (phd-ark-modeline-whitespace))
   '(:eval (phd-ark-modeline-buffer-name))
   '(:eval (phd-ark-modeline-whitespace))
   '(:eval (phd-ark-modeline-buffer-position))
   '(:eval (phd-ark-modeline-whitespace 4))
   '(:eval (phd-ark-modeline-vc-icon 0 0 1))
   '(:eval (phd-ark-modeline-vc-status))
   '(:eval (phd-ark-modeline-space-between 2))
   '(:eval (phd-ark-modeline-major-mode))
   '(:eval (phd-ark-modeline-whitespace))
   '(:eval (phd-ark-modeline-padding))
   )
  "Default format of the phd-ark-modeline.")

(defcustom phd-ark-modeline-format nil
  "The customizable phd-ark-modeline format."
  :type '(list))


;; --- Customization
;; My personal configuration:
;; (setq phd-ark-modeline-column-mode t
;;       phd-ark-modeline-mail-mode t)
;; (setq phd-ark-modeline-format
;;       (list
;;        '(:eval (phd-ark-modeline-padding))
;;        '(:eval (phd-ark-modeline-marker ""))
;;        '(:eval (phd-ark-modeline-whitespace))
;;        '(:eval (phd-ark-modeline-buffer-lock-icon))
;;        '(:eval (phd-ark-modeline-buffer-name))
;;        '(:eval (phd-ark-modeline-buffer-modified-icon))
;;        '(:eval (phd-ark-modeline-whitespace))
;;        '(:eval (phd-ark-modeline-buffer-position))
;;        '(:eval (phd-ark-modeline-media-info))
;;        '(:eval (phd-ark-modeline-whitespace))
;;        '(:eval (phd-ark-modeline-flycheck-status))
;;        '(:eval (phd-ark-modeline-whitespace 4))
;;        '(:eval (phd-ark-modeline-vc-icon 1 1 1))
;;        '(:eval (phd-ark-modeline-vc-status))
;;        '(:eval (phd-ark-modeline-mail-icon))
;;        '(:eval (phd-ark-modeline-mail-status))
;;        '(:eval (phd-ark-modeline-whitespace))
;;        '(:eval (phd-ark-modeline-space-between 4))
;;        '(:eval (phd-ark-modeline-mode-icon))
;;        '(:eval (phd-ark-modeline-whitespace))
;;        '(:eval (phd-ark-modeline-major-mode))
;;        '(:eval (phd-ark-modeline-whitespace))
;;        '(:eval (phd-ark-modeline-padding))
;;        ))

;;;###autoload
(define-minor-mode phd-ark-modeline-mode
  "Toggle phd-ark-modeline on or off."
  :init-value nil
  :group 'phd-ark-modeline
  :global t
  :keymap phd-ark-modeline-mode-map
  (if phd-ark-modeline-mode
      (if phd-ark-modeline-format
          (progn
            (setq-default mode-line-format phd-ark-modeline-format)
            (message "phd-ark-modeline-mode activated!"))
        (progn
          (setq-default mode-line-format phd-ark-modeline-format-default)
          (message "phd-ark-modeline-mode activated!")))
    (progn
      (setq-default mode-line-format phd-ark-modeline-format-original)
      (message "phd-ark-modeline deactivated!")))
  (force-mode-line-update))


(provide 'phd-ark-modeline)
;;(provide 'phd-ark-modeline-mode)
;;; phd-ark-modeline.el ends here
