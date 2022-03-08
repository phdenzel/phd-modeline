;;; phd-modeline.el --- My modeline customization
;;
;;; Author: phdenzel
;;
;;; Commentary:
;; This is my module for customising the Emacs mode-line.
;;
;;; Installation (for example):
;;
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
;;   :after (all-the-icons)
;;   :hook (after-init . phd-modeline-mode)
;;   :bind (("C-x |" . phd-modeline-mode))
;;   :config
;;   ...
;;   )


;;; Code:
(require 'all-the-icons)

(defgroup phd-modeline nil
  "A minimal mode-line."
  :group 'mode-line
  :link '(url-link :tag "Homepage" "https://github.com/phdenzel/phd-modeline"))


;; ------ Style
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

(defface phd-modeline-icon-face
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
  :set (lambda (sym val)
         (set sym (if (> val 0) val 1)))
  :group 'phd-modeline)

(defcustom phd-modeline-bar-data-func 'phd-ml/bar-tri-data
  "The data function for the bar image.
Choose between
- `phd-ml/bar-rec-data' - rectangular bar image
- `phd-ml/bar-tri-data' - triangular  bar image"
  :type 'function
  :set (lambda (sym val) (fset sym val))
  :group 'phd-modeline)

(defcustom phd-modeline-use-icons t
  "Disable all icon drawing fuctionality.
TODO: Not yet implemented."
  :type 'boolean
  ;;:set (lambda (sym val))
  :group 'phd-modeline)

;; for terminal mode
;; (unless window-system
;;   (defun all-the-icons-octicon (&rest _) "" "")
;;   (defun all-the-icons-faicon (&rest _) "" "")
;;   (defun all-the-icons-fileicon (&rest _) "" "")
;;   (defun all-the-icons-wicon (&rest _) "" "")
;;   (defun all-the-icons-alltheicon (&rest _) "" ""))


;; ------ Utilities
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


;; ------ Components
;; Space fillers
(defun phd-modeline-whitespace ()
  "Format buffer name in phd-modeline depending on its state."
  (list (propertize " "
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
                                           mode-name))
                                      ))))))


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
   (phd-modeline-whitespace)
   (propertize "C:[%3c]"
               'face (if (phd-ml/selected-window-active-p)
                         'phd-modeline-buffer-column-face
                       'phd-modeline-inactive-face))
   (phd-modeline-whitespace)
   (propertize "@"
               'face (if (phd-ml/selected-window-active-p)
                         'mode-line
                       'phd-modeline-inactive-face))
   (phd-modeline-whitespace)
   (propertize "%p"
               'face (if (phd-ml/selected-window-active-p)
                         'phd-modeline-buffer-percentage-face
                       'phd-modeline-inactive-face))
   (phd-modeline-whitespace)))

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

(defun phd-modeline-git-vc ()
  "Git repository status info for phd-modeline."
  nil
  )


;; Icons
(defun phd-modeline-buffer-lock-icon ()
  "Iconify read-only buffer in phd-modeline."
  (when buffer-read-only
    (list
     (all-the-icons-octicon "lock"
                            :face (if (phd-ml/selected-window-active-p)
                                      'phd-modeline-buffer-read-only-face
                                    'phd-modeline-inactive-face)
                            :v-adjust -0.05)
     (phd-modeline-whitespace))))

(defun phd-modeline-buffer-modified-icon ()
  "Iconify modified buffer in phd-modeline."
  (when (and (buffer-modified-p) (not buffer-read-only))
    (list
     (phd-modeline-whitespace)
     (all-the-icons-faicon "floppy-o"
                            :face (if (phd-ml/selected-window-active-p)
                                      'phd-modeline-buffer-modified-face
                                    'phd-modeline-inactive-face)
                            :v-adjust -0.05))))

(defun phd-modeline-mode-icon ()
  "Iconify major mode in phd-modeline."
  (list
   (all-the-icons-icon-for-mode
    major-mode
    ;; :face (if (phd-ml/selected-window-active-p)
    ;;           'phd-modeline-icon-face
    ;;         'phd-modeline-inactive-face)
   )))


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
   '(:eval (phd-modeline-whitespace))
   '(:eval (phd-modeline-buffer-name))
   '(:eval (phd-modeline-whitespace))
   '(:eval (phd-modeline-buffer-position))
   '(:eval (phd-modeline-media-info))
   '(:eval (phd-modeline-space-between 1))
   '(:eval (phd-modeline-major-mode))
   '(:eval (phd-modeline-whitespace)))
  "Default format of the phd-modeline.")

(defcustom phd-modeline-format nil
  "The customizable phd-modeline format."
  :type (list))

(setq phd-modeline-format
      (list
       '(:eval (phd-modeline-bar))
       '(:eval (phd-modeline-whitespace))
       '(:eval (phd-modeline-buffer-lock-icon))
       '(:eval (phd-modeline-buffer-name))
       '(:eval (phd-modeline-buffer-modified-icon))
       '(:eval (phd-modeline-whitespace))
       '(:eval (phd-modeline-buffer-position))
       '(:eval (phd-modeline-media-info))
       '(:eval (phd-modeline-flycheck-status))
       '(:eval (phd-modeline-space-between 4))
       '(:eval (phd-modeline-mode-icon))
       '(:eval (phd-modeline-whitespace))
       '(:eval (phd-modeline-major-mode))
       '(:eval (phd-modeline-whitespace))
       ))

;; Left-aligned:
;; <Flyspell_asIcon>
;; Right-aligned:
;; <major-mode>

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
    (setq-default mode-line-format phd-modeline-format-original)
    ))

(provide 'phd-modeline)
;;; phd-modeline.el ends here