;;; eyeliner.el --- spaceline helper library
;; Copyright (C) 2018 Dustin Lacewell

;; Author: Dustin Lacewell <dlacewell@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24") (dash "0") (magit "0") (spaceline "0") (all-the-icons "0"))
;; Keywords: eyeliner spaceline modeline
;; URL: http://github.com/dustinlacewell/eyeliner

;;; Commentary:

;; Good looking mode-lines for everyone!

;;; Code:
(require 'dash)
(require 'magit)
(require 'spaceline)
(require 'all-the-icons)

(defvar eyeliner/warm-color "indian red"
  "Color for when something needs attention")
(defvar eyeliner/cool-color "deep sky blue"
  "Color for when something is fine")
(defvar eyeliner/plain-color (face-foreground 'default))
  "Color for when no attention needs to be drawn"
(defvar eyeliner/default-darkness 35
  "Amount to darken dimmed styles")
(defvar eyeliner/default-desaturation 65
  "Amount to desaturate dimmed styles")

(defun eyeliner/transform (form)
  "Transform FORM into a form suitable for a let binding"
  (if (ignore-errors (fboundp form)) `(it (apply (quote ,form) (list it)))
    `(it ,form)))

(defmacro eyeliner/pipeline (value &rest exprs)
  "Bind VALUE to 'it and evaluate each of EXPRS. The result of
     each evaluation is bound to 'it before the next."
  (declare (indent defun))
  (let* ((transforms (mapcar 'eyeliner/transform exprs))
         (assignments (append `((it ,value)) transforms)))
    `(let* ,assignments it)))

(defun eyeliner/symbol (name suffix)
  "Return a symbol with the given name and suffix"
  (intern (format "eyeliner/%s-%s" name suffix)))

(defun eyeliner/adjust-color (color &optional darkness desaturation)
  "Return COLOR modified by DARKNESS and DESATURATION"
  (eyeliner/pipeline color
    (color-darken-name it (or darkness eyeliner/default-darkness))
    (color-desaturate-name it (or desaturation eyeliner/default-desaturation))))

(defun eyeliner/make-face (symbol props)
  "Create a face with the given SYMBOL and PROPS"
  (face-spec-set symbol (list (cons t props))) symbol)

(defun eyeliner/make-active-face (name color &optional props)
  "Create a face with the given COLOR and PROPS and return the
   symbol based on NAME"
  (let ((symbol (eyeliner/symbol name "active"))
        (props (append props `(:foreground ,color))))
    (eyeliner/make-face symbol props)))

(defun eyeliner/make-inactive-face (name color &optional darkness desaturation props)
  "Create a face with the given COLOR modified by DARKNESS and
   DESATURATION and any other PROPS. Returns the symbol based on
   NAME."
  (let* ((color (eyeliner/adjust-color color darkness desaturation))
         (symbol (eyeliner/symbol name "inactive"))
         (props `(:foreground ,color)))
    (eyeliner/make-face symbol props)))

(defun eyeliner/make-face-pair (name color &optional darkness desaturation props)
  "Return the a cons containing symbols, based on NAME, for newly
   created faces. One face's COLOR will be modified by DARKNESS
   and DESATURATION."
  (let ((active-face (eyeliner/make-active-face name color props))
        (inactive-face (eyeliner/make-inactive-face name color darkness desaturation props)))
    (cons active-face inactive-face)))

(defun eyeliner/get-icon-factory (set-name)
  "Return an icon factory for the given iconset"
  (--when-let (all-the-icons--function-name set-name)
    (when (fboundp it) it)))

(defun eyeliner/get-icon-family (set-name)
  "Return the family-name for a given iconset"
  (--when-let (all-the-icons--family-name set-name)
    (apply it '())))

(defun eyeliner/find-icon (icon-name)
  "Return a cons containing an icon and its family-name"
  (cl-loop for set-name in '(octicon faicon wicon fileicon material alltheicon)
           for factory = (eyeliner/get-icon-factory set-name)
           for icon = (ignore-errors (apply factory `(,icon-name)))
           for family = (eyeliner/get-icon-family set-name)
           if icon
             return (cons icon family)))

(defmacro eyeliner/with-icon (icon-name &rest body)
  "Execute body while binding icon and family"
  (declare (indent defun))
  `(--when-let (eyeliner/find-icon ,icon-name)
     (cl-destructuring-bind (icon . family) it ,@body)))

(defvar eyeliner/segment-definitions '())

(defvar eyeliner/left-hand-segments
  '((eyeliner/buffer-modified)
    (eyeliner/branch-icon :skip-alternate t :tight-right t)
    (eyeliner/branch-name)
    (eyeliner/project-name :skip-alternate t)
    (eyeliner/mode-icon :skip-alternate t :tight-left t :tight-right t)
    (eyeliner/buffer-name)))

(defvar eyeliner/right-hand-segments
  '(("%l:%c")))

(defmacro eyeliner/segment (name body &rest props)
  "Defer the evaluation of BODY until 'eyeliner/segments
   is called."
  (declare (indent defun))
  `(setq eyeliner/segment-definitions
         (append eyeliner/segment-definitions
                 '((spaceline-define-segment ,name ,body ,@props)))))

(defun eyeliner/style (name color &optional darkness desaturation props)
  "Define a function which will return propertized text with the
   proper color based on the value of ACTIVE which is bound by
   spaceline."
  (declare (indent defun))
  (let ((symbol (eyeliner/symbol name "style")))
    (cl-destructuring-bind (active-face . inactive-face)
        (eyeliner/make-face-pair name
                                 color
                                 darkness
                                 desaturation
                                 props)
      (fset symbol (eval `(lambda (text)
                            (propertize text
                              'font-lock-face (if active (quote ,active-face)
                                                (quote ,inactive-face))
                              'face (if active (quote ,active-face)
                                      (quote ,inactive-face)))))))))

(defmacro eyeliner/icon (name icon-name color &optional darkness desaturation props display)
  "Define a function which will return propertized text with the
   proper color based on the value of ACTIVE which is bound by
   spaceline."
  (declare (indent defun))
  (eyeliner/with-icon icon-name
    (cl-destructuring-bind (active-face . inactive-face)
        (let ((color (eval color))
              (face-name (format "%s-icon" name)))
          (eyeliner/make-face-pair face-name color darkness desaturation props))
      (let ((symbol (eyeliner/symbol name "icon")))
        `(defun ,symbol ()
           (propertize ,icon
             'display (or ,display '((height 1.0) (raise 0.0)))
             'font-lock-face (if active (quote ,active-face)
                               (quote ,inactive-face))
             'face (if active (quote ,active-face)
                     (quote ,inactive-face))))))))

;; (eyeliner/icon branch-new "git-branch" (badger-color "yellow"))

(defun eyeliner/install (&optional left right)
  "Evaluate each form in 'eyeliner/segment-definitions then update 'mode-line-format"
  (setq eyeliner/left-hand-segments (or left eyeliner/left-hand-segments))
  (setq eyeliner/right-hand-segments (or right eyeliner/right-hand-segments))
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
  (mapc 'eval eyeliner/segment-definitions)
  (spaceline-install 'main
    eyeliner/left-hand-segments
    eyeliner/right-hand-segments))

;; buffer modification icon
(eyeliner/icon unmodified "circle-o" eyeliner/cool-color)
(eyeliner/icon modified "dot-circle-o" eyeliner/warm-color)
(eyeliner/icon locked "diff-added" eyeliner/warm-color)

(eyeliner/segment eyeliner/buffer-modified
  (let ((buffer-state (format-mode-line "%*")))
    (cond
     ((string= buffer-state "-") (eyeliner/unmodified-icon))
     ((string= buffer-state "*") (eyeliner/modified-icon))
     ((string= buffer-state "%") (eyeliner/locked-icon)))))

;; buffer name
(eyeliner/style 'buffer-name eyeliner/plain-color)
(eyeliner/style 'buffer-name-modified eyeliner/warm-color)

(eyeliner/segment eyeliner/buffer-name
  (let* ((buffer-state (format-mode-line "%*"))
         (style (cond
                 ((string= buffer-state "-") 'eyeliner/buffer-name-style)
                 ((string= buffer-state "*") 'eyeliner/buffer-name-modified-style)
                 ((string= buffer-state "%") 'eyeliner/buffer-name-modified-style))))
    (apply style `(,(buffer-name)))))

(eyeliner/icon branch "git-branch" eyeliner/cool-color)
(eyeliner/icon branch-diff "note_add" eyeliner/warm-color nil nil nil
  '((raise -0.2)))

(eyeliner/segment eyeliner/branch-icon
  (when vc-mode
    (if (magit-anything-modified-p) (eyeliner/branch-diff-icon)
      (eyeliner/branch-icon))))

(eyeliner/style 'branch-clean eyeliner/cool-color)
(eyeliner/style 'branch-dirty eyeliner/warm-color)
(eyeliner/segment eyeliner/branch-name
  (when vc-mode
    (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
      (if (magit-anything-modified-p) (eyeliner/branch-dirty-style branch)
        (eyeliner/branch-clean-style branch)))))

(eyeliner/style 'project-name eyeliner/plain-color)

(eyeliner/segment eyeliner/project-name
  (when (projectile-project-p)
      (eyeliner/project-name-style (projectile-project-name))))

(eyeliner/segment eyeliner/mode-icon
  (let* ((icon) (all-the-icons-icon-for-mode major-mode))
    (if icon
        (propertize icon
          'help-echo (format "Major-mode!: `%s'" major-mode)
          'display '(raise 0)
          'face `(
                  :foreground ,eyeliner/plain-color
                  :height 0.9
                  :family ,(all-the-icons-icon-family-for-mode major-mode)
                  :inherit))
      (eyeliner/with-icon "emacs"
        (propertize icon
          'help-echo (format "Major-mode: `%s'" major-mode)
          'display '(raise 0)
          'face `(
                  :foreground ,eyeliner/plain-color
                  :height 0.9
                  :family ,family
                  :inherit))))))

(provide 'eyeliner)
;;; eyeliner.el ends here
