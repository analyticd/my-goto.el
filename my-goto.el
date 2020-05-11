;;; my-goto.el --- go to things quickly -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Bas Alberts <bas@anti.computer>
;; URL: https://github.com/anticomputer/my-goto.el

;; Version: 0.1
;; Package-Requires: ((emacs "25") (cl-lib "0.5"))

;; Keywords: bookmark

;;; Commentary:

;;; This lets you define custom dispatch bookmarks
;;; You can think of it as a lightweight `bookmark+'

;;; Code:
(require 'bookmark)
(require 'cl-lib)
(require 'url-util)

;; add any custom classes to this alist as (class . read-prompt)
;; read-prompt should take a single PROMPT argument defaults to read-string
(defvar my/goto-classes
  '((:uri)
    (:eshell . read-directory-name)
    (:term . read-directory-name)
    (:file . read-file-name)
    (:directory  . read-directory-name)
    (:pdf . read-file-name)))

;; define a generic (xristos-fu)
(cl-defgeneric my/goto-dispatch (class goto)
  "Visit GOTO based on CLASS.")

;; specialize the generic for the cases we want to handle
(cl-defmethod my/goto-dispatch ((class (eql :uri)) goto)
  "Visit GOTO based on CLASS."
  (browse-url goto))

(cl-defmethod my/goto-dispatch ((class (eql :eshell)) goto)
  "Visit GOTO based on CLASS."
  (let ((default-directory goto))
    (eshell t)))

(cl-defmethod my/goto-dispatch ((class (eql :term)) goto)
  "Visit GOTO based on CLASS."
  (let ((default-directory goto))
    (term "fish")))

(cl-defmethod my/goto-dispatch ((class (eql :file)) goto)
  "Visit GOTO based on CLASS."
  (find-file goto))

(cl-defmethod my/goto-dispatch ((class (eql :directory)) goto)
  "Visit GOTO based on CLASS."
  (find-file goto))

(cl-defmethod my/goto-dispatch ((class (eql :pdf)) goto)
  "Visit GOTO based on CLASS."
  (find-file goto))

;; fall-through method
(cl-defmethod my/goto-dispatch (class goto)
  "Visit GOTO based on CLASS."
  (message "my/goto: no handler for %s!" class))

;;;###autoload
(defun my/goto-bookmark-url-at-point ()
  "Create a goto bookmark for the url at point."
  (interactive)
  (let ((url (url-get-url-at-point)))
    (if url
        (let ((label (read-string "label: " nil nil url)))
          (my/goto-bookmark-location :uri url label))
      (message "my/goto: no url at point!"))))

;;;###autoload
(defun my/goto-bookmark-handler (bookmark)
  "Handle goto BOOKMARK through goto dispatchers."
  (let* ((v (cdr (assq 'goto bookmark)))
         (class (car v))
         (goto (cadr v)))
    (my/goto-dispatch class goto)))

;;;###autoload
(defun my/goto-bookmark-location (class location &optional label tags-list)
  "Bookmark LOCATION of CLASS under optional LABEL."
  (interactive
   (let* ((class (read (completing-read "class: " my/goto-classes)))
          (prompt "location: ")
          (get-location (cdr (assq class my/goto-classes)))
          (location
           (if (functionp get-location)
               (funcall get-location prompt)
             ;; default to read-string
             (read-string prompt (when (derived-mode-p 'w3m-mode)
                                   w3m-current-url))))
          (label (read-string "label: " location nil location))
          (tags-list (read (read-string "tags (e.g., '(\"haskell\" \"monads\")): " "'(nil)"))))
     (list class location label tags-list)))
  (let ((label (or label location)))
    (bookmark-store
     ;; prepend any goto labels with their class
     (format "%s %s" class label)
     (if (not (eq tags-list '(nil)))
         `((filename . ,location)
           (tags     . ,tags-list)
           (handler  . my/goto-bookmark-handler)
           (goto     . (,class ,location)))
       `((filename . ,location)
         (handler  . my/goto-bookmark-handler)
         (goto     . (,class ,location))))
     nil)))

(provide 'my-goto)
;;; my-goto.el ends here
