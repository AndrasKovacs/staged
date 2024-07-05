;;; 2ltt.el --- Mode for the cctt programming language -*- lexical-binding: t -*-
;; URL: https://github.com/AndrasKovacs/cctt
;; Package-version: 1.0
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5"))
;; Keywords: languages

;;; Commentary:
;; This package provides a major mode for editing proofs or programs
;; in 2ltt, an implementation of a cartesian cubical type theory.

(require 'comint)
(require 'cl-lib)

;;;; Customization options

(defgroup 2ltt nil "Options for 2ltt-mode"
  :group 'languages
  :prefix '2ltt-
  :tag "2ltt")

;;;; Syntax

(defvar 2ltt-keywords
  '("let")
  "Keywords.")

(defvar 2ltt-operations
  '("Nat0" "Nat1" "NatElim0" "NatElim1" "U0" "U1" "suc0" "suc1" "zero0" "zero1")
  "Operations.")

(defvar 2ltt-special
  '("undefined")
  "Special operators.")

(defvar 2ltt-keywords-regexp
  (regexp-opt 2ltt-keywords 'words)
  "Regexp that recognizes keywords.")

(defvar 2ltt-operations-regexp
  (regexp-opt 2ltt-operations 'words)
  "Regexp that recognizes operations.")

(defvar 2ltt-operators-regexp
  (regexp-opt '(":" "->" "→" "=" ":=" "\\" "λ" "_" ".") t)
  "Regexp that recognizes operators.")

(defvar 2ltt-special-regexp
  (regexp-opt 2ltt-special 'words)
  "Regexp that recognizes special operators.")

(defvar 2ltt-def-regexp "^[[:word:]'-]+"
  "Regexp that recognizes the beginning of a definition.")

(defvar 2ltt-font-lock-keywords
  `((,2ltt-keywords-regexp . font-lock-keyword-face)
    (,2ltt-operations-regexp . font-lock-builtin-face)
    (,2ltt-operators-regexp . font-lock-variable-name-face)
    (,2ltt-special-regexp . font-lock-warning-face)
    (,2ltt-def-regexp . font-lock-function-name-face))
  "Font-lock information, assigning each class of keyword a face.")

(defvar 2ltt-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\{  "(}1nb" st)
    (modify-syntax-entry ?\}  "){4nb" st)
    (modify-syntax-entry ?-  "_ 123" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\\ "." st)
    st)
  "Syntax table with Haskell-style comments.")


;;;###autoload
(define-derived-mode 2ltt-mode prog-mode
  "2ltt"
  "Major mode for editing 2ltt files."

  :syntax-table 2ltt-syntax-table

  ;; Make comment-dwim do the right thing
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")

  ;; Code for syntax highlighting
  (setq font-lock-defaults '(2ltt-font-lock-keywords))

  ;; Clear memory
  (setq 2ltt-keywords-regexp nil)
  (setq 2ltt-operators-regexp nil)
  (setq 2ltt-special-regexp nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.2ltt\\'" . 2ltt-mode))

(provide '2ltt)
