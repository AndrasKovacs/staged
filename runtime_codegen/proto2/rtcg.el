
;;; rtcg.el --- Mode for the cctt programming language -*- lexical-binding: t -*-
;; URL: https://github.com/AndrasKovacs/staged
;; Package-version: 1.0
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5"))
;; Keywords: languages

;;; Commentary:
;; This package provides a major mode for editing proofs or programs
;; in rtcg, an implementation of a minimal dependently typed language
;; with runtime code generation.

(require 'comint)
(require 'cl-lib)

;;;; Customization options

(defgroup rtcg nil "Options for rtcg-mode"
  :group 'languages
  :prefix 'rtcg-
  :tag "rtcg")

;;;; Syntax

(defvar rtcg-keywords
  '("let" "do")
  "Keywords.")

(defvar rtcg-operations
  '("U" "Top" "Code" "Ref" "Eff" "tt" "read" "write" "new" "return")
  "Operations.")

(defvar rtcg-special
  '("undefined")
  "Special operators.")

(defvar rtcg-keywords-regexp
  (regexp-opt rtcg-keywords 'words)
  "Regexp that recognizes keywords.")

(defvar rtcg-operations-regexp
  (regexp-opt rtcg-operations 'words)
  "Regexp that recognizes operations.")

(defvar rtcg-operators-regexp
  (regexp-opt '(":" "->" "<-" "→" "←" "=" ":=" "=" "\\" "λ" "_" "." "<" ">" "~" "⊤" "□" ";") t)
  "Regexp that recognizes operators.")

(defvar rtcg-special-regexp
  (regexp-opt rtcg-special 'words)
  "Regexp that recognizes special operators.")

(defvar rtcg-def-regexp "^[[:word:]'-]+"
  "Regexp that recognizes the beginning of a definition.")

(defvar rtcg-font-lock-keywords
  `((,rtcg-keywords-regexp . font-lock-keyword-face)
    (,rtcg-operations-regexp . font-lock-builtin-face)
    (,rtcg-operators-regexp . font-lock-variable-name-face)
    (,rtcg-special-regexp . font-lock-warning-face)
    (,rtcg-def-regexp . font-lock-function-name-face))
  "Font-lock information, assigning each class of keyword a face.")

(defvar rtcg-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\{  "(}1nb" st)
    (modify-syntax-entry ?\}  "){4nb" st)
    (modify-syntax-entry ?-  "_ 123" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\\ "." st)
    st)
  "Syntax table with Haskell-style comments.")


;;;###autoload
(define-derived-mode rtcg-mode prog-mode
  "rtcg"
  "Major mode for editing rtcg files."

  :syntax-table rtcg-syntax-table

  ;; Make comment-dwim do the right thing
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")

  ;; Code for syntax highlighting
  (setq font-lock-defaults '(rtcg-font-lock-keywords))

  ;; Clear memory
  (setq rtcg-keywords-regexp nil)
  (setq rtcg-operators-regexp nil)
  (setq rtcg-special-regexp nil))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rtcg\\'" . rtcg-mode))

(provide 'rtcg)
