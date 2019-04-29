#lang racket

(require racket/file)
(require "base.rkt")
(require "type.rkt")

(provide (all-defined-out))

(define-catch (copy-file src dest)
  (when (and (file-exists? src) (file-exists? dest)) (delete-directory/files dest))
  (copy-directory/files src dest))

(define-catch (move-file src dest)
  (if (and (file-exists? src) (file-exists? dest))
    (begin
      (delete-directory/files dest)
      (copy-directory/files src dest)
      (delete-directory/files src))
    (error
      (cond
        ((not (file-exists? src)) (format "file ~a doesn't exists" src))
        ((not (file-exists? dest)) (format "file ~a doesn't exists" dest))
        (else "unknown error")))))

(define-catch (get-file-extension path)
  (let* ((path-parts (string-split (->string path) "."))
        (extension (if (> (length path) 1) (last path-parts) "")))
    extension))
