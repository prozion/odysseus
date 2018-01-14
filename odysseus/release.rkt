#lang racket

(require racket/file)
(require "../lib/all.rkt")

(provide (all-defined-out))

(define ODYSSEUS "c:/denis/denis_core/odysseus/")
(define ODYSSEUS-RELEASES "c:/denis/denis_core/odysseus-releases/")

(define (extract-files files-set new-directory #:exception-set (exception-set #f) #:extract-to (extract-to ODYSSEUS-RELEASES))
  (let ((new-root (string->path (str ODYSSEUS-RELEASES new-directory))))
    ;(make-directory* new-root)
    (for ((file files-set))
      (let ((old-path (string->path (str ODYSSEUS file)))
            (new-path (string->path (str extract-to new-directory "/" file))))
        ;(when (directory-exists? old-path) (make-directory* new-path))
        (delete-directory/files
            new-path
            #:must-exist? #f)
        (copy-directory/files
          old-path
          new-path)))
    (when exception-set
      (for ((file exception-set))
        (delete-directory/files (string->path (str extract-to new-directory "/" file)))))))
