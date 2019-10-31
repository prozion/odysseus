#lang racket

(require racket/cmdline)
(require compatibility/defmacro)

(require "../lib/_all.rkt")
(require "../scrap/vk.rkt")
(require "../graphics/console.rkt")
(require "../report/html.rkt")

; (define ns (module->namespace (string->path "c:/denis/denis_core/odysseus/scrap/vk.rkt")))
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define intersects "")
(define group-minus "")
(define group-difference "")
(define expression "")
(define output-file "")

; intersects "Пересечение по участникам" vk/intersect-groups " ∩ "
(define-macro (process-option var title op symbol)
  `(let ((res-ids (apply ,op ,var)))
    (if (notnil? output-file)
      (write-html-file
        output-file
        ,title
        (map vk/id->href res-ids)
        #:lead
        (format
          "~a<br>~a<br>"
          (implode
            (map
              (λ (x) (html-a
                        (str "https://vk.com/" (remove-vk-url-prefix x))
                        (get-group-name x)
                        #:color "#009000"))
              ,var)
            (html-color ,symbol "red"))
          (format "Всего: <b>~a</b> человек~n" (length res-ids))))
      (display (format "~n~a: ~a человек~n" ,title (length res-ids))))))

(define (process-expression expression)
  (define (id->href-iter res-ids (res empty))
    (cond
      ((empty? res-ids) res)
      (else (sleep 0.1)
            (id->href-iter (cdr res-ids) (pushr res (vk/id->href (car res-ids)))))))
  (let ((title "Результат выражения")
        (res-ids (eval (read (open-input-string expression)) ns)))
    (if (notnil? output-file)
      (write-html-file
        output-file
        title
        (id->href-iter res-ids)
        #:lead
        (format
          "~a<br>~a<br>"
          (html-color expression "#009000")
          (format "Всего: <b>~a</b> человек~n" (length res-ids))))
      (display (format "~n~a: ~a человек~n" title (length res-ids))))))

(command-line
  #:program "vk-groups, utility to work with groups in vk.com"
  #:multi
    [("-i" "--intersect") i
                    "groups intersection, write groups ids through a space"
                    (set! intersects (split i " "))]
    [("-m" "--minus") gr-m
                    "groups intersection, write groups ids through a space"
                    (set! group-minus (split gr-m " "))]
    [("-d" "--difference") gr-d
                    "groups intersection, write groups ids through a space"
                    (set! group-difference (split gr-d " "))]
    [("-e" "--eval") e
                    "evaluate custom expression (through combination of g^, g- and g--)"
                    (set! expression e)]
    [("-o" "--output") o
                    "redirect output to file in the current directory"
                    (set! output-file o)]
    [("-s" "--status")
                    "display the process progress"
                    (status-output #t)]
  #:args
    ()
    (cond
      ((notnil? intersects) (process-option intersects "Пересечение по участникам" vk/intersect-groups " ∩ "))
      ((notnil? group-minus) (process-option group-minus "Разница по участникам" vk/minus-groups " − "))
      ((notnil? group-difference) (process-option group-difference "Симметрическая разница по участникам" vk/difference-groups " ⊖ "))
      ((notnil? expression) (process-expression expression))
      (else
        (newline)
        (display "какой запрос вы хотите выполнить?")
        (void)))
)
