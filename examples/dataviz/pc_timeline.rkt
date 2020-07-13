#lang racket

(require "../../lib/load/all.rkt")
(require "../../ontology/tab-tree-hash.rkt")
(require "../../graphics/svg-light.rkt")
(require "../../graphics/fonts.rkt")

(define _ (parse-tab-tree-hash "c:/denis/denis_core/denis_personal/my_memories/hard_soft.tree"))
; (define myprojects (hash-tree-flatten-with-paths _))
(define myprojects
  (append
    (map (λ (x) (hash-union x (hash 'category "hard"))) ($ hard _))
    (map (λ (x) (hash-union x (hash 'category "soft"))) ($ soft _))
))

(define start "09.1994")
(define end "02.2018")
(define timeline (hash 'x 50 'y 50 'w 1720 'h 800 'background (hash 'fill "#f0f0f0")))
(define title (hash 'y 30 'style (hash 'fill "#606060" 'font-size 20 'font-weight "bold" 'font-family "Tahoma") 'text "МОИ КОМПЫ И ЯЗЫКИ ПРОГРАММИРОВАНИЯ"))
(define label-text (hash 'vmargin 2 'style (hash 'font-size 10 'font-weight "normal" 'font-family "Tahoma")))
(define month-tick (hash 'style (hash 'stroke-width 0.3 'stroke "black")))
(define year-tick (hash 'style (hash 'stroke-width 0.8 'stroke "black")))
(define annotation-text (hash 'vmargin 10 'hmargin 3 'style (hash 'font-size 12 'font-weight "normal" 'font-family "Tahoma" 'fill "black") 'plate-style (hash 'fill "white" 'opacity 0.8)))
(define myproject (hash 'style (hash 'stroke-width 8 )))
(define gap-between-projects (/ (- ($ h timeline) 10.0) (length myprojects)))
(define month-width (/ ($ w timeline) (month-diff start end) 1.0))

(define colors (hash "hard" "#B35959" "soft" "#59B3B3"))

(define (get-project-color n)
  (hash-ref colors nth (hash-keys colors) n))

(define (get-year months-passed-from-the-start)
  (+ (->number (year start)) (quotient (+ months-passed-from-the-start (->number (month start))) 12)))

(define (month->x month-year)
  (let ((month-year (if (equal? month-year "current")
                          (m.y (current-date))
                          month-year)))
    (+ ($ x timeline) (* month-width (month-diff start month-year)))))

(define (get-time-intervals project)
  (let* ((interval ($ interval project))
        (interval (string-split interval ", "))
        (interval (map (λ (x) (split x "-")) interval)))
    interval))

(define (draw-title title)
  (let ((xc (h-centrify ($ w timeline) ($ text title) #:font-size ($ style.font-size title))))
    `(text
      (@
        (x ,xc)
        (y ,($ y title))
        (style ,(style->string ($ style title))))
      ,($ text title))))

(define (draw-timeline-background timeline)
  `(rect (@
          (x ,($ x timeline)) (y ,($ y timeline)) (width ,($ w timeline)) (height ,($ h timeline))
          (style ,(style->string ($ background timeline))))))

(define (draw-tick x (is-first-month #f))
  `(line (@
      (x1 ,x)
      (y1 ,($ y timeline))
      (x2 ,x)
      (y2 ,(+ ($ y timeline) ($ h timeline)))
      (style ,(if is-first-month
                (style->string ($ style year-tick))
                (style->string ($ style month-tick)))))))

(define (draw-label yl)
  `(g
     (text
        (@ (x ,($ x yl)) (y ,(- ($ y timeline) ($ vmargin label-text))) (style ,(style->string ($ style label-text))))
        ,($ text yl))
     (text
        (@ (x ,($ x yl)) (y ,(+ ($ y timeline) ($ h timeline) ($ vmargin label-text) ($ style.font-size label-text))) (style ,(style->string ($ style label-text))))
        ,($ text yl))))

(write-svg
  "pc-timeline.svg"
  `(g
      ,(draw-title title)
      (g
        ,(draw-timeline-background timeline))
      (g
        ,@(for/fold
          ((res (list)))
          ((i (in-range 1 (+ 1 (month-diff start end)))))
          (let* ((x (+ ($ x timeline) (* i month-width)))
                (is-first-month (first-month? i (->number (month start))))
                (yl (if is-first-month
                                (hash 'x (+ x (h-centrify (* 12.0 month-width) "0000")) 'text (get-year i))
                                #f))
                (res (pushr res (draw-tick x is-first-month)))
                (res (if yl (pushr res (draw-label yl)) res)))
            res)))
      (g
        ,@(for/fold
          ((res (list)))
          ((project myprojects))
          (let* ((idx (indexof myprojects project))
                (project (hash-union project (hash 'style (hash 'stroke (hash-ref colors ($ category project))))))
                (project (hash-fuse project myproject))
                (y (+ ($ y timeline) (* idx gap-between-projects)))
                (intervals (get-time-intervals project))
                (xs (map
                      (λ (part) (list (month->x (first part)) (month->x (second part))))
                      intervals))
                (x0 (caar xs)))
            (pushr
              res
              `(g
                  (rect (@
                          (x ,x0) (y ,(- y ($ vmargin annotation-text) ($ style.font-size annotation-text)))
                          (width ,(+ ($ hmargin annotation-text)
                                    5
                                    (text-length ($ name project)
                                      #:font-family ($ style.font-family annotation-text)
                                      #:font-size ($ style.font-size annotation-text)
                                      #:font-style ($ style.font-weight annotation-text))))
                          (height ,(* 1.3 ($ style.font-size annotation-text)))
                          (style ,(style->string ($ plate-style annotation-text)))))
                  (a (@ (href ,(or ($ url project) ($ url-results project) "#")) (target "_blank") (class "hover_group"))
                    (text (@
                            (x ,(+ ($ hmargin annotation-text) x0)) (y ,(- y ($ vmargin annotation-text)))
                            (style ,(style->string ($ style annotation-text))))
                      ,($ name project))
                  ,@(for/list
                    ((xx xs))
                    (let ((x1 (first xx))
                          (x2 (second xx)))
                      `(line (@
                              (x1 ,x1) (y1 ,y) (x2 ,(+ x2 month-width)) (y2 ,y)
                              (style ,(style->string ($ style project)))))))))))))
    )
    #:width 2000
    #:height (+ ($ x timeline) ($ h timeline) 100)
)
