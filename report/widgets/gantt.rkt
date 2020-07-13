#lang racket

(require "../../lib/_all.rkt")
(require "../../graphics/svg.rkt")
(require "../../graphics/color.rkt")
(require "../../tabtree/tab-tree.rkt")
(require "../../tabtree/html.rkt")

(provide draw-gantt draw-longtime-gantt)

; constants
(define y0 50)
(define track-h 27) ; height per one job bar
(define colors (string-split "black #00B6F2 #0096B2 #CC46C7 #89B836 #F29A4C #B22E2A #4C57DB #74D95F" " "))
; (define colors (string-split "black #3EA6B2 #5656C2 #559139 #618AB8 #984EB8 #53AB4D" " "))

(define (get-afters element jobs)
  (if-not ($ after element)
    #f
    (let* ((after ($ after element))
          (after-ids (string-split after ","))
          (afters (for/fold
                    ((res empty))
                    ((a after-ids))
                    (let* ((after-parts (string-split a "+"))
                          (after-id (first after-parts))
                          (increment (if (> (length after-parts) 1) (second after-parts) 0))
                          (after (@id after-id jobs)))
                      (if after
                        (pushr res (list after increment))
                        res)))))
      afters)))

(define-catch (get-start-time element jobs)
  (let* (
        (start ($ start element))
        (afters (get-afters element jobs))
        (with ($ with element))
        (with-parts (and with (string-split with "+")))
        (with-related-element (and with-parts (first with-parts)))
        (with-related-element (@id with-related-element jobs))
        (with-increment (and with-parts (> (length with-parts) 1) (second with-parts)))
        (start
          (cond
            ((and afters (not-empty? afters))
              (d+
                (car
                  (sort
                    (map (λ (x)
                            (d+ (get-end-time (first x) jobs) (days-count (second x))))
                          afters)
                    d>))
                1)) ; as finishing day is inclusive, then increase calculated day by one
            ((and with with-related-element)
              (d+
                (get-start-time with-related-element jobs)
                (if with-increment (days-count with-increment) 0)))
            (start start)
            (else (exit) (error (format "Cannot define start time for ~a" ($ id element)))))))
    start))

(define (get-end-time element jobs)
  (let* (
        (start (get-start-time element jobs))
        (len ($ length element))
        (len (if len (dec (days-count len)) 0)) ; +2d = 2 days, and as the end day is inclusive, decrease resulting day by one
        (end (or ($ end element) (d+ start len))))
    end))

(define-catch (get-start-month element jobs)
  (let* (
        (start ($ start element))
        (afters (get-afters element jobs))
        (with ($ with element))
        (with-parts (and with (string-split with "+")))
        (with-related-element (and with-parts (first with-parts)))
        (with-related-element (@id with-related-element jobs))
        (with-increment (and with-parts (> (length with-parts) 1) (second with-parts)))
        (start
          (cond
            ((and afters (not-empty? afters))
              (inc
                (car
                  (sort
                    (map (λ (x)
                            (+
                              (get-end-month (first x) jobs)
                              (months-count (second x))))
                          afters)
                  d>))))
            ((and with with-related-element)
              (+ (get-start-month with-related-element jobs) (if with-increment (months-count with-increment) 0)))
            ((not start) (error (format "Undefined start date for '~a'" ($ id element))))
            (else
              (date->month start)))))
    start))

(define-catch (get-end-month element jobs)
  (let* (
        (start (get-start-month element jobs))
        (len ($ length element))
        (len (if (false? len)
                  0
                  (months-count len)))
        (end (and ($ end element) (date->month ($ end element))))
        (end (or end (+ start len -1))))
    end))

(define (get-first-after element jobs)
  (let* (
        (afters (get-afters element jobs))
        (result
          (if (and afters (not-empty? (cleanmap afters)))
              (apply min (map (λ (x) ($ _order (first x))) afters))
              #f)))
    result))

;;; Gantt by days
(define-catch (draw-gantt tabtree-file path svg-width svg-height #:tick-dates (tick-dates 'all) #:tracks (tracks-description-path "root.tracks"))
  (let* ((tabtree (parse-tab-tree tabtree-file))
        (path (split path "."))
        (tracks (get-$3 (split tracks-description-path ".") tabtree))
        (jobs (get-$3 path tabtree))
        (start-times (map (λ (x) (get-start-time x jobs)) jobs))
        (end-times (map (λ (x) (get-end-time x jobs)) jobs))
        (min-start-time (apply min (map date->days start-times)))
        (max-end-time (apply max (map date->days end-times)))
        (days-length (+ 3 (- max-end-time min-start-time)))
        (step (/f svg-width days-length))
        (users-list empty)
        )
    (g
      (str
          ; ticks
          (for/fold
            ((res ""))
            ((tick (range 0 (inc days-length))))
            (let* ((aday (+ min-start-time tick))
                  (adate (days->date aday))
                  (full-adate (days->date aday #:year #t))
                  (is-leap-year? (leap-year? (->number (third (split full-adate ".")))))
                  (adate-day (->number (first (split adate "."))))
                  (adate-month (second (split adate ".")))
                  (adate-month-abbr (string-titlecase (month-name adate-month #:lang 'ru)))
                  (first-day? (equal? 1 adate-day))
                  (adate (->string adate-day))
                  (canvas-height (- svg-height 50))
                  (y1 20))
              (str
                res "\n"
                (line 'stroke "grey" 'stroke-width "1" 'opacity 0.5 'x1 (* step tick) 'y1 y1 'x2 (* step tick) 'y2 (+ y1 canvas-height)) "\n"
                (when (holiday? (days->date aday #:year #t))
                  (rect 'x (* step tick) 'y y1 'width step 'height canvas-height 'class "holiday-bg"))
                "\n"
                ; today
                (when (equal? (current-date) full-adate)
                  (rect 'x (* step tick) 'y y1 'width step 'height canvas-height 'class "today-bg"))
                (when first-day?
                  (g
                    (line 'stroke "grey" 'stroke-width "1" 'opacity 0.8 'x1 (* step tick) 'y1 (- y1 25) 'x2 (* step tick) 'y2 (+ y1 canvas-height))
                    (text (@ 'x (+ (* 0.5 step) (* step tick)) 'y 12 'class "month-byday") adate-month-abbr)))
                (when
                  (or (equal? tick-dates 'all)
                      (indexof? (map ->string '(1 3 5 7 10 12 15 17 20 23 25 27 29)) adate))
                  (text
                    (@ 'x (+ (* 0.5 step) (* step tick)) 'y 33 'class "labels")
                    adate))
                "\n"
              )))
          ; bars
          (for/fold
            ((res ""))
            ((job jobs) (i (in-naturals)))
            (let* (
                  (track ($ track job))
                  (track (and track (car (split track ","))))
                  (color (or
                            ($ color (@id track tracks))
                            ($ color (@id "Unknown" tracks))))
                  (_ (--- color))
                  (color (if (equal? ($ status job) "done") "#bbb" color)) ; (color/shadow (color/desaturate color -55) 15)
                  (start-date (get-start-time job jobs))
                  (end-date (get-end-time job jobs))
                  (start (- (date->days start-date) min-start-time))
                  (end (- (date->days end-date) min-start-time))
                  (x1 (* step start))
                  (x2 (* step (inc end))) ; make the end day inclusive
                  (width (- x2 x1))
                  (y (* track-h i))
                  (min_order (apply min (map (λ (x) ($ _order x)) jobs)))
                  (first-after (get-first-after job jobs))
                  (y-first-after (if first-after (* track-h (- first-after min_order)) #f))
                  (h (- track-h 3))
                  )
              (str
                res "\n"
                (g (@ 'transform (format "translate(0,~a)" y0))
                  ; convergence line
                  (when y-first-after
                    (str (line 'x1 x1 'y1 (+ y h) 'x2 x1 'y2 y-first-after 'class "convergence_line") "\n"))
                  ; bar
                  (title (or ($ title job) ($ goal job)))
                  (rect 'x x1 'y y 'width width 'height h 'fill color 'opacity 0.6) "\n"
                  ; (text (@ 'x (+ x 6) 'y (+ y 18) 'class "job_title_shadow") (namefy ($ id job))) "\n"
                  (text (@ 'x (+ x1 5) 'y (+ y (/ track-h 2.0) 2) 'class "job_title") (namefy ($ id job))) "\n"
                )
              )
            ))
          ; legend
          ; (for/fold
          ;   ((res ""))
          ;   ((user users-list) (i (in-naturals)))
          ;   (str
          ;     res "\n"
          ;     (rect 'x (* i 100) 'y (- svg-height 20) 'width 20 'height 10 'fill (list-ref (rest colors) i))
          ;     (text (@ 'x (+ 25 (* i 100)) 'y (- svg-height 10)) user)))
))))


;;; Gantt by months
(define-catch (draw-longtime-gantt tabtree-file path svg-width svg-height #:ticks (ticks 'all) #:legend (legend #f))
  (let* ((tabtree (parse-tab-tree tabtree-file))
        (path (split path "."))
        (jobs (get-$3 path tabtree))
        (start-months (map (λ (x) (get-start-month x jobs)) jobs))
        (end-months (map (λ (x) (get-end-month x jobs)) jobs))
        (initial-month ($ start (get-$2 path tabtree)))
        (initial-month (if (equal? initial-month "<current>") (current-date) initial-month))
        (min-start-month (if initial-month (date->month initial-month) (apply min (map ->number start-months))))
        (max-end-month (apply max (map ->number end-months)))
        (months-length (- max-end-month min-start-month -2))
        (step (/f svg-width months-length))
        (users-list empty)
        ; geometry
        (header-height 50)
        (footer-height 20)
        (y0 header-height) ; upper part height
        (canvas-height (- svg-height header-height footer-height))
        (y1 (- y0 25)) ; where top labels start
        (bottom-y (+ header-height canvas-height footer-height))
        (y2 bottom-y)
        )
    (g
      (str
          ; ticks
          (for/fold
            ((res ""))
            ((tick (range 0 (inc months-length))))
            (let* (
                  (amonth (+ min-start-month tick))
                  (first-month? (= 1 (remainder amonth 12)))
                  (month-abbr (month-name amonth #:lang 'ru))
                  (month-abbr (string-titlecase month-abbr)))
                (str
                  res "\n"
                  (g (@ 'class "ticks")
                  (line 'stroke "grey" 'stroke-width "1" 'opacity 0.3 'x1 (* step tick) 'y1 (+ y1 (* (- y0 y1) 0.5)) 'x2 (* step tick) 'y2 (+ y0 canvas-height)) "\n"
                  (when (odd? amonth)
                    (rect 'x (* step tick) 'y y0 'width step 'height canvas-height 'class "odd-bg"))
                  "\n"
                  ; today
                  (when (equal? (current-month) amonth)
                    (rect 'x (* step tick) 'y y1 'width step 'height canvas-height 'class "even-bg"))
                  (when first-month?
                    (g
                      (text (@ 'x (+ (* step tick) (* step 6)) 'y 20 'class "year") (->string (month->year amonth)))
                      (line 'stroke "grey" 'stroke-width "1" 'opacity 0.8 'x1 (* step tick) 'y1 y1 'x2 (* step tick) 'y2 (+ y0 canvas-height))))
                  (when (equal? ticks 'all)
                    (text
                      (@ 'x (+ (* 0.5 step) (* step tick)) 'y (- y0 7) 'class "month")
                      month-abbr))
                  (when (and (equal? legend 'my-age) (equal? (month-name amonth) "august"))
                    (text
                      (@ 'x (+ (* 0.5 step) (* step tick)) 'y y2 'class "age")
                      (->string (- (month->year amonth) 1979))))
                  "\n"
                ))))
          (line 'stroke "grey" 'stroke-width "1" 'opacity 0.3 'x1 0 'y1 y0 'x2 (* step (inc months-length)) 'y2 y0) "\n"
          ; bars
          (for/fold
            ((res ""))
            ((job jobs) (i (in-naturals)))
            (let* (
                  (user ($ user job))
                  (user (and user (car (split user ","))))
                  (_ (set! users-list (if (indexof? users-list user) users-list (pushr users-list user))))
                  (color (if user (list-ref colors (indexof users-list user)) (list-ref colors 0)))
                  (color (if (empty? (filter true? users-list)) (list-ref colors 1) color))
                  (color (if (equal? ($ status job) "done") "#bbb" color)) ; (color/shadow (color/desaturate color -55) 15)
                  (start-month (get-start-month job jobs))
                  (end-month (get-end-month job jobs))
                  (start (- start-month min-start-month))
                  (end (- end-month min-start-month))
                  (x1 (* step start))
                  (x2 (* step (inc end)))
                  (width (- x2 x1))
                  (y (* track-h i))
                  (min_order (apply min (map (λ (x) ($ _order x)) jobs)))
                  (first-after (get-first-after job jobs))
                  (y-first-after (if first-after (* track-h (- first-after min_order)) #f))
                  (h (- track-h 5))
                  )
              (str
                res "\n"
                (g (@ 'transform (format "translate(0,~a)" y0))
                  ; convergence line
                  (when y-first-after
                    (str (line 'x1 x1 'y1 (+ y h) 'x2 x1 'y2 y-first-after 'class "convergence_line") "\n"))
                  ; bar
                  (title ($ title job))
                  (rect 'x x1 'y y 'width width 'height h 'fill color 'opacity 0.6) "\n"
                  ; (text (@ 'x (+ x 6) 'y (+ y 18) 'class "job_title_shadow") (namefy ($ id job))) "\n"
                  (text (@ 'x (+ x1 5) 'y (+ y (/ track-h 2.0) 2) 'class "job_title") (namefy ($ id job))) "\n"
                )
              )
            ))
          ; legend
          ; (for/fold
          ;   ((res ""))
          ;   ((user users-list) (i (in-naturals)))
          ;   (str
          ;     res "\n"
          ;     (rect 'x (* i 100) 'y (- svg-height 20) 'width 20 'height 10 'fill (list-ref (rest colors) i))
          ;     (text (@ 'x (+ 25 (* i 100)) 'y (- svg-height 10)) user)))
))))
