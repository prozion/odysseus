#lang racket

(require "../lib/load/all.rkt")
(require "../graphics/svg.rkt")
(require "../graphics/color.rkt")
(require "../knowledge-base/tab-tree.rkt")
(require "../knowledge-base/html.rkt")

(provide (all-defined-out))

(define-catch (draw-gantt tabtree-file path svg-width svg-height #:tick-dates (tick-dates 'all))

  (define (get-start-time element jobs)
    (let* (
          (start ($ start element))
          (after ($ after element))
          (after-ids (if after (string-split after ",") empty))
          (afters (map (λ (id) (get-item-by-id-from-the-list jobs id)) after-ids))
          (start
            (cond
              ; ((and afters (not-empty? afters) start) start)
              ((and afters (not-empty? afters))
                (car (sort (map (λ (x) (get-end-time x jobs)) afters) d>)))
              (else
                start))))
      start))

  (define (get-end-time element jobs)
    (let* (
          (start (get-start-time element jobs))
          (len ($ length element))
          (len (if len (days-count len) 0))
          (end (or ($ end element) (d+ start len))))
      end))

  (define-catch (get-first-after element jobs)
    (let* (
          (after-ids ($ after element))
          (after-ids (if after-ids
                        (string-split ($ after element) ",")
                        #f))
          (afters (if after-ids
                    (map (λ (id) (get-item-by-id-from-the-list jobs id)) after-ids)
                    #f))
          (result
            (if afters
                (apply min (map (λ (x) ($ _order x)) afters))
                #f)))
      result))

  ; constants
  (define y0 50)
  (define track-h 30) ; height per one job bar
  (define colors (string-split "black #00B6F2 #0096B2 #CC46C7 #89B836 #F29A4C #B22E2A #4C57DB #74D95F" " "))
  ; (define colors (string-split "black #3EA6B2 #5656C2 #559139 #618AB8 #984EB8 #53AB4D" " "))
  (let* ((tabtree (parse-tab-tree tabtree-file))
        (path (split path "."))
        (jobs (get-$3 path tabtree))
        (start-times (map (λ (x) (get-start-time x jobs)) jobs))
        (end-times (map (λ (x) (get-end-time x jobs)) jobs))
        (min-start-time (apply min (map date->days start-times)))
        (max-end-time (apply max (map date->days end-times)))
        (days-length (- max-end-time min-start-time))
        (step (/f svg-width days-length))
        (users-list empty)
        )
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
                (adate-month-abbr (nth months (->number adate-month)))
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
              (when first-day? (text (@ 'x (+ (* 0.5 step) (* step tick)) 'y 12 'class "labels") adate-month-abbr))
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
                (user ($ user job))
                (user (and user (car (split user ","))))
                (_ (set! users-list (if (indexof? users-list user) users-list (pushr users-list user))))
                (color (if user (list-ref colors (indexof users-list user)) (list-ref colors 0)))
                (color (if (empty? (filter true? users-list)) (list-ref colors 1) color))
                (color (if (equal? ($ status job) "done") "#bbb" color)) ; (color/shadow (color/desaturate color -55) 15)
                (start-date (get-start-time job jobs))
                (end-date (get-end-time job jobs))
                (start (- (date->days start-date) min-start-time))
                (end (- (date->days end-date) min-start-time))
                (x (* step start))
                (dx (* step end))
                (width (- dx x))
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
                  (str (line 'x1 x 'y1 (+ y h) 'x2 x 'y2 y-first-after 'class "convergence_line") "\n"))
                ; bar
                (title ($ title job))
                (rect 'x x 'y y 'width width 'height h 'fill color 'opacity 0.6) "\n"
                ; (text (@ 'x (+ x 6) 'y (+ y 18) 'class "job_title_shadow") (namefy ($ id job))) "\n"
                (text (@ 'x (+ x 5) 'y (+ y (/ track-h 2.0) 2) 'class "job_title") (namefy ($ id job))) "\n"
              )
            )
          ))
        ; legend
        (for/fold
          ((res ""))
          ((user users-list) (i (in-naturals)))
          (str
            res "\n"
            (rect 'x (* i 100) 'y (- svg-height 20) 'width 20 'height 10 'fill (list-ref (rest colors) i))
            (text (@ 'x (+ 25 (* i 100)) 'y (- svg-height 10)) user))))))
