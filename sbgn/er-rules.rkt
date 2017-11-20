 #lang racket

(require "../lib/base.rkt")
(require "../lib/controls.rkt")
(require "../lib/debug.rkt")

(provide get-new-state get-synapse-signal get-axone-signal)

(define state-conversion-rules
 '(
  ; (activated blocked %) ; try different resolve solutions (different 'strategies' fixed also in code)
  ; in old-state new-state
   (activated blocked blocked)
   (blocked blocked blocked)
   (not-exists blocked not-exists)
   (catalyzed blocked blocked)
   (exists blocked blocked)
   (inhibited blocked blocked)
   (nop blocked blocked)
   (? blocked blocked)

   (activated exists exists)
   (blocked exists blocked)
   (not-exists exists not-exists)
   (catalyzed exists catalyzed)
   (exists exists exists)
   (inhibited exists inhibited)
   (nop exists exists)
   (? exists ?)

   (activated catalyzed exists)
   (blocked catalyzed blocked)
   (not-exists catalyzed not-exists)
   (catalyzed catalyzed catalyzed)
   (exists catalyzed catalyzed)
   (inhibited catalyzed ?)
   (nop catalyzed catalyzed)
   (? catalyzed ?)

   (activated inhibited exists)
   (blocked inhibited blocked)
   (not-exists inhibited not-exists)
   (catalyzed inhibited exists)
   (exists inhibited inhibited)
   (inhibited inhibited inhibited)
   (nop inhibited inhibited)
   (? inhibited ?)

   (activated ? ?)
   (blocked ? blocked)
   (not-exists ? not-exists)
   (catalyzed ? ?)
   (exists ? ?)
   (inhibited ? ?)
   (nop ? ?)
   (? ? ?)
))

(define (get-new-state in state)
  (for/or ((rule state-conversion-rules))
    (and
      (equal? (first rule) in)
      (equal? (second rule) state)
      (third rule))))

(define synapse-forming-rules
 '(
   ; state type signal
   (not-exists stimulation nop)
   (blocked stimulation nop)
   (inhibited stimulation nop)
   (? stimulation nop) ; nop - if stimulation is not able to switch the synapsed link from blocked to active
   (nop stimulation nop)
   (exists stimulation catalyzed)
   (catalyzed stimulation catalyzed)

   (not-exists necessary-stimulation blocked)
   (blocked necessary-stimulation blocked)
   (inhibited necessary-stimulation inhibited) ; !!
   (? necessary-stimulation ?)
   (nop necessary-stimulation blocked)
   (exists necessary-stimulation nop)
   (catalyzed necessary-stimulation nop)

   (not-exists absolute-stimulation nop)
   (blocked absolute-stimulation nop)
   (inhibited absolute-stimulation nop)
   (? absolute-stimulation ?)
   (nop absolute-stimulation nop)
   (exists absolute-stimulation activated)
   (catalyzed absolute-stimulation activated)

   (not-exists inhibition nop)
   (blocked inhibition nop)
   (inhibited inhibition nop)
   (? inhibition ?)
   (nop inhibition nop)
   (exists inhibition inhibited)
   (catalyzed inhibition inhibited)

   (not-exists absolute-inhibition nop)
   (blocked absolute-inhibition nop)
   (inhibited absolute-inhibition nop)
   (? absolute-inhibition ?)
   (nop absolute-inhibition nop)
   (exists absolute-inhibition blocked)
   (catalyzed absolute-inhibition blocked)

   (not-exists assignment reset)
   (blocked assignment reset)
   (inhibited assignment reset)
   (? assignment nop)
   (nop assignment nop)
   (exists assignment new-value)
   (catalyzed assignment new-value)

   (exists node exists)
   (not-exists node nop)
))

(define (get-synapse-signal state type)
  (for/or ((rule synapse-forming-rules))
    (and
      (equal? (first rule) state)
      (equal? (second rule) type)
      (third rule))))


(define axone-forming-rules
 '(
  ; state type signal
   (not-exists link not-exists)
   (blocked link blocked)
   (inhibited link inhibited)
   (? link ?)
   (nop link nop)
   (exists link exists)
   (catalyzed link catalyzed)

   (not-exists node not-exists)
   (exists node exists)
))

(define (get-axone-signal state type)
  (for/or ((rule axone-forming-rules))
    (and
      (equal? (first rule) state)
      (equal? (second rule) type)
      (third rule))))
