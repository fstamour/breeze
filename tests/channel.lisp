#|

Those were smoke tests that I was running manually when developping
this package.

I could add actual test to this package... but I already have lots of
tests on the commands which exercise this code _a lot_.

|#

(in-package #:breeze.channel)


(cons nil nil) == (list nil)

(let ((q (list nil)))
  (enqueue q 42)
  q)
((42) 42)

(let ((q (list nil)))
  (enqueue q 42)
  (enqueue q 1)
  q)
((42 1) 42 1)

(let ((q (list nil)))
  (enqueue q 42)
  (values (dequeue q) q))
42
(NIL)

(let ((q (list nil)))
  (enqueue q 42)
  (enqueue q 1)
  (values (dequeue q) q))
42
((1) 42 1)



(let ((c (make-channel)))
  (cons (bt2:make-thread
         (lambda () (receive c)))
        (bt2:make-thread
         (lambda () (send c 42)))))
