;;;; I added domain (unix) socket to myelin (another project of mine),
;;;; I want to see if it would be hard to add them to breeze too.

(defvar myelin-client nil
  "Client to myelin's domain socket")

(setf myelin-client
      (make-network-process
       :name "myelin"
       :family 'local
       :remote (concat
                (xdg-runtime-dir)
                "/myelin/myelin.sock")
       :coding 'utf-8
       :filter (lambda (proc string)
                 (message "Got %S from myelin (%S)" string proc))
       ;; :buffer "*myelin*"
       ))

(process-send-string myelin-client "hi")
;; => Got "hello" from myelin (#<process myelin<2>>)

;; Well, that was easy enough, but myelin's domain socket server is
;; currently implemented to receive one command and then close the
;; connection. It will probably harder to manager with breeze,
;; especially if the message get big.
