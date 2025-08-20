;; something like "transients" could be hacked with this:

(cl-loop
 repeat 3
 for key = (read-key "RET: ok q: cancel")
 until (memq key (list ?q ?\C-g (aref (key-parse "C-M-c") 0)))
 when (= key ?\C-m)
 collect :ok
 collect key)

(aref (kbd "RET") 0)
;; => 13 (#o15, #xd, ?\C-m)
