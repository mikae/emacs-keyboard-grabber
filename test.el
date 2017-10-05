;; (require 'keyboard-grabber)

(defun my-key-event-hook ()
  (let ((--event-type (elt kg-last-event 0))
        (--key-code   (elt kg-last-event 1)))
    (cond
     ((eq --event-type
          kg-key-press)
      (message "Press: %d" --key-code))
     ((eq --event-type
          kg-key-release)
      (message "Release: %d" --key-code))
     (t nil))))

(add-hook 'kg-key-event-hook
          #'my-key-event-hook)

(setq --conn (kg-open-connection))

(defun nyan ()
  (kg-read-event --conn)
  (deferred:$
    (deferred:wait 10)
    (deferred:nextc it
      'nyan)))

(deferred:$
  (deferred:next
    (lambda ()
      (kg-grab-keyboard --conn)))
  (deferred:nextc it
    'nyan)
  )

;; (deferred:$
;;   (deferred:next
;;     (lambda () (message "deferred start")))
;;   (deferred:nextc it
;;     (lambda ()
;;       (message "chain 1")
;;       1))
;;   (deferred:nextc it
;;     (lambda (x)
;;       (message "chain 2 : %s" x)))
;;   (deferred:nextc it
;;     (lambda ()
;;       (read-minibuffer "Input a number: ")))
;;   (deferred:nextc it
;;     (lambda (x)
;;       (message "Got the number : %i" x)))
;;   (deferred:error it
;;     (lambda (err)
;;       (message "Wrong input : %s" err)))
;;   )
