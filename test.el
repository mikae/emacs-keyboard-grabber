;; (require 'keyboard-grabber)

(defun my-key-event-hook ()
  (let ((--event-type (elt kg-last-event 0))
        (--key-code   (elt kg-last-event 1)))
    ))

(add-hook 'kg-key-event-hook
          #'my-key-event-hook)

(setq --conn (kg-open-connection))

(defun nyan ()
  (let* ((vec (kg-read-event --conn))
         (--event-type (elt vec 0))
         (--key-code (elt vec 1)))
    (cond
     ((eq --event-type
          kg-key-press)
      (message "Press: %d" --key-code))
     ((eq --event-type
          kg-key-release)
      (message "Release: %d" --key-code))))

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
