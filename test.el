(require 'keyboard-grabber)

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
(kg-grab-keyboard --conn)
(kg-run-event-loop --conn)

;; Local Variables:
;; make-backup-files: nil
;; End:
