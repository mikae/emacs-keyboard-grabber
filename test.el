(require 'keyboard-grabber)
(kg-test)
(let ((--evval        (list "RELEASED"
                            "PRESSED "
                            "REPEATED"))
      (--device-index 2)
      (--fd           nil)
      (--counter      50)
      (--vec          (make-vector 4 0))
      (--type         nil)
      (--code         nil)
      (--value        nil))
  (setq --fd (kg-open-device --device-index))
  ;;(kg-grab-device --fd)
  (while (> --counter 0)
    (kg-read-event --fd --vec)
    (setq --code  (elt --vec 1))
    (setq --type  (elt --vec 2))
    (setq --value (elt --vec 3))

    (when (and (eq --type 1)
               (>=  --value 0)
               (<=  --value 2))
      (message "%s 0x%x %d"
               (elt --evval --value)
               --code
               --code)
      (setq --counter (1- --counter)))
    )
  ;;(kg-ungrab-device --fd)
  (kg-close-device  --fd))


;; Local Variables:
;; make-backup-files: nil
;; End:
