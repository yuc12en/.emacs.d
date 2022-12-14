;;; init-utils.el  -- provide some handy functions that fits into my workflow
;;; commentary:
;;; code:


;; time-string
(defun insert-time-string ()
  "Insert time string"
  (interactive)
  (let ((time-string (format-time-string "%U %w %m %d")))
    (progn
    (setq time-string (split-string time-string))
    (setq Week (car time-string))
    (setq Day (cadr time-string))
    (setq Mon (caddr time-string))
    (setq Date (cadddr time-string))
    (insert (format "week%s %s %s/%s" (week-handle Week) (day-handle Day) Mon Date)))))
(defun week-handle(w)
  "Set the week number W begins at Sep"
  (setq w (string-to-number w))
  (setq w (- w 34))
  (number-to-string w))
(defun day-handle(d)
 "Set the day number D to string"
  (setq d (string-to-number d))
  (cond ((equal d 0) "Sun")
	((equal d 1) "Mon")
	((equal d 2) "Tues")
	((equal d 3) "Wed")
	((equal d 4) "Thur")
	((equal d 5) "Fri")
	((equal d 6) "Sat")))


(provide 'init-utils)
