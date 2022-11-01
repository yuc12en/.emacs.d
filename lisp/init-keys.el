;;; init-keys --- provided functions to be inteactively called by general attatched which-keys.

;;; Commentary:
;;; code:

(defun motion/natrual-up ()
  (interactive)
  (evil-scroll-line-up 1)
  (evil-previous-line))

(defun motion/natrual-down ()
  (interactive)
  (evil-scroll-line-down 1)
  (evil-next-line))


(provide 'init-keys)
