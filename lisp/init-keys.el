;;; init-keys --- provided functions to be inteactively called by general attatched which-keys.

;;; Commentary:
;;; code:


(defun web/github ()
  (interactive)
  (eaf-open-browser "github.com"))

(defun web/youtube ()
  (interactive)
  (eaf-open-browser "www.youtube.com"))

(defun web/bing ()
  (interactive)
  (eaf-open-browser "www.bing.com"))

(defun web/baidu ()
  (interactive)
  (eaf-open-browser "www.baidu.com"))


(provide 'init-keys)
