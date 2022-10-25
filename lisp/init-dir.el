(defun dir/neo-here ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (progn 
       (neotree-find)
       (ace-window 'current-window))))



(provide 'init-dir)
