;;; window --- hydra for window management
;;; Commentary:
;;; Code:

(winner-mode 1)
(defhydra hydra-window ()
   "
^Movement^    | ^Split^      |    ^Switch^		  | ^Resize^
^--------^----+-^-----^------+----^------^------+-^------^-----
_←_         	| _v_ertical   |  	_b_uffer		  |  _a_ X←
_↓_         	| _h_orizontal |  	_f_ind files	|  _s_ X↓
_↑_         	| _z_ undo     | ace_W_indow 	    |  _w_ X↑
_→_         	| _Z_ reset    |   	_S_wap	     	|  _d_ X→
_F_ollow	  	|              |    _D_lt Other  	|  _m_aximize
_q_ quit    	| _o_nly this  |  	_c_lose win   |  _M_inimise
"
   ("<left>" windmove-left )
   ("<down>" windmove-down )
   ("<up>" windmove-up )
   ("<right>" windmove-right )
	 ;; these are unknown
   ("w" shrink-window)
   ("a" shrink-window-horizontally)
   ("s" enlarge-window)
   ("d" enlarge-window-horizontally)
   ("b" switch-to-buffer)
   ("f" counsel-find-file)
   ("F" follow-mode)
   ("W" (lambda ()
          (interactive)
          (ace-window 1)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
       )
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
       )
   ("h" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
       )
   ("S" (lambda ()
          (interactive)
          (ace-window 4)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body)))
   ("c" delete-window)
   ("D" (lambda ()
          (interactive)
          (ace-window 16)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
       )
   ("o" delete-other-windows)
	 ;; TODO: fix below
   ("m" ace-maximize-window)
   ("M" ace-minimize-window)
   ("z" (progn
          (winner-undo)
          (setq this-command 'winner-undo))
   )
   ("Z" winner-redo)
   ("q" nil)
   )

(global-set-key (kbd "C-x w") 'hydra-window/body)

;;; window.el ends here
