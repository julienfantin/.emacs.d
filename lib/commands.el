;;; commands --- Inetractive commands
;;; Commentary:
;;
;;; Code:
;;

;; * Editing

;;;###autoload
(defun esk-align-current ()
  (interactive)
  (let ((current-prefix-arg '(4))) ; C-u
    (call-interactively 'align-current)))

;;;###autoload
(defun esk-backward-kill-word ()
  (interactive)
  (if (region-active-p)
      (if (bound-and-true-p paredit-mode)
          (call-interactively 'paredit-kill-region)
        (call-interactively 'kill-region))
    (if (bound-and-true-p paredit-mode)
        (call-interactively 'paredit-backward-kill-word)
      (call-interactively 'backward-kill-word))))

;;;###autoload
(defun esk-cleanup ()
  (interactive)
  (save-excursion
    (unless (use-region-p)
      (goto-char (point-min))
      (push-mark)
      (goto-char (point-max)))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (whitespace-cleanup)))
  (message (format "%s cleaned!" (buffer-name))))


;; * Windows

;;;###autoload
(defun esk-alternate-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

;;;###autoload
(defun esk-alternate-window ()
  (interactive)
  (next-multiframe-window)
  (other-window -1))

;;;###autoload
(defun inc-transparency ()
  (interactive)
  (let ((alpha (or (car (frame-parameter (selected-frame) 'alpha)) 100)))
    (set-frame-parameter (selected-frame) 'alpha (list  (- alpha 5)  (- alpha 5)))))

;;;###autoload
(defun dec-transparency ()
  (interactive)
  (let ((alpha (or (car (frame-parameter (selected-frame) 'alpha)) 100)))
    (set-frame-parameter (selected-frame) 'alpha (list  (+ alpha 5)  (+ alpha 5)))))

;;;###autoload
(defun esk-window-focus-toggle ()
  (interactive)
  (when (not (window-minibuffer-p (selected-window)))
    (if (= 1 (count-windows))
        (jump-to-register ?u)
      (window-configuration-to-register ?u)
      (delete-other-windows))))

;;;###autoload
(defun esk-window-split-toggle ()
  "Switch window split from horizontally to vertically, or vice
versa. i.e. change right window to bottom, or change bottom
window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

;; Copied from https://github.com/ramnes/move-border

(defun xor (b1 b2)
  (or (and b1 b2)
      (and (not b1) (not b2))))

(defun move-border-left-or-right (arg dir)
  "General function covering move-border-left and move-border-right. If DIR is
t, then move left, otherwise move right."
  (interactive)
  (if (null arg) (setq arg 3))
  (let ((left-edge (nth 0 (window-edges))))
    (if (xor (= left-edge 0) dir)
        (shrink-window arg t)
      (enlarge-window arg t))))

(defun move-border-up-or-down (arg dir)
  "General function covering move-border-up and move-border-down. If DIR is
t, then move up, otherwise move down."
  (interactive)
  (if (null arg) (setq arg 3))
  (let ((top-edge (nth 1 (window-edges))))
    (if (xor (= top-edge 0) dir)
        (shrink-window arg nil)
      (enlarge-window arg nil))))

;;;###autoload
(defun move-border-left (arg)
  (interactive "P")
  (move-border-left-or-right arg t))

;;;###autoload
(defun move-border-right (arg)
  (interactive "P")
  (move-border-left-or-right arg nil))

;;;###autoload
(defun move-border-up (arg)
  (interactive "P")
  (move-border-up-or-down arg t))

;;;###autoload
(defun move-border-down (arg)
  (interactive "P")
  (move-border-up-or-down arg nil))


;; * Mark

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(provide 'commands)
;;; commands.el ends here
