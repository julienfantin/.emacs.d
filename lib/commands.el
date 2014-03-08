;;; commands --- Inetractive commands
;;; Commentary:
;;
;;; Code:
;;
;; * Editing
;;

(defun cleanup-buffer-or-region ()
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (whitespace-cleanup)))
  (message (format "%s cleaned!" (buffer-name))))

;; * Windows
;;

(defun window-focus-toggle ()
  (interactive)
  (when (not (window-minibuffer-p (selected-window)))
    (if (= 1 (count-windows))
        (jump-to-register ?u)
      (window-configuration-to-register ?u)
      (delete-other-windows))))

(defun window-split-toggle ()
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

(provide 'commands)
;;; commands.el ends here
