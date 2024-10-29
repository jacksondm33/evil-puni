;;; evil-puni.el --- Evil Puni -*- lexical-binding: t; -*-

(require 'evil)
(require 'puni)

(defvar evil-puni-mode-map (make-sparse-keymap))

(defvar evil-puni-bindings
  '(("d" . evil-puni-delete)
    ("D" . evil-puni-delete-line)
    ("y" . evil-puni-yank)
    ("Y" . evil-puni-yank-line)
    ("c" . evil-puni-change)
    ("C" . evil-puni-change-line)))

(defun evil-puni--soft-deletion-region (beg end type &optional style)
  (if (eq type 'block)
      (cons beg end)
    (when (evil-visual-state-p)
      (setq style 'within)
      (evil-exit-visual-state))
    (let ((beg-after-blanks
           (save-excursion
             (goto-char beg)
             (puni--forward-blanks end)
             (point))))
      (if-let ((new-range
                (or (puni-soft-delete beg-after-blanks end nil 'precise nil nil t)
                    (if (eq style 'within)
                        (save-excursion
                          (puni-soft-delete
                           beg-after-blanks end nil style nil 'jump-and-reverse-delete t))
                      (puni-soft-delete beg-after-blanks end nil style nil nil t)))))
          (let ((new-beg (car new-range))
                (end (cdr new-range)))
            (when (and (eq type 'line) (= (char-before end) ?\n))
              (setq end (1- end)))
            (if (/= beg-after-blanks new-beg)
                (cons new-beg end)
              (cons beg end)))
        (cons beg beg)))))

(defun evil-puni--delete-for-change (beg end type register yank-handler)
  (evil-delete beg end type register yank-handler)
  (indent-according-to-mode))

(evil-define-operator evil-puni-delete (beg end type register yank-handler &optional style)
  "Safely delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (cl-destructuring-bind
      (beg . end) (evil-puni--soft-deletion-region beg end type (or style 'within))
    (let ((beg-bol (save-excursion (goto-char beg) (bolp))))
      (evil-delete beg end type register yank-handler)
      (cond
       ((and (eq type 'line)
             (not beg-bol)
             (/= beg end))
        (join-line 1))
       ((eq type 'line)
        (beginning-of-line)
        (if (and (save-excursion (end-of-line 0) (puni--in-comment-p))
                 (not (looking-at-p "\n")))
            (indent-according-to-mode)
          (join-line)
          (forward-line 1)))))))

(evil-define-operator evil-puni-delete-line (beg end type register yank-handler)
  "Safely delete to end of line."
  :motion evil-end-of-line-or-visual-line
  (interactive "<R><x><y>")
  (evil-puni-delete beg end type register yank-handler 'beyond))

(evil-define-operator evil-puni-yank (beg end type register yank-handler)
  "Save the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (cl-destructuring-bind
      (beg . end) (evil-puni--soft-deletion-region beg end type 'within)
    (evil-yank beg end type register yank-handler)))

(evil-define-operator evil-puni-yank-line (beg end type register)
  "Save whole lines into the kill-ring."
  :motion evil-line-or-visual-line
  :move-point nil
  (interactive "<R><x>")
  (cl-destructuring-bind
      (beg end type) (evil-expand-line-for-line-based-operators beg end type)
    (evil-puni-yank beg end type register)))

(evil-define-operator evil-puni-change (beg end type register yank-handler)
  "Safely change text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (cl-destructuring-bind
      (beg . end) (evil-puni--soft-deletion-region beg end type 'within)
    (evil-change beg end (if (eq type 'line) 'inclusive type) register yank-handler
                 (and (eq type 'line) #'evil-puni--delete-for-change))))

(evil-define-operator evil-puni-change-line (beg end type register yank-handler)
  "Safely change to end of line, or change whole line if characterwise visual mode."
  :motion evil-end-of-line-or-visual-line
  (interactive "<R><x><y>")
  (if (and (evil-visual-state-p) (eq type 'inclusive))
      (cl-destructuring-bind
          (beg end _type) (evil-expand-line-for-line-based-operators beg end type)
        (evil-puni-change beg end 'line register yank-handler))
    (evil-puni-change beg end type register yank-handler)))

(defun evil-puni-set-bindings ()
  (dolist (binding evil-puni-bindings)
    (evil-define-key 'normal evil-puni-mode-map (car binding) (cdr binding))))

;;;###autoload
(define-minor-mode evil-puni-mode
  "Enable keybindings for Evil Puni commands."
  :keymap evil-puni-mode-map
  (when evil-puni-mode
    (evil-normalize-keymaps)))

(evil-puni-set-bindings)

(provide 'evil-puni)
;;; evil-puni.el ends here
