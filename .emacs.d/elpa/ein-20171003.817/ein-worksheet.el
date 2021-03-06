;;; ein-worksheet.el --- Worksheet module

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ein-worksheet.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ein-worksheet.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ein-worksheet.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'ewoc)

(require 'ein-core)
(require 'ein-utils)
(require 'ein-cell)
(require 'ein-kill-ring)


;;; Configuration

(define-obsolete-variable-alias
  'ein:notebook-enable-undo 'ein:worksheet-enable-undo "0.2.0")

(defcustom ein:worksheet-show-slide-data nil
  "Controls whether to show slide metadata by default when
  opening or creating worksheets. Note that viewing of slide
  metadata can be toggled in an open worksheet using the command
  C-cS."
  :type 'boolean
  :group 'ein)

(defcustom ein:worksheet-enable-undo 'yes
  "Configure undo in notebook buffers.

`no' : symbol
    Do not use undo in notebook buffers.  It is the safest option.
`yes' : symbol
    Enable undo in notebook buffers.  You can't undo after
    modification of cell (execution, add, remove, etc.).  This
    is default.
`full' : symbol
    Enable full undo in notebook buffers.  It is powerful but
    sometime (typically after the cell specific commands) undo
    mess up notebook buffer.  Use it on your own risk.  When the
    buffer is messed up, you can just redo and continue editing,
    or save it once and reopen it if you want to be careful.

You need to reopen the notebook buffer to reflect the change of
this value."
  :type '(choice (const :tag "No" no)
                 (const :tag "Yes" yes)
                 (const :tag "Full" full))
  :group 'ein)


;;; Configuration getter

(defun ein:worksheet-empty-undo-maybe ()
  "Empty `buffer-undo-list' if `ein:worksheet-enable-undo' is `yes'."
  ;; (when (eq ein:worksheet-enable-undo 'yes)
  ;;   (setq buffer-undo-list nil))
  )


;;; Class and variable

(defvar ein:worksheet-buffer-name-template "*ein: %s/%s*")

(ein:deflocal ein:%worksheet% nil
  "Buffer local variable to store an instance of `ein:worksheet'.")


;;; Initialization of object and buffer

(defun ein:worksheet-new (nbformat get-notebook-name discard-output-p
                                   kernel events &rest args)
  (apply #'make-instance 'ein:worksheet
         :nbformat nbformat :get-notebook-name get-notebook-name
         :discard-output-p discard-output-p :kernel kernel :events events
         :show-slide-data-p ein:worksheet-show-slide-data
         args))

(defmethod ein:worksheet-bind-events ((ws ein:worksheet))
  (with-slots (events) ws
    ;; Bind events for sub components:
    (mapc (lambda (cell) (setf (slot-value cell 'events) events))
          (ein:worksheet-get-cells ws))))

(defun ein:worksheet-class-bind-events (events)
  "Binds event handlers which are not needed to be bound per instance."
  (ein:events-on events
                 'maybe_reset_undo.Worksheet
                 (lambda (-ignore- cell)
                   (ein:with-live-buffer (ein:cell-buffer cell)
                     (ein:worksheet-empty-undo-maybe))))
  (ein:events-on events 'set_next_input.Worksheet
                 #'ein:worksheet--set-next-input)
  (ein:events-on events 'set_dirty.Worksheet #'ein:worksheet--set-dirty))

(defun ein:worksheet--set-next-input (-ignore- data)
  (destructuring-bind (&key cell text) data
    (ein:with-live-buffer (ein:cell-buffer cell)
      (ein:and-let* ((ws ein:%worksheet%)
                     (new-cell
                      (ein:worksheet-insert-cell-below ws 'code cell)))
        (ein:cell-set-text new-cell text)
        (setf (slot-value ws 'dirty) t)))))

(defun ein:worksheet--set-dirty (-ignore- data)
  "Set dirty flag of worksheet in which CELL in DATA locates."
  (destructuring-bind (&key value cell) data
    (ein:with-live-buffer (ein:cell-buffer cell)
      (ein:worksheet-set-modified-p ein:%worksheet% value))))

(defmethod ein:worksheet-notebook-name ((ws ein:worksheet))
  (ein:funcall-packed (ein:worksheet--notebook-name ws)))

(defmethod ein:worksheet-url-or-port ((ws ein:worksheet))
  (ein:kernel-url-or-port (ein:worksheet--kernel ws)))

(defmethod ein:worksheet-name ((ws ein:worksheet))
  (plist-get (ein:worksheet--metadata ws) :name))

(defmethod ein:worksheet-set-name ((ws ein:worksheet) name)
  "Set worksheet name.

\(fn ws name)"
  (assert (stringp name) nil "NAME must be a string.  Got: %S" name)
  (setf (ein:worksheet--metadata ws) (plist-put (ein:worksheet--metadata ws) :name name)))

(defmethod ein:worksheet-full-name ((ws ein:worksheet))
  (let ((nb-name (ein:worksheet-notebook-name ws)))
    (ein:aif (ein:worksheet-name ws)
        (concat nb-name "/" it)
      nb-name)))

(defmethod ein:worksheet-buffer ((ws ein:worksheet))
  (ein:and-let* (((slot-boundp ws :ewoc))
                 (ewoc (ein:worksheet--ewoc ws))
                 (buffer (ewoc-buffer ewoc))
                 ((buffer-live-p buffer)))
    buffer))

(defmethod ein:worksheet--buffer-name ((ws ein:worksheet))
  (format ein:worksheet-buffer-name-template
          (ein:worksheet-url-or-port ws)
          (ein:worksheet-full-name ws)))

(defmethod ein:worksheet--get-buffer ((ws ein:worksheet))
  (or (ein:worksheet-buffer ws)
      (generate-new-buffer (ein:worksheet--buffer-name ws))))

(defmethod ein:worksheet-set-buffer-name ((ws ein:worksheet))
  (ein:with-live-buffer (ein:worksheet-buffer ws)
    (rename-buffer (ein:worksheet--buffer-name ws) t)))

(defmethod ein:worksheet-set-modified-p ((ws ein:worksheet) dirty)
  (ein:with-live-buffer (ein:worksheet-buffer ws)
    (set-buffer-modified-p dirty))
  (setf (slot-value ws 'dirty) dirty))

(defun ein:worksheet-toggle-slideshow-view ()
  "Changes the display of
 slideshow metadata in the current worksheet."
  (interactive)
  (let ((ws (ein:worksheet--get-ws-or-error)))
    (setf (ein:worksheet--show-slide-data-p ws)
          (not (ein:worksheet--show-slide-data-p ws)))
    (ein:worksheet-render ws)))

(defmethod ein:worksheet-render ((ws ein:worksheet))
  (with-current-buffer (ein:worksheet--get-buffer ws)
    (setq ein:%worksheet% ws)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (let ((ewoc (ein:ewoc-create 'ein:worksheet-pp
                                   (ein:propertize-read-only "\n")
                                   nil t))
            (cells (ein:worksheet--saved-cells ws)))
        (setf (ein:worksheet--ewoc ws) ewoc)
        (if cells
            (mapc (lambda (c)
                    (setf (slot-value c 'ewoc) ewoc)
                    (ein:cell-enter-last c))
                  cells)
          (ein:worksheet-insert-cell-below ws 'code nil t))))
    (set-buffer-modified-p nil)
    ;;(setq buffer-undo-list nil)  ; clear undo history
    (when (eq ein:worksheet-enable-undo 'no)
      (setq buffer-undo-list t))
    (ein:worksheet-bind-events ws)
    (ein:worksheet-set-kernel ws)
    (ein:log 'info "Worksheet %s is ready" (ein:worksheet-full-name ws))))

(defun ein:worksheet-pp (ewoc-data)
  (let ((path (ein:$node-path ewoc-data))
        (data (ein:$node-data ewoc-data)))
    (case (car path)
      (cell (ein:cell-pp (cdr path) data)))))


;;; Persistance and loading

(defmethod ein:worksheet-from-json ((ws ein:worksheet) data)
  (destructuring-bind (&key cells metadata &allow-other-keys) data
    (setf (slot-value ws 'metadata) metadata)
    (setf (slot-value ws 'saved-cells)
          (mapcar (lambda (data) (ein:cell-from-json data)) cells)))
  ws)

(defmethod ein:worksheet-from-cells ((ws ein:worksheet) cells)
  )

(defmethod ein:worksheet-to-json ((ws ein:worksheet))
  "Convert worksheet WS into JSON ready alist.
It sets buffer internally so that caller doesn not have to set
current buffer."
  (let* ((discard-output-p (ein:worksheet--discard-output-p ws))
         (cells (ein:with-possibly-killed-buffer (ein:worksheet-buffer ws)
                  (mapcar (lambda (c)
                            (ein:cell-to-json
                             c (ein:funcall-packed discard-output-p c)))
                          (ein:worksheet-get-cells ws)))))
    `((cells . ,(apply #'vector cells))
      ,@(ein:aand (ein:worksheet--metadata ws) `((metadata . ,it))))))

(defmethod ein:worksheet-to-nb4-json ((ws ein:worksheet) wsidx)
  (let* ((discard-output-p (slot-value ws 'discard-output-p))
         (cells (ein:with-possibly-killed-buffer (ein:worksheet-buffer ws)
                  (mapcar (lambda (c)
                            (ein:cell-to-nb4-json
                             c wsidx (ein:funcall-packed discard-output-p c)))
                          (ein:worksheet-get-cells ws)))))
    cells))

(defmethod ein:worksheet-save-cells ((ws ein:worksheet) &optional deactivate)
  "Save cells in worksheet buffer in cache before killing the buffer.

.. warning:: After called with non-nil DEACTIVATE flag is given,
   cells in worksheet cannot be used anymore.  Use only just
   before killing the buffer.

You don't need to set current buffer to call this function.
Do nothing when the worksheet WS has no buffer.

If the `:dont-save-cells' slot is non-nil (meaning that
`ein:worksheet-dont-save-cells' has been called), cells in the
worksheet buffer are not saved.  When the DEACTIVATE option is
given, cached cells are deactivated instead of the cells in
buffer.  Calling this function unconditionally resets
`:dont-save-cells' flag to nil to make caching work when the
worksheet WS is reopened.

\(fn ws deactivate)"
  (when (ein:worksheet-has-buffer-p ws)
    (unless (slot-value ws 'dont-save-cells)
      (let ((cells (ein:worksheet-get-cells ws)))
        (with-current-buffer (ein:worksheet-buffer ws)
          (mapc #'ein:cell-save-text cells))
        (when deactivate (mapc #'ein:cell-deactivate cells))
        (setf (slot-value ws 'saved-cells) cells)))
    (when deactivate
      (mapc #'ein:cell-deactivate (slot-value ws 'saved-cells))))
  (setf (slot-value ws 'dont-save-cells) nil))

(defmethod ein:worksheet-dont-save-cells ((ws ein:worksheet))
  "Turn on `:dont-save-cells' flag so that next call on
`ein:worksheet-save-cells' actually do nothing.

\(fn ws)"
  (setf (slot-value ws 'dont-save-cells) t))


;;; Cell indexing, retrieval, etc.

(defmethod ein:worksheet-cell-from-type ((ws ein:worksheet) type &rest args)
  "Create a cell of TYPE (symbol or string)."
  ;; FIXME: unify type of TYPE to symbol or string.
  (apply #'ein:cell-from-type
         (format "%s" type)
         :ewoc (slot-value ws 'ewoc)
         :events (slot-value ws 'events)
         args))

(defmethod ein:worksheet-get-cells ((ws ein:worksheet))
  (if (ein:worksheet-has-buffer-p ws)
      (let* ((ewoc (slot-value ws 'ewoc))
             (nodes (ewoc-collect ewoc
                                  (lambda (n) (ein:cell-node-p n 'prompt)))))
        (mapcar #'ein:$node-data nodes))
    (slot-value ws 'saved-cells)))

(defmethod ein:worksheet-ncells ((ws ein:worksheet))
  (length (ein:worksheet-get-cells ws)))

(defun ein:worksheet-get-ewoc (&optional ws)
  (ein:aand (or ws ein:%worksheet%) (slot-value it 'ewoc)))

(defun ein:worksheet-get-current-ewoc-node (&optional pos)
  (ein:aand (ein:worksheet-get-ewoc) (ewoc-locate it pos)))

(defun ein:worksheet-get-nearest-cell-ewoc-node (&optional pos max cell-p)
  (ein:and-let* ((ewoc-node (ein:worksheet-get-current-ewoc-node pos)))
    ;; FIXME: can be optimized using the argument `max'
    (while (and ewoc-node
                (not (and (ein:cell-ewoc-node-p ewoc-node)
                          (if cell-p
                              (funcall cell-p
                                       (ein:cell-from-ewoc-node ewoc-node))
                            t))))
      (setq ewoc-node (ewoc-next (slot-value ein:%worksheet% 'ewoc) ewoc-node)))
    ewoc-node))

(defun* ein:worksheet-get-current-cell (&key pos noerror
                                             (cell-p #'ein:basecell-child-p))
  "Return a cell at POS.  If POS is not given, it is assumed be the
current cursor position.  When the current buffer is not worksheet
buffer or there is no cell in the current buffer, return `nil'."
  (let ((cell (ein:cell-from-ewoc-node
               (ein:worksheet-get-current-ewoc-node pos))))
    (if (funcall cell-p cell)
        cell
      (unless noerror
        (error "No cell of type %s found at current position." cell-p)))))

(defun ein:worksheet-at-codecell-p ()
  (ein:worksheet-get-current-cell :noerror t :cell-p #'ein:codecell-p))

(defun ein:worksheet-get-cells-in-region (beg end)
  (ein:clip-list (ein:aand ein:%worksheet% (ein:worksheet-get-cells it))
                 (ein:worksheet-get-current-cell :pos beg)
                 (ein:worksheet-get-current-cell :pos end)))

(defun* ein:worksheet-get-cells-in-region-or-at-point
    (&key noerror (cell-p #'ein:basecell-child-p))
  (or (ein:filter cell-p
                  (if (region-active-p)
                      (ein:worksheet-get-cells-in-region (region-beginning)
                                                         (region-end))
                    (list (ein:worksheet-get-current-cell))))
      (unless noerror
        (error "Cell not found"))))


;;; Insertion and deletion of cells

(defun ein:worksheet--get-ws-or-error ()
  (or ein:%worksheet% (error "Not in worksheet buffer.")))

(defun ein:worksheet-focus-cell ()
  (ein:aand (ein:worksheet-get-current-cell :noerror t) (ein:cell-goto it)))

(defun ein:worksheet-delete-cell (ws cell &optional focus)
  "Delete a cell.  \(WARNING: no undo!)
This command has no key binding because there is no way to undo
deletion.  Use kill to play on the safe side.

If you really want use this command, you can do something like this
\(but be careful when using it!)::

  \(define-key ein:notebook-mode-map \"\\C-c\\C-d\"
              'ein:worksheet-delete-cell)"
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)
                     t))
  (let ((inhibit-read-only t)
        (buffer-undo-list t))        ; disable undo recording
    (apply #'ewoc-delete
           (slot-value ws 'ewoc)
           (ein:cell-all-element cell)))
  (oset ws :dirty t)
  ;;(ein:worksheet-empty-undo-maybe)
  (when focus (ein:worksheet-focus-cell)))

(defun ein:worksheet-kill-cell (ws cells &optional focus)
  "Kill (\"cut\") the cell at point or cells in region.
Note that the kill-ring for cells is not shared with the default
kill-ring of Emacs (kill-ring for texts)."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-cells-in-region-or-at-point)
                     t))
  (when cells
    (mapc (lambda (c)
            (ein:cell-save-text c)
            (ein:worksheet-delete-cell ws c)
            (ein:cell-deactivate c))
          cells)
    (ein:kill-new cells)
    (when focus
      (deactivate-mark)
      (ein:worksheet-focus-cell))))

(defun ein:worksheet-copy-cell (cells)
  "Copy the cell at point.  (Put the current cell into the kill-ring.)"
  (interactive
   (list (when (ein:worksheet--get-ws-or-error)
           (prog1 (ein:worksheet-get-cells-in-region-or-at-point)
             (deactivate-mark)))))
  (let ((cells (mapcar
                (lambda (c)
                  (ein:cell-deactivate (ein:cell-copy c))) cells)))
    (ein:log 'info "%s cells are copied." (length  cells))
    (ein:kill-new cells)))

(defun ein:worksheet-insert-clone-below (ws cell pivot)
  (let ((clone (ein:cell-copy cell)))
    ;; Cell can be from another buffer, so reset `ewoc'.
    (setf (ein:basecell--ewoc clone) (ein:worksheet--ewoc ws))
    ;(oset clone :ewoc (oref ws :ewoc))
    (ein:worksheet-insert-cell-below ws clone pivot)
    clone))

(defun ein:worksheet-yank-cell (ws &optional n)
  "Insert (\"paste\") the latest killed cell.
Prefixes are act same as the normal `yank' command."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (let ((arg current-prefix-arg))
                       (cond ((listp arg) 0)
                             ((eq arg '-) -2)
                             (t (1- arg))))))
  (let* ((cell (ein:worksheet-get-current-cell :noerror t)) ; can be nil
         (killed (ein:current-kill n)))
    (loop for c in killed
          with last = cell
          do (setq last (ein:worksheet-insert-clone-below ws c last))
          finally (ein:cell-goto last))))

(defun ein:worksheet-maybe-new-cell (ws type-or-cell)
  "Return TYPE-OR-CELL as-is if it is a cell, otherwise return a new cell."
  (let ((cell (if (cl-typep type-or-cell 'ein:basecell)                    ;(ein:basecell-child-p type-or-cell)
                  type-or-cell
                (ein:worksheet-cell-from-type ws type-or-cell))))
    ;; When newly created or copied, kernel is not attached or not the
    ;; kernel of this worksheet.  So reset it here.
    (when (ein:codecell-p cell)
      (setf (slot-value cell 'kernel) (slot-value ws 'kernel)))
    (setf (slot-value cell 'events) (slot-value ws 'events))
    cell))

(defun ein:worksheet-insert-cell-below (ws type-or-cell pivot &optional focus)
  "Insert cell below.  Insert markdown cell instead of code cell
when the prefix argument is given.

When used as a lisp function, insert a cell of TYPE-OR-CELL just
after PIVOT and return the new cell."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (if current-prefix-arg 'markdown 'code)
                     (ein:worksheet-get-current-cell :noerror t) ; can be nil
                     t))
  (let ((cell (ein:worksheet-maybe-new-cell ws type-or-cell)))
    (cond
     ((= (ein:worksheet-ncells ws) 0)
      (ein:cell-enter-last cell))
     (pivot
      (ein:cell-insert-below pivot cell))
     (t (error
         "PIVOT is `nil' but ncells != 0.  There is something wrong...")))
                                        ;(ein:worksheet-empty-undo-maybe)
    (push `(apply ein:worksheet-delete-cell ,ws ,cell) buffer-undo-list)
    (oset ws :dirty t)
    (when focus (ein:cell-goto cell))
    cell))

(defun ein:worksheet-insert-cell-above (ws type-or-cell pivot &optional focus)
  "Insert cell above.  Insert markdown cell instead of code cell
when the prefix argument is given.
See also: `ein:worksheet-insert-cell-below'."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (if current-prefix-arg 'markdown 'code)
                     (ein:worksheet-get-current-cell :noerror t) ; can be nil
                     t))
  (let ((cell (ein:worksheet-maybe-new-cell ws type-or-cell)))
    (cond
     ((< (ein:worksheet-ncells ws) 2)
      (ein:cell-enter-first cell))
     (pivot
      (let ((prev-cell (ein:cell-prev pivot)))
        (if prev-cell
            (ein:cell-insert-below prev-cell cell)
          (ein:cell-enter-first cell))))
     (t (error
         "PIVOT is `nil' but ncells > 0.  There is something wrong...")))
    (push `(apply ein:worksheet-delete-cell ,ws ,cell) buffer-undo-list)
    (oset ws :dirty t)
    (when focus (ein:cell-goto cell))
    cell))

(defun ein:worksheet-toggle-cell-type (ws cell &optional focus)
  "Toggle the cell type of the cell at point.
Use `ein:worksheet-change-cell-type' to change the cell type
directly."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)
                     t))
  (let ((type (case (slot-value ws 'nbformat)
                (2 (ein:case-equal (slot-value cell 'cell-type)
                     (("code") "markdown")
                     (("markdown") "code")))
                (3 (ein:case-equal (slot-value cell 'cell-type)
                     (("code") "markdown")
                     (("markdown") "raw")
                     (("raw") "heading")
                     (("heading") "code")))
                (4 (ein:case-equal (slot-value cell 'cell-type)
                     (("code") "markdown")
                     (("markdown") "raw")
                     (("raw") "code"))))))
    (let ((relpos (ein:cell-relative-point cell))
          (new (ein:cell-convert-inplace cell type)))
      (when (ein:codecell-p new)
        (setf (slot-value new 'kernel) (slot-value ws 'kernel)))
      (ein:worksheet-empty-undo-maybe)
      (when focus (ein:cell-goto new relpos))))
  )

(defun ein:worksheet-toggle-slide-type (ws cell &optional focus)
  "Toggle the slide metadata of the cell at point. Available slide settings are:
 [slide, subslide, fragment, skip, notes, - (none)]."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)
                     t))
  (let ((new-slide-type (ein:case-equal (oref cell :slidetype)
                          (("-") "slide")
                          (("slide") "subslide")
                          (("subslide") "fragment")
                          (("fragment") "skip")
                          (("skip") "notes")
                          (("notes") "-"))))
    (message "changing slide type %s" new-slide-type)
    (oset cell :slidetype new-slide-type))
  (ewoc-invalidate (oref cell :ewoc) (ein:cell-element-get cell :prompt))
  (ein:worksheet-empty-undo-maybe)
  (when focus (ein:cell-goto cell)))

(defun ein:worksheet-change-cell-type (ws cell type &optional level focus)
  "Change the cell type of the current cell.
Prompt will appear in the minibuffer.

When used in as a Lisp function, TYPE (string) should be chose
from \"code\", \"markdown\", \"raw\" and \"heading\".  LEVEL is
an integer used only when the TYPE is \"heading\"."
  (interactive
   (let* ((ws (ein:worksheet--get-ws-or-error))
          (cell (ein:worksheet-get-current-cell))
          (choices (case (slot-value ws 'nbformat)
                     (2 "cm")
                     (3 "cmr123456")
                     (4 "cmr123456")))
          (key (ein:ask-choice-char
                (format "Cell type [%s]: " choices) choices))
          (type (case key
                  (?c "code")
                  (?m "markdown")
                  (?r "raw")
                  (t "heading")))
          (level (when (equal type "heading")
                   (string-to-number (char-to-string key)))))
     (list ws cell type level t)))

  (let ((relpos (ein:cell-relative-point cell))
        (new (ein:cell-convert-inplace cell type)))
    (when (ein:codecell-p new)
      (setf (slot-value new 'kernel) (slot-value ws 'kernel)))
    (when level
      (ein:cell-change-level new level))
    (ein:worksheet-empty-undo-maybe)
    (when focus (ein:cell-goto new relpos))))

(defun ein:worksheet-split-cell-at-point (ws cell &optional no-trim focus)
  "Split cell at current position. Newlines at the splitting
point will be removed. This can be omitted by giving a prefix
argument \(C-u)."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)
                     current-prefix-arg
                     t))
  ;; FIXME: should I inhibit undo?
  (let* ((beg (set-marker (make-marker) (ein:cell-input-pos-min cell)))
         (pos (point-marker))
         (head (buffer-substring beg pos))
         (new (ein:worksheet-insert-cell-above ws
                                               (slot-value cell 'cell-type)
                                               cell)))
    (when (ein:headingcell-p cell)
      (ein:cell-change-level new (slot-value cell 'level)))
    (delete-region beg pos)
    (unless no-trim
      (setq head (ein:trim-right head "\n"))
      (save-excursion
        (goto-char pos)
        (let ((end (set-marker (make-marker) (ein:cell-input-pos-max cell))))
          (while (and (looking-at-p "\n") (< (point) end))
            (delete-char 1)))))
    (ein:cell-set-text new head)
    (ein:worksheet-empty-undo-maybe)
    (when focus (ein:cell-goto cell))))

(defun ein:worksheet-merge-cell (ws cell &optional next focus)
  "Merge previous cell into current cell.
If prefix is given, merge current cell into next cell."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)
                     current-prefix-arg
                     t))
  (unless next
    (setq cell (ein:cell-prev cell))
    (unless cell (error "No previous cell"))
    (ein:cell-goto cell))
  (let* ((next-cell (ein:cell-next cell))
         (head (ein:cell-get-text cell)))
    (assert next-cell nil "No cell to merge.")
    (ein:worksheet-delete-cell ws cell)
    (save-excursion
      (goto-char (ein:cell-input-pos-min next-cell))
      (insert head "\n"))
    (ein:worksheet-empty-undo-maybe)
    (when focus (ein:cell-goto next-cell))))


;;; Cell selection.

(defun* ein:worksheet-next-input-cell (ewoc-node &optional up (nth 1))
  "Return a cell containing the next input node after EWOC-NODE.
When UP is non-`nil', do the same for the *previous* input node.
When NTH is specified, return NTH cell.  Note that this function is
*not* defined for NTH=0; it returns nil."
  (unless (= nth 0)
    (when (< nth 0)
      (setq nth (* nth -1))
      (setq up (not up)))
    (let ((cell (ein:worksheet-next-input-cell-1 ewoc-node up)))
      (loop repeat (1- nth)
            with next = (if up #'ein:cell-prev #'ein:cell-next)
            if (funcall next cell)
            do (setq cell it)
            else
            return nil)
      cell)))

(defun ein:worksheet-next-input-cell-1 (ewoc-node &optional up)
  (let* ((ewoc-data (ewoc-data ewoc-node))
         (cell (ein:$node-data ewoc-data))
         (path (ein:$node-path ewoc-data))
         (element (nth 1 path)))
    (if (memql element (if up '(output footer) '(prompt)))
        cell
      (funcall (if up #'ein:cell-prev #'ein:cell-next) cell))))

(defun ein:worksheet-goto-input (ewoc-node up)
  (ein:aif (ein:worksheet-next-input-cell ewoc-node up)
      (ein:cell-goto it)
    (error "No %s input!" (if up "previous" "next"))))

(defun ein:worksheet-goto-next-input (ewoc-node)
  (interactive (list (and (ein:worksheet--get-ws-or-error)
                          (ein:worksheet-get-current-ewoc-node))))
  (ein:worksheet-goto-input ewoc-node nil))

(defun ein:worksheet-goto-prev-input (ewoc-node)
  (interactive (list (and (ein:worksheet--get-ws-or-error)
                          (ein:worksheet-get-current-ewoc-node))))
  (ein:worksheet-goto-input ewoc-node t))

(defun ein:worksheet-goto-next-cell-element (&optional nth up relpos prop)
  "Go to NTH next cell element named PROP and shift cursor by RELPOS.
Go to previous cell if UP is t.
Return t when the movement is succeeded."
  (unless prop (setq prop :input))
  (ein:and-let* ((current-node (ein:worksheet-get-current-ewoc-node))
                 (current-cell (ein:cell-from-ewoc-node current-node))
                 (target-cell
                  (if (and (= nth 1)
                           (eq (ein:cell-element-get current-cell :input)
                               current-node)
                           (not (and up
                                     (= (1+ (ewoc-location current-node))
                                        (point)))))
                      current-cell
                    (ein:worksheet-next-input-cell current-node up nth))))
    (ein:cell-goto target-cell relpos prop)
    t))

(defun ein:worksheet-beginning-of-cell-input (&optional arg)
  "Move backward to the beginning of a cell.
This function is for `beginning-of-defun-function', so behaves
similarly with `beginning-of-defun'.
It is set in `ein:notebook-multilang-mode'."
  (ein:worksheet-goto-next-cell-element (or arg 1) t))

(defun ein:worksheet-end-of-cell-input (&optional arg)
  "Move forward to the end of a cell.
This function is for `end-of-defun-function', so behaves
similarly with `end-of-defun'.
It is set in `ein:notebook-multilang-mode'."
  (ein:worksheet-goto-next-cell-element (or arg 1) nil 0 :after-input))


;;; Cell movement

(defun ein:worksheet-move-cell (ws cell up)
  (ein:aif (if up (ein:cell-prev cell) (ein:cell-next cell))
      (let ((inhibit-read-only t)
            (pivot-cell it))
        (ein:with-undo-disabled
         (ein:cell-save-text cell)
         (ein:worksheet-delete-cell ws cell)
         (funcall (if up
                      #'ein:worksheet-insert-cell-above
                    #'ein:worksheet-insert-cell-below)
                  ws cell pivot-cell)
         (ein:cell-goto cell)
         (oset ws :dirty t))
        (push `(apply ein:worksheet-move-cell ,ws ,cell ,(not up)) buffer-undo-list))
    (error "No %s cell" (if up "previous" "next"))))

(defun ein:worksheet-move-cell-up (ws cell)
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)))
  (ein:worksheet-move-cell ws cell t))

(defun ein:worksheet-move-cell-down (ws cell)
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)))
  (ein:worksheet-move-cell ws cell nil))


;;; Cell collapsing and output clearing

(defun ein:worksheet-toggle-output (ws cell)
  "Toggle the visibility of the output of the cell at point.
This does not alter the actual data stored in the cell."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell
                      :cell-p #'ein:codecell-p)))
  (ein:cell-toggle-output cell)
  (ein:worksheet-empty-undo-maybe)
  (setf (slot-value ws 'dirty) t))

(defun ein:worksheet-set-output-visibility-all (ws &optional collapsed)
  "Show all cell output.  When prefix is given, hide all cell output."
  (interactive (list (ein:worksheet--get-ws-or-error) current-prefix-arg))
  (when collapsed (setq collapsed t))   ; force it to be a boolean
  (mapc (lambda (c)
          (when (ein:codecell-p c) (ein:cell-set-collapsed c collapsed)))
        (ein:worksheet-get-cells ws))
  (ein:worksheet-empty-undo-maybe)
  (setf (slot-value ws 'dirty) t))

(defun ein:worksheet-clear-output (cell &optional preserve-input-prompt)
  "Clear output from the current cell at point.
Do not clear input prompt when the prefix argument is given."
  (interactive (list (ein:worksheet-get-current-cell
                      :cell-p #'ein:codecell-p)
                     current-prefix-arg))
  (ein:cell-clear-output cell t t t)
  (unless preserve-input-prompt
    (ein:cell-set-input-prompt cell))
  (ein:worksheet-empty-undo-maybe))

(defun ein:worksheet-clear-all-output (ws &optional preserve-input-prompt)
  "Clear output from all cells.
Do not clear input prompts when the prefix argument is given."
  (interactive (list (ein:worksheet--get-ws-or-error) current-prefix-arg))
  (mapc (lambda (c) (ein:worksheet-clear-output c preserve-input-prompt))
        (ein:filter #'ein:codecell-p (ein:worksheet-get-cells ws))))


;;; Kernel related things

(defmethod ein:worksheet-set-kernel ((ws ein:worksheet))
  (mapc (lambda (cell) (setf (slot-value cell 'kernel) (slot-value ws 'kernel)))
        (ein:filter #'ein:codecell-p (ein:worksheet-get-cells ws))))

(defun ein:undo-execute-cell (ws cell old-cell)
  (ein:worksheet-insert-cell-below ws old-cell cell)
  (ein:worksheet-delete-cell ws cell))

(defun ein:worksheet-execute-cell (ws cell)
  "Execute code type CELL."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell
                      :cell-p #'ein:codecell-p)))
  (ein:kernel-if-ready (slot-value ws 'kernel)
    (push `(apply ein:undo-execute-cell ,ws ,cell ,(ein:cell-copy cell)) buffer-undo-list)
    (ein:log 'debug "buffer-undo-list: %s" (length buffer-undo-list))
    (ein:with-undo-disabled
     (ein:cell-execute cell)
     (oset ws :dirty t))
    (ein:log 'debug "buffer-undo-list: %s" (length buffer-undo-list))
    cell))

(defun ein:worksheet-execute-cell-and-goto-next (ws cell &optional insert)
  "Execute cell at point if it is a code cell and move to the
next cell, or insert if none."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)))
  (when (ein:codecell-p cell)
    (ein:worksheet-execute-cell ws cell))
  (ein:aif (and (not insert) (ein:cell-next cell))
      (ein:cell-goto it)
    (ein:worksheet-insert-cell-below ws 'code cell t)))

(defun ein:worksheet-execute-cell-and-insert-below (ws cell)
  "Execute cell at point if it is a code cell and insert a
cell bellow."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)))
  (ein:worksheet-execute-cell-and-goto-next ws cell t))

(defun ein:worksheet-execute-all-cell (ws)
  "Execute all cells in the current worksheet buffer."
  (interactive (list (ein:worksheet--get-ws-or-error)))
  (mapc #'ein:cell-execute
        (ein:filter #'ein:codecell-p (ein:worksheet-get-cells ws))))

(defun ein:worksheet-insert-last-input-history (ws cell index)
  "Insert INDEX-th previous history into CELL in worksheet WS."
  (ein:kernel-history-request
   (slot-value ws 'kernel)
   (list
    :history_reply
    (cons
     (lambda (cell content -metadata-not-used-)
       (destructuring-bind (session line-number input)
           (car (plist-get content :history))
         (if (eq (ein:worksheet-get-current-cell) cell)
             (ein:cell-set-text cell input)
           (ein:log 'warning
             "Cursor moved from the cell after history request."))
         (ein:log 'info "Input history inserted: session:%d line:%d"
                  session line-number)))
     cell))
   :hist-access-type "range"
   :session 0
   :start (- index)
   :stop (- 1 index)))

(defvar ein:worksheet--history-index 1)

(defun ein:worksheet--get-history-index (inc)
  "Increment history index by (possibly negative) INC.
Get history index for `ein:worksheet-previous-input-history' and
`ein:worksheet-next-input-history'.  Raise error if caller tries
to decrement index to less than or equal to 1."
  (if (or (eq last-command 'ein:worksheet-previous-input-history)
          (eq last-command 'ein:worksheet-next-input-history))
      (progn
        (setq ein:worksheet--history-index
              (+ ein:worksheet--history-index inc))
        (when (< ein:worksheet--history-index 1)
          (setq ein:worksheet--history-index 1)
          (warn "This is the latest input"))
        ein:worksheet--history-index)
    (setq ein:worksheet--history-index 1)))

(defun ein:worksheet-previous-input-history (ws cell index)
  "Insert the previous input in the execution history.
You can go back further in the history by repeating this command.
Use `ein:worksheet-next-input-history' to go forward in the
history."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)
                     (ein:worksheet--get-history-index +1)))
  (ein:worksheet-insert-last-input-history ws cell index))

(defun ein:worksheet-next-input-history (ws cell index)
  "Insert next input in the execution history.
You can go forward further in the history by repeating this
command.  Use `ein:worksheet-previous-input-history' to go back
in the history."
  (interactive (list (ein:worksheet--get-ws-or-error)
                     (ein:worksheet-get-current-cell)
                     (ein:worksheet--get-history-index -1)))
  (ein:worksheet-insert-last-input-history ws cell index))


;;; Metadata

(defun ein:worksheet-rename-sheet (ws name)
  "Change worksheet name (*not* notebook name)."
  (interactive (let ((ws (ein:worksheet--get-ws-or-error)))
                 (list ws
                       (read-from-minibuffer
                        "New worksheet name: " (ein:worksheet-name ws)))))
  (unless (equal name (or (ein:worksheet-name ws) ""))
    (ein:worksheet-set-name ws name)
    (ein:worksheet-set-modified-p ws t)
    (ein:worksheet-set-buffer-name ws)))


;;; Generic getter

(defun ein:get-url-or-port--worksheet ()
  (when (ein:worksheet-p ein:%worksheet%)
    (ein:worksheet-url-or-port ein:%worksheet%)))

(defun ein:get-kernel--worksheet ()
  (when (ein:worksheet-p ein:%worksheet%) (slot-value ein:%worksheet% 'kernel)))

;; in edit-cell-mode, worksheet is bound as src--ws
;; used by ein:get-kernel as a last option so completion, tooltips
;; work in edit-cell-mode
(defun ein:get-kernel--worksheet-in-edit-cell ()
  "Get kernel when in edit-cell-mode."
  (when (ein:worksheet-p ein:src--ws) (oref ein:src--ws :kernel)))

(defun ein:get-cell-at-point--worksheet ()
  (ein:worksheet-get-current-cell :noerror t))

(defun ein:get-traceback-data--worksheet ()
  (ein:aand (ein:get-cell-at-point--worksheet) (ein:cell-get-tb-data it)))


;;; Predicate

(defun ein:worksheet-buffer-p ()
  "Return non-`nil' if the current buffer is a worksheet buffer."
  ein:%worksheet%)

(defmethod ein:worksheet-has-buffer-p ((ws ein:worksheet))
  (ein:aand (ein:worksheet-buffer ws) (buffer-live-p it)))

(defmethod ein:worksheet-modified-p ((ws ein:worksheet))
  (let ((buffer (ein:worksheet-buffer ws)))
    (and (buffer-live-p buffer)
         (or (slot-value ws 'dirty)
             (buffer-modified-p buffer)))))


;;; Utility commands

(defun ein:worksheet-dedent-cell-text (cell)
  "Dedent text in CELL."
  (interactive (list (ein:worksheet-get-current-cell)))
  (let* ((beg (ein:cell-input-pos-min cell))
         (end (ein:cell-input-pos-max cell)))
    (indent-rigidly
     beg end (- (ein:find-leftmot-column beg end)))))


;;; Auto-execution

(defun ein:worksheet-toggle-autoexec (cell)
  "Toggle auto-execution flag of the cell at point."
  (interactive (list (ein:worksheet-get-current-cell #'ein:codecell-p)))
  (ein:cell-toggle-autoexec cell))

(defun ein:worksheet-turn-on-autoexec (cells &optional off)
  "Turn on auto-execution flag of the cells in region or cell at point.
When the prefix argument is given, turn off the flag instead.

To use autoexec feature, you need to turn on auto-execution mode
in connected buffers, using the `ein:connect-toggle-autoexec'
command."
  (interactive
   (list (ein:worksheet-get-cells-in-region-or-at-point
          :cell-p #'ein:codecell-p)
         current-prefix-arg))
  (mapc (lambda (c) (ein:cell-set-autoexec c (not off))) cells)
  (ein:log 'info "Turn %s auto-execution flag of %s cells."
           (if off "off" "on")
           (length cells)))

(defun ein:worksheet-execute-autoexec-cells (ws)
  "Execute cells of which auto-execution flag is on.
This function internally sets current buffer to the worksheet
buffer, so you don't need to set current buffer to call this
function."
  (interactive (list (ein:worksheet--get-ws-or-error)))
  (ein:with-live-buffer (ein:worksheet-buffer ws)
    (ein:kernel-if-ready (slot-value ws 'kernel)
      (mapc #'ein:cell-execute
            (ein:filter #'ein:cell-autoexec-p
                        (ein:worksheet-get-cells ws))))))


;;; Imenu

(defun ein:worksheet-imenu-create-index ()
  "`imenu-create-index-function' for notebook buffer."
  ;; As Imenu does not provide the way to represent level *and*
  ;; position, use #'s to do that.
  (loop for cell in (when (ein:worksheet-p ein:%worksheet%)
                      (ein:filter #'ein:headingcell-p
                                  (ein:worksheet-get-cells ein:%worksheet%)))
        for sharps = (loop repeat (slot-value cell 'level) collect "#")
        for text = (ein:cell-get-text cell)
        for name = (ein:join-str "" (append sharps (list " " text)))
        collect (cons name (ein:cell-input-pos-min cell))))

(defun ein:worksheet-imenu-setup ()
  "Called via notebook mode hooks."
  (setq imenu-create-index-function #'ein:worksheet-imenu-create-index))


;;; Workarounds

(defadvice fill-paragraph (around ein:worksheet-fill-paragraph activate)
  "Prevent \"Text is read-only\" error when filling paragraph in
EIN worksheet."
  (if ein:%worksheet%
      (let* ((cell (ein:worksheet-get-current-cell))
             (beg (copy-marker (ein:cell-input-pos-min cell))))
        (save-excursion
          (goto-char beg)
          (insert "\n"))
        (unwind-protect
            ad-do-it
          (save-excursion
            (goto-char beg)
            (delete-char 1))))
    ad-do-it))

(provide 'ein-worksheet)

;;; ein-worksheet.el ends here
