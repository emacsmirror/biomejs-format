;;; biomejs-format.el --- Minor mode to format JS code with Biome on file save

;; Author: James Long and contributors
;; Maintainer: Kanon Kakuno <yadex205@yadex205.com>
;; Created: 26 March 2024
;; Url: https://github.com/yadex205/emacs-biomejs-format
;; SPDX-License-Identifier: BSD-3-Clause
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience wp edit js

;; Copyright (c) 2014 The go-mode Authors. All rights reserved.
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.
;; Portions Copyright (c) 2024 Kanon Kakuno

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;; * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;; * Neither the name of the copyright holder nor the names of its
;; contributors may be used to endorse or promote products derived from
;; this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.)

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Formats your JavaScript or other supported code using 'biome' on file save.

;;; Code:

(defgroup biomejs-format nil
  "Minor mode to format JS code with Biome on file save."
  :group 'languages
  :prefix "biomejs-format"
  :link '(url-link :tag "Repository" "https://github.com/biomejs/biome"))

(defcustom biomejs-format-biome-command "biome"
  "The `biome` command."
  :type 'string
  :group 'biomejs-format)

(defcustom biomejs-format-biome-args '("format")
  "List of args to send to biome command."
  :type '(repeat string)
  :group 'biomejs-format)

(defcustom biomejs-format-show-errors 'buffer
    "Where to display biome error output.
It can either be displayed in its own buffer, in the echo area, or not at all.
Please note that Emacs outputs to the echo area when writing
files and will overwrite biome's echo output if used from inside
a `before-save-hook'."
    :type '(choice
            (const :tag "Own buffer" buffer)
            (const :tag "Echo area" echo)
            (const :tag "None" nil))
      :group 'biomejs-format)

(defcustom biomejs-format-width-mode nil
  "Specify width when formatting buffer contents."
  :type '(choice
          (const :tag "Window width" window)
          (const :tag "Fill column" fill)
          (const :tag "None" nil))
  :group 'biomejs-format)

(defun biomejs-format--goto-line (line)
  "Move cursor to line LINE."
  (goto-char (point-min))
    (forward-line (1- line)))

(defun biomejs-format--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in biomejs-format--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (setq line-offset (- line-offset len))
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (biomejs-format--goto-line (- from line-offset))
                (setq line-offset (+ line-offset len))
                (let ((beg (point)))
                  (forward-line len)
                  (delete-region (point) beg))))
             (t
              (error "Invalid rcs patch or internal error in biomejs-format--apply-rcs-patch")))))))))

(defun biomejs-format--process-errors (filename errorfile errbuf)
  "Process errors for FILENAME, using ERRORFILE and display the output in ERRBUF."
  (with-current-buffer errbuf
    (if (eq biomejs-format-show-errors 'echo)
        (progn
          (message "%s" (buffer-string))
          (biomejs-format--kill-error-buffer errbuf))
      (insert-file-contents errorfile nil nil nil)
      ;; Convert the biome stderr to something understood by the compilation mode.
      (goto-char (point-min))
      (insert "biomejs errors:\n")
      (while (search-forward-regexp "^stdin" nil t)
        (replace-match (file-name-nondirectory filename)))
      (compilation-mode)
      (display-buffer errbuf))))

(defun biomejs-format--kill-error-buffer (errbuf)
  "Kill buffer ERRBUF."
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (with-current-buffer errbuf
        (erase-buffer))
      (kill-buffer errbuf))))

(defun biomejs-format ()
   "Format the current buffer according to the Biome tool."
   (interactive)
   (let* ((ext (file-name-extension buffer-file-name t))
          (bufferfile (make-temp-file "biomejs" nil ext))
          (outputfile (make-temp-file "biomejs" nil ext))
          (errorfile (make-temp-file "biomejs" nil ext))
          (errbuf (if biomejs-format-show-errors (get-buffer-create "*biomejs errors*")))
          (patchbuf (get-buffer-create "*biomejs patch*"))
          (coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8)
          (width-args
           (cond
            ((equal biomejs-format-width-mode 'window)
             (list "--line-width" (number-to-string (window-body-width))))
            ((equal biomejs-format-width-mode 'fill)
             (list "--line-width" (number-to-string fill-column)))
            (t
             '()))))
     (unwind-protect
         (save-restriction
           (widen)
           (write-region nil nil bufferfile)
           (if errbuf
               (with-current-buffer errbuf
                 (setq buffer-read-only nil)
                 (erase-buffer)))
           (with-current-buffer patchbuf
             (erase-buffer))
           (if (zerop (apply 'call-process
                             biomejs-format-biome-command bufferfile (list (list :file outputfile) errorfile)
                             nil (append biomejs-format-biome-args width-args (list "--stdin-file-path" buffer-file-name))))
               (progn
                 (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "--strip-trailing-cr" "-"
                                      outputfile)
                 (biomejs-format--apply-rcs-patch patchbuf)
                 (message "Applied biome with args `%s'" biomejs-format-biome-args)
                 (if errbuf (biomejs-format--kill-error-buffer errbuf)))
             (message "Could not apply biome")
             (if errbuf
                 (biomejs-format--process-errors (buffer-file-name) errorfile errbuf))))
       (kill-buffer patchbuf)
       (delete-file errorfile)
       (delete-file bufferfile)
       (delete-file outputfile))))

;;;###autoload
(define-minor-mode biomejs-format-mode
  "Runs Biome on file save when this mode is turned on."
  :lighter " BiomeFmt"
  :global nil
  (if biomejs-format-mode
      (add-hook 'before-save-hook 'biomejs-format nil 'local)
    (remove-hook 'before-save-hook 'biomejs-format 'local)))

(provide 'biomejs-format)
;;; biomejs-format.el ends here
