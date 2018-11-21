;;; tidal.el --- Interact with TidalCycles for live coding patterns  -*- lexical-binding: t; -*-

;; Copyright (C) 2012  alex@slab.org
;; Copyright (C) 2006-2008  rohan drape (hsc3.el)

;; Author: alex@slab.org
;; Homepage: https://github.com/tidalcycles/Tidal
;; Version: 0
;; Keywords: tools
;; Package-Requires: ((haskell-mode "16") (emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; notes from hsc3:
;; This mode is implemented as a derivation of `haskell' mode,
;; indentation and font locking is courtesy that mode.  The
;; inter-process communication is courtesy `comint'.  The symbol at
;; point acquisition is courtesy `thingatpt'.  The directory search
;; facilities are courtesy `find-lisp'.

;;; Code:


(require 'scheme)
(require 'comint)
(require 'thingatpt)
(require 'find-lisp)
(require 'pulse)
(require 'haskell-mode)

(defvar tidal-buffer
  "*tidal*"
  "*The name of the tidal process buffer (default=*tidal*).")

(defvar tidal-interpreter
  "ghci"
  "*The haskell interpeter to use (default=ghci).")

(defvar tidal-interpreter-version
  (substring (shell-command-to-string (concat tidal-interpreter " --numeric-version")) 0 -1)
  "*The version of tidal interpreter as a string.")

(defvar tidal-interpreter-arguments
  (list "-XOverloadedStrings"
        )
  "*Arguments to the haskell interpreter (default=none).")

(defvar tidal-literate-p
  t
  "*Flag to indicate if we are in literate mode (default=t).")

(defvar tidal-modules nil
  "Additional module imports.  See `tidal-run-region'.")

(make-variable-buffer-local 'tidal-literate-p)

(defun tidal-unlit (s)
  "Remove bird literate marks in S."
  (replace-regexp-in-string "^> " "" s))

(defun tidal-intersperse (e l)
  "Insert E between every element of list L."
  (when l
    (cons e (cons (car l) (tidal-intersperse e (cdr l))))))

(defun tidal-start-haskell ()
  "Start haskell."
  (interactive)
  (if (comint-check-proc tidal-buffer)
      (error "A tidal process is already running")
    (apply
     'make-comint
     "tidal"
     tidal-interpreter
     nil
     tidal-interpreter-arguments)
    (tidal-see-output))
  (tidal-send-string ":set prompt \"\"")
  (if (string< tidal-interpreter-version "8.2.0")
      (tidal-send-string ":set prompt2 \"\"")
    (tidal-send-string ":set prompt-cont \"\""))
  (tidal-send-string ":module Sound.Tidal.Context")
  (tidal-send-string "import qualified Sound.Tidal.Scales as Scales")
  (tidal-send-string "import qualified Sound.Tidal.Chords as Chords")
  (tidal-send-string "(cps, nudger, getNow) <- cpsUtils'")
  (tidal-send-string "(d1,t1) <- superDirtSetters getNow")
  (tidal-send-string "(d2,t2) <- superDirtSetters getNow")
  (tidal-send-string "(d3,t3) <- superDirtSetters getNow")
  (tidal-send-string "(d4,t4) <- superDirtSetters getNow")
  (tidal-send-string "(d5,t5) <- superDirtSetters getNow")
  (tidal-send-string "(d6,t6) <- superDirtSetters getNow")
  (tidal-send-string "(d7,t7) <- superDirtSetters getNow")
  (tidal-send-string "(d8,t8) <- superDirtSetters getNow")
  (tidal-send-string "(d9,t9) <- superDirtSetters getNow")
  (tidal-send-string "(d10,t10) <- superDirtSetters getNow")
  (tidal-send-string "(c1,ct1) <- dirtSetters getNow")
  (tidal-send-string "(c2,ct2) <- dirtSetters getNow")
  (tidal-send-string "(c3,ct3) <- dirtSetters getNow")
  (tidal-send-string "(c4,ct4) <- dirtSetters getNow")
  (tidal-send-string "(c5,ct5) <- dirtSetters getNow")
  (tidal-send-string "(c6,ct6) <- dirtSetters getNow")
  (tidal-send-string "(c7,ct7) <- dirtSetters getNow")
  (tidal-send-string "(c8,ct8) <- dirtSetters getNow")
  (tidal-send-string "(c9,ct9) <- dirtSetters getNow")
  (tidal-send-string "(c10,ct10) <- dirtSetters getNow")
  (tidal-send-string "let bps x = cps (x/2)")
  (tidal-send-string "let hush = mapM_ ($ silence) [c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,d1,d2,d3,d4,d5,d6,d7,d8,d9,d10]")
  (tidal-send-string "let solo = (>>) hush")
  (tidal-send-string ":set prompt \"tidal> \"")
)

(defun tidal-see-output ()
  "Show haskell output."
  (interactive)
  (when (comint-check-proc tidal-buffer)
    (delete-other-windows)
    (split-window-vertically)
    (with-current-buffer tidal-buffer
      (let ((window (display-buffer (current-buffer))))
	(goto-char (point-max))
	(save-selected-window
	  (set-window-point window (point-max)))))))

(defun tidal-quit-haskell ()
  "Quit haskell."
  (interactive)
  (kill-buffer tidal-buffer)
  (delete-other-windows))

(defun tidal-chunk-string (n s)
  "Split a string S into chunks of N characters."
  (let* ((l (length s))
         (m (min l n))
         (c (substring s 0 m)))
    (if (<= l n)
        (list c)
      (cons c (tidal-chunk-string n (substring s n))))))

(defun tidal-send-string (s)
  (if (comint-check-proc tidal-buffer)
      (let ((cs (tidal-chunk-string 64 (concat s "\n"))))
        (mapcar (lambda (c) (comint-send-string tidal-buffer c)) cs))
    (error "no tidal process running?")))

(defun tidal-transform-and-store (f s)
  "Transform example text into compilable form."
  (with-temp-file f
    (mapc (lambda (module)
	    (insert (concat module "\n")))
	  tidal-modules)
    (insert "main = do\n")
    (insert (if tidal-literate-p (tidal-unlit s) s))))


(defun tidal-get-now ()
  "Store the current cycle position in a variable called 'now'."
  (interactive)
  (tidal-send-string "now' <- getNow")
  (tidal-send-string "let now = nextSam now'")
  (tidal-send-string "let retrig = (now `rotR`)")
  (tidal-send-string "let fadeOut n = spread' (_degradeBy) (retrig $ slow n $ envL)")
  (tidal-send-string "let fadeIn n = spread' (_degradeBy) (retrig $ slow n $ (1-) <$> envL)")

  )

(defun tidal-run-line ()
  "Send the current line to the interpreter."
  (interactive)
  (tidal-get-now)
  (let* ((s (buffer-substring (line-beginning-position)
			      (line-end-position)))
	 (s* (if tidal-literate-p
		 (tidal-unlit s)
	       s)))
    (tidal-send-string s*))
  (pulse-momentary-highlight-one-line (point))
  (forward-line)
  )

(defun tidal-eval-multiple-lines ()
  "Eval the current region in the interpreter as a single line."
  (tidal-get-now)
  (mark-paragraph)
  (let* ((s (buffer-substring-no-properties (region-beginning)
                                            (region-end)))
         (s* (if tidal-literate-p
                 (tidal-unlit s)
               s)))
    (tidal-send-string ":{")
    (tidal-send-string s*)
    (tidal-send-string ":}")
    (mark-paragraph)
    (pulse-momentary-highlight-region (mark) (point))
    )
  )

(defun tidal-run-multiple-lines ()
  "Send the current region to the interpreter as a single line."
  (interactive)
  (if (>= emacs-major-version 25)
      (save-mark-and-excursion
       (tidal-eval-multiple-lines))
    (save-excursion
     (tidal-eval-multiple-lines))
    )
  )

(defun tidal-run-d1 ()
  "Send the first instance of d1 to the interpreter as a single line."
  (interactive)
  (goto-char 0)
  (search-forward "d1" nil nil 1)
  (tidal-run-multiple-lines)
  )

(defun tidal-run-d2 ()
  "Send the d2 to the interpreter as a single line."
  (interactive)
  (goto-char 0)
  (search-forward "d2" nil nil 1)
  (tidal-run-multiple-lines)
  )

(defun tidal-run-d3 ()
  "Send the d3 to the interpreter as a single line."
  (interactive)
  (goto-char 0)
  (search-forward "d3" nil nil 1)
  (tidal-run-multiple-lines)
  )

(defun tidal-run-d4 ()
  "Send the d4 to the interpreter as a single line."
  (interactive)
  (goto-char 0)
  (search-forward "d4" nil nil 1)
  (tidal-run-multiple-lines)
  )
(defun tidal-run-d5 ()
  "Send the d5 to the interpreter as a single line."
  (interactive)
  (goto-char 0)
  (search-forward "d5" nil nil 1)
  (tidal-run-multiple-lines)
  )
(defun tidal-run-d6 ()
  "Send the d6 to the interpreter as a single line."
  (interactive)
  (goto-char 0)
  (search-forward "d6" nil nil 1)
  (tidal-run-multiple-lines)
  )
(defun tidal-run-d7 ()
  "Send the d7 to the interpreter as a single line."
  (interactive)
  (goto-char 0)
  (search-forward "d7" nil nil 1)
  (tidal-run-multiple-lines)
  )
(defun tidal-run-d8 ()
  "Send the d9 to the interpreter as a single line."
  (interactive)
  (goto-char 0)
  (search-forward "d8" nil nil 1)
  (tidal-run-multiple-lines)
  )
(defun tidal-run-d9 ()
  "Send the d9 to the interpreter as a single line."
  (interactive)
  (goto-char 0)
  (search-forward "d9" nil nil 1)
  (tidal-run-multiple-lines)
  )


(defun tidal-stop-d1 ()
  "send d1 $ silence as a single line"
  (interactive)
  (tidal-send-string ":{")
  (tidal-send-string " mapM_ ($ silence) [d1]")
  (tidal-send-string ":}")
  )

(defun tidal-stop-d2 ()
  "send d1 $ silence as a single line"
  (interactive)
  (tidal-send-string ":{")
  (tidal-send-string " mapM_ ($ silence) [d2]")
  (tidal-send-string ":}")
  )
(defun tidal-stop-d3 ()
  "send d1 $ silence as a single line"
  (interactive)
  (tidal-send-string ":{")
  (tidal-send-string " mapM_ ($ silence) [d3]")
  (tidal-send-string ":}")
  )


(defun tidal-stop-d4 ()
  "send d1 $ silence as a single line"
  (interactive)
  (tidal-send-string ":{")
  (tidal-send-string " mapM_ ($ silence) [d4]")
  (tidal-send-string ":}")
  )

(defun tidal-stop-d5 ()
  "send d1 $ silence as a single line"
  (interactive)
  (tidal-send-string ":{")
  (tidal-send-string " mapM_ ($ silence) [d5]")
  (tidal-send-string ":}")
  )
(defun tidal-stop-d6 ()
  "send d1 $ silence as a single line"
  (interactive)
  (tidal-send-string ":{")
  (tidal-send-string " mapM_ ($ silence) [d6]")
  (tidal-send-string ":}")
  )

(defun tidal-stop-d7 ()
  "send d1 $ silence as a single line"
  (interactive)
  (tidal-send-string ":{")
  (tidal-send-string " mapM_ ($ silence) [d7]")
  (tidal-send-string ":}")
  )

(defun tidal-stop-d8 ()
  "send d1 $ silence as a single line"
  (interactive)
  (tidal-send-string ":{")
  (tidal-send-string " mapM_ ($ silence) [d8]")
  (tidal-send-string ":}")
  )
(defun tidal-stop-d9 ()
  "send d1 $ silence as a single line"
  (interactive)
  (tidal-send-string ":{")
  (tidal-send-string " mapM_ ($ silence) [d9]")
  (tidal-send-string ":}")
  )

(defun tidal-run-region ()
  "Place the region in a do block and compile."
  (interactive)
  (tidal-transform-and-store
   "/tmp/tidal.hs"
   (buffer-substring-no-properties (region-beginning) (region-end)))
  (tidal-send-string ":load \"/tmp/tidal.hs\"")
  (tidal-send-string "main"))

(defun tidal-load-buffer ()
  "Load the current buffer."
  (interactive)
  (save-buffer)
  (tidal-send-string (format ":load \"%s\"" buffer-file-name)))

(defun tidal-run-main ()
  "Run current main."
  (interactive)
  (tidal-send-string "main"))

(defun tidal-interrupt-haskell ()
  (interactive)
  (if (comint-check-proc tidal-buffer)
      (with-current-buffer tidal-buffer
	(interrupt-process (get-buffer-process (current-buffer))))
    (error "no tidal process running?")))

(defvar tidal-mode-map nil
  "Tidal keymap.")

(defun tidal-mode-keybindings (map)
  "Haskell Tidal keybindings."
  (define-key map [?\C-c ?\C-s] 'tidal-start-haskell)
  (define-key map [?\C-c ?\C-v] 'tidal-see-output)
  (define-key map [?\C-c ?\C-q] 'tidal-quit-haskell)
  (define-key map [?\C-c ?\C-c] 'tidal-run-line)
  (define-key map [?\C-c ?\C-e] 'tidal-run-multiple-lines)
  (define-key map (kbd "<C-return>") 'tidal-run-multiple-lines)
  (define-key map [?\C-c ?\C-r] 'tidal-run-region)
  (define-key map [?\C-c ?\C-l] 'tidal-load-buffer)
  (define-key map [?\C-c ?\C-i] 'tidal-interrupt-haskell)
  (define-key map [?\C-c ?\C-m] 'tidal-run-main)
  (define-key map [?\C-c ?\C-1] 'tidal-run-d1)
  (define-key map [?\C-c ?\C-2] 'tidal-run-d2)
  (define-key map [?\C-c ?\C-3] 'tidal-run-d3)
  (define-key map [?\C-c ?\C-4] 'tidal-run-d4)
  (define-key map [?\C-c ?\C-5] 'tidal-run-d5)
  (define-key map [?\C-c ?\C-6] 'tidal-run-d6)
  (define-key map [?\C-c ?\C-7] 'tidal-run-d7)
  (define-key map [?\C-c ?\C-8] 'tidal-run-d8)
  (define-key map [?\C-c ?\C-9] 'tidal-run-d9)
  (define-key map [?\C-v ?\C-1] 'tidal-stop-d1)
  (define-key map [?\C-v ?\C-2] 'tidal-stop-d2)
  (define-key map [?\C-v ?\C-3] 'tidal-stop-d3)
  (define-key map [?\C-v ?\C-4] 'tidal-stop-d4)
  (define-key map [?\C-v ?\C-5] 'tidal-stop-d5)
  (define-key map [?\C-v ?\C-6] 'tidal-stop-d6)
  (define-key map [?\C-v ?\C-7] 'tidal-stop-d7)
  (define-key map [?\C-v ?\C-8] 'tidal-stop-d8)
  (define-key map [?\C-v ?\C-9] 'tidal-stop-d9))

(defun turn-on-tidal-keybindings ()
  "Haskell Tidal keybindings in the local map."
  (local-set-key [?\C-c ?\C-s] 'tidal-start-haskell)
  (local-set-key [?\C-c ?\C-v] 'tidal-see-output)
  (local-set-key [?\C-c ?\C-q] 'tidal-quit-haskell)
  (local-set-key [?\C-c ?\C-c] 'tidal-run-line)
  (local-set-key [?\C-c ?\C-e] 'tidal-run-multiple-lines)
  (local-set-key (kbd "<C-return>") 'tidal-run-multiple-lines)
  (local-set-key [?\C-c ?\C-r] 'tidal-run-region)
  (local-set-key [?\C-c ?\C-l] 'tidal-load-buffer)
  (local-set-key [?\C-c ?\C-i] 'tidal-interrupt-haskell)
  (local-set-key [?\C-c ?\C-m] 'tidal-run-main)
  (local-set-key [?\C-c ?\C-1] 'tidal-run-d1)
  (local-set-key [?\C-c ?\C-2] 'tidal-run-d2)
  (local-set-key [?\C-c ?\C-3] 'tidal-run-d3)
  (local-set-key [?\C-c ?\C-4] 'tidal-run-d4)
  (local-set-key [?\C-c ?\C-5] 'tidal-run-d5)
  (local-set-key [?\C-c ?\C-6] 'tidal-run-d6)
  (local-set-key [?\C-c ?\C-7] 'tidal-run-d7)
  (local-set-key [?\C-c ?\C-8] 'tidal-run-d8)
  (local-set-key [?\C-c ?\C-9] 'tidal-run-d9)
  (local-set-key [?\C-v ?\C-1] 'tidal-stop-d1)
  (local-set-key [?\C-v ?\C-2] 'tidal-stop-d2)
  (local-set-key [?\C-v ?\C-3] 'tidal-stop-d3)
  (local-set-key [?\C-v ?\C-4] 'tidal-stop-d4)
  (local-set-key [?\C-v ?\C-5] 'tidal-stop-d5)
  (local-set-key [?\C-v ?\C-6] 'tidal-stop-d6)
  (local-set-key [?\C-v ?\C-7] 'tidal-stop-d7)
  (local-set-key [?\C-v ?\C-8] 'tidal-stop-d8)
  (local-set-key [?\C-v ?\C-9] 'tidal-stop-d9))

(defun tidal-mode-menu (map)
  "Haskell Tidal menu."
  (define-key map [menu-bar tidal]
    (cons "Haskell-Tidal" (make-sparse-keymap "Haskell-Tidal")))
  (define-key map [menu-bar tidal help]
    (cons "Help" (make-sparse-keymap "Help")))
  (define-key map [menu-bar tidal expression]
    (cons "Expression" (make-sparse-keymap "Expression")))
  (define-key map [menu-bar tidal expression load-buffer]
    '("Load buffer" . tidal-load-buffer))
  (define-key map [menu-bar tidal expression run-main]
    '("Run main" . tidal-run-main))
  (define-key map [menu-bar tidal expression run-region]
    '("Run region" . tidal-run-region))
  (define-key map [menu-bar tidal expression run-multiple-lines]
    '("Run multiple lines" . tidal-run-multiple-lines))
  (define-key map [menu-bar tidal expression run-line]
    '("Run line" . tidal-run-line))
  (define-key map [menu-bar tidal haskell]
    (cons "Haskell" (make-sparse-keymap "Haskell")))
  (define-key map [menu-bar tidal haskell quit-haskell]
    '("Quit haskell" . tidal-quit-haskell))
  (define-key map [menu-bar tidal haskell see-output]
    '("See output" . tidal-see-output))
  (define-key map [menu-bar tidal haskell start-haskell]
    '("Start haskell" . tidal-start-haskell)))

(unless tidal-mode-map
  (let ((map (make-sparse-keymap "Haskell-Tidal")))
    (tidal-mode-keybindings map)
    (tidal-mode-menu map)
    (setq tidal-mode-map map)))

;;;###autoload
(define-derived-mode
  literate-tidal-mode
  tidal-mode
  "Literate Haskell Tidal"
  "Major mode for interacting with an inferior haskell process."
  (set (make-local-variable 'paragraph-start) "\f\\|[ \t]*$")
  (set (make-local-variable 'paragraph-separate) "[ \t\f]*$")
  (setq tidal-literate-p t)
  (setq haskell-literate 'bird)
  (turn-on-font-lock))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ltidal$" . literate-tidal-mode))
;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/haskell-mode/") ;required by olig1905 on linux
;;(require 'haskell-mode) ;required by olig1905 on linux

;;;###autoload
(define-derived-mode
  tidal-mode
  haskell-mode
  "Haskell Tidal"
  "Major mode for interacting with an inferior haskell process."
  (set (make-local-variable 'paragraph-start) "\f\\|[ \t]*$")
  (set (make-local-variable 'paragraph-separate) "[ \t\f]*$")
  (setq tidal-literate-p nil)
  (turn-on-font-lock))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tidal$" . tidal-mode))

(provide 'tidal)
;;; tidal.el ends here
