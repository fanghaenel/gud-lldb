;;; gud-lldb.el --- Grand Unified Debugger support for LLDB

;; Copyright (C) 2017-2020 Thomas Fanghaenel

;; Author: Thomas Fanghaenel <thomas.fanghaenel@gmail.com>
;; Keywords: c, tools, unix

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'gud)

;; Regular expression to identify a marker that refers to a thread.  This is
;; what lldb prints out when stopping at a breakpoint or when single-stepping.
;;
;; Example:
;; =============================================================================
;; Process 1234 stopped
;; * thread #1: tid = 0xc480a, 0x0000000100009965 program`functionName + 37 at /full/path/to/source/file.c:169, stop reason = breakpoint 1.1
;;     frame #0: 0x0000000100009965 program`functionName + 37 at /full/path/to/source/file.c:169
;; Target 0: (process-name) stopped.
;; =============================================================================
(defvar gud-lldb-thread-marker-start-regexp
  "Process [0-9]+ stopped\n")
(defvar gud-lldb-thread-marker-full-regexp
  "Process [0-9]+ stopped\n\\* thread.* at \\([^:\n]*\\):\\([0-9]*\\), stop reason = \\(.*\\)\n")
(defvar gud-lldb-tread-marker-finished-regexp
  "^\\(Target [0-9]+: (.+) stopped\\).\n\\'")

;; Regular expression to identify a marker that refers to a stack frame.
;; This is what lldb prints out when going up and down the stack.
;;
;; Example:
;; =============================================================================
;; frame #2: 0x0000000100002bde program`functionName + 2222 at /full/path/to/source/file.c:868
;;==============================================================================
(defvar gud-lldb-frame-marker-start-regexp
  "^frame #[0-9]+:")
(defvar gud-lldb-frame-marker-full-regexp
  "^frame #[0-9]+:.* at \\([^:\n]*\\):\\([0-9]*\\)\n")

(defvar gud-lldb-startup-cmds
  '(
    ;; Frame format: Make sure the output contains the full path to source
    ;; files, not just the file name.
    "settings set frame-format frame #${frame.index}: ${frame.pc}{ ${module.file.basename}{`${function.name}${function.pc-offset}}}{ at ${line.file.fullpath}:${line.number}}\\n"

    ;; Thread stop format: Make sure the output always has source file and
    ;; line number information, even if it may be redundant.  Also, make
    ;; sure we use the full path for source files.
    "settings set thread-stop-format thread #${thread.index}: tid = ${thread.id}{, ${frame.pc}}{ ${module.file.basename}{`${function.name}${function.pc-offset}}}{ at ${line.file.fullpath}:${line.number}}{, stop reason = ${thread.stop-reason}}\\n"

    ;; Suppress the source code snippets whenever execution stops.
    "settings set stop-line-count-before 0"
    "settings set stop-line-count-after 0"
    ))

(defun gud-lldb-massage-args (_file args)
  "Appends to a lldb command line all the bits and pieces that we
need in order to get the output formatted in a way that we can
reliably find the markers.  See `gud-lldb-marker-filter' for more
details on the expected appearance of markers.

The additional startup options are in `gud-lldb-startup-cmds' in
the form of normal lldb commands (usually \"settings set ...\"),
and they get appended as a list of \"-O\" command line options."
  (nconc (let ((cmds gud-lldb-startup-cmds)
	       (result nil))
	   (while cmds
	     (setq result (cons (car cmds) (cons "-O" result)))
	     (setq cmds (cdr cmds)))
	   (nreverse result))
	 args))

(defvar gud-marker-acc "")
(make-variable-buffer-local 'gud-marker-acc)

(defvar gud-output-acc "")
(make-variable-buffer-local 'gud-output-acc)

(defvar gud-maybe-swallow-output nil)
(make-variable-buffer-local 'gud-maybe-swallow-output)

(defvar gud-empty-output-block t)
(make-variable-buffer-local 'gud-empty-output-block)

(defun gud-lldb-flush-output-acc ()
  "Flushes the contents of the output accumulator once the end of
a block of output is reached, i.e. once the next line starting
with a prompt is encountered."

  (let ((output ""))

    ;; If we have output buffered up, check how to handle this.
    (if gud-maybe-swallow-output
        (progn
          (cond

           ;; If we've seen a thread marker at the beginning of the buffered
           ;; output, we extract out of that line whatever we may find useful
           ;; later on.  This is the file and line number of the stop location,
           ;; and the reason for stopping.
           ;;
           ;; After that, we're looking for a line like "Target 1234: stopped"
           ;; at the end of the output section.  This is the format that lldb
           ;; uses when single-stepping, when hitting a breakpoint, or when
           ;; getting an interrupt.  Unless we see that line, we won't bother
           ;; processing the output.
           ((string-match gud-lldb-thread-marker-full-regexp gud-output-acc)

            (let ((file (match-string 1 gud-output-acc))
                  (line (string-to-number (match-string 2 gud-output-acc)))
                  (reason (match-string 3 gud-output-acc)))

              (if (string-match gud-lldb-tread-marker-finished-regexp gud-output-acc)
                  ;; We've seen the final line that says "Target 1234: stopped".
                  ;; Now let's check if we can figure out what the reason for
                  ;; stopping was.
                  ;;
                  ;; If the reason for stopping is single-stepping, we swallow
                  ;; the output.  For any other reason (like breakpoint, signal,
                  ;; etc.) we will still print out the thread and frame info.
                  (progn
                    (setq gud-last-frame (cons file line))
                    (if (not (string-match "\\`step" reason))
                        (setq gud-output-acc (replace-regexp-in-string
                                              gud-lldb-tread-marker-finished-regexp
                                              (concat "\\1 - " reason ".")
                                              gud-output-acc)
                              output (concat output gud-output-acc))))
                ;; If we don't see that final "Target 1234: stopped" line, this
                ;; chunk may be output from a different lldb command (like "bt",
                ;; for instance).  In that case, we just print the entire
                ;; buffered-up output.
                (setq output (concat output gud-output-acc)))))

           ;; If we're seeing a frame marker with no spaces at the beginning of
           ;; the line, this is likely output produced by navigating up and down
           ;; the call stack.  We just extract the source file information, and
           ;; still show the output.
           ((string-match (concat gud-lldb-frame-marker-full-regexp "\\'") gud-output-acc)
            (let ((file (match-string 1 gud-output-acc))
                  (line (string-to-number (match-string 2 gud-output-acc))))
              (setq gud-last-frame (cons file line)
                    output (concat output gud-output-acc))))

           ;; We shouldn't really come here, because output buffering is only
           ;; triggered when we see one of the markers.  But in any event, the
           ;; safe thing to do is to just print the buffered-up output, and
           ;; continue.
           (t
            (setq output (concat output gud-output-acc))))

          ;; If a non-empty block of output doesn't end with a newline, then add
          ;; one.
          (if (and (not gud-empty-output-block) (not (string-match "\n\\'" output)))
              (setq output (concat output "\n")))

          ;; Reset the output accumulator, and start the next block without
          ;; output buffering.
          (setq gud-output-acc ""
                gud-maybe-swallow-output nil
                gud-empty-output-block t)))

    output))

(defun gud-lldb-process-next-line (line)
  "Processes the next line of output.  The line of output is
passed in `line', and contains everything up to the next line
break."

  (let ((output ""))

    ;; If this line starts with a prompt, finish the previous block of output,
    ;; and consume the prompt from the current line.
    (if (string-match comint-prompt-regexp line)
        (let ((after-prompt (match-end 0)))
          (setq output (concat output
                               (gud-lldb-flush-output-acc)
                               (substring line 0 after-prompt))
                line (substring line after-prompt))))

    ;; See how to handle this new line of output.  We may display it
    ;; immediately, or delay the decision as to whether or not it will be
    ;; displayed until some later point.
    (if gud-maybe-swallow-output

        ;; If we're in output buffering mode, just keep appending to the output
        ;; accumulator.  The accumulated output will be processed once the next
        ;; line that starts with a prompt is encountered.
        (setq gud-output-acc (concat gud-output-acc line))

      ;; If we're not in output buffering mode, we need to check if this line
      ;; requires us to switch into buffering mode.  This happens when we see
      ;; something that looks like the start of a marker.
      (if (or (string-match gud-lldb-thread-marker-start-regexp line)
              (string-match gud-lldb-frame-marker-start-regexp line))

          ;; If we find what looks like a marker, we start accumulating
          ;; output until we reach the end of a block, i.e. we see the
          ;; next prompt.  This is so that we can filter out things that
          ;; are irrelevant (e.g. all the clutter produced while
          ;; single-stepping through the code).
          (setq output (concat output (substring line 0 (match-beginning 0)))
                gud-output-acc (concat (substring line (match-beginning 0)))
                gud-maybe-swallow-output t)

        ;; If there's no marker in the current output, just echo the
        ;; current output immediately.
        (setq output (concat output line)
              gud-empty-output-block nil)))

    output))

(defun gud-lldb-marker-filter (chunk)
  (setq gud-marker-acc (concat gud-marker-acc chunk))
  (let ((output ""))

    ;; Process all complete lines.
    (while (string-match "\n" gud-marker-acc)
      (let ((after-newline (match-end 0)))
        (setq output (concat output
                             (gud-lldb-process-next-line (substring gud-marker-acc
                                                                    0 after-newline)))
              gud-marker-acc (substring gud-marker-acc after-newline))))

    ;; Asynchronous output, e.g. when hitting a breakpoint for a second time
    ;; after having continued before, may just end at a line break, and doesn't
    ;; have a prompt.  If we detect output that ends on a line break, and it
    ;; doesn't have any incomplete lines, we flush the output accumulator and
    ;; insert another prompt.
    (if (string= "" gud-marker-acc)
        (setq output (concat output (gud-lldb-flush-output-acc) "(lldb) ")))

    ;; If the final incomplete line only contains a prompt, or if it looks like
    ;; it's waiting for user input, then finish the previous command's output,
    ;; and display that incomplete line.
    (if (or (string-match (concat comint-prompt-regexp "\\'") gud-marker-acc)
            (string-match ".* \[[YyNn/]+\] \\'" gud-marker-acc))
        (setq output (concat output (gud-lldb-flush-output-acc) gud-marker-acc)
              gud-marker-acc ""))

    output))

(defun gud-lldb-quit ()
  "Quit the current lldb session, detach if necessary, and kill
the associated buffer."

  (let ((gud-lldb-buffer gud-comint-buffer))
    (with-current-buffer gud-lldb-buffer
      (unwind-protect
          (progn
            (gud-call "quit")
            (sit-for 0)
            (gud-basic-call "y")
            (sit-for 0.25))
        (kill-buffer (current-buffer))))))

;;;###autoload
(defun lldb (command-line)
  "Run lldb as invoked with command line COMMAND-LINE in a buffer.

The buffer is named \"*gud*\" if there isn't any executable
specified in the command line, or \"*gud-<executable>*\" if there
is.

The directory where the executable resides becomes the initial
working directory, and also the default search directory for
source files.  We try to not rely on the latter too much though,
by instructing the debugger to always spit out the fully
qualified paths to source files."
  (interactive (list (gud-query-cmdline 'lldb)))

  (gud-common-init command-line 'gud-lldb-massage-args 'gud-lldb-marker-filter)
  (set (make-local-variable 'gud-minor-mode) 'lldb)

  (gud-def gud-bt     "thread backtrace"                   "b"    "Show stack for the current thread.")
  (gud-def gud-bt-all "thread backtrace all"               "B"    "Show stacks for all the threads.")

  (gud-def gud-listb  "breakpoint list"                    "l"    "List all breakpoints.")
  (gud-def gud-break  "breakpoint set -f %f -l %l"         "\C-b" "Set breakpoint at current line.")
  (gud-def gud-tbreak "breakpoint set -o true -f %f -l %l" "\C-t" "Set one-shot breakpoint at current line.")
  (gud-def gud-remove "breakpoint clear -f %f -l %l"       "\C-d" "Remove breakpoint at current line")

  (gud-def gud-step   "thread step-in"                     "\C-s" "Step one source line with display.")
  (gud-def gud-stepi  "thread step-inst"                   "\C-i" "Step one instruction with display.")
  (gud-def gud-next   "thread step-over"                   "\C-n" "Step one line (skip functions).")
  (gud-def gud-nexti  "thread step-inst-over"              nil    "Step one instruction (skip functions).")
  (gud-def gud-cont   "process continue"                   "\C-r" "Continue with display.")
  (gud-def gud-finish "thread step-out"                    "\C-f" "Finish executing current function.")

  (gud-def gud-up     "frame select -r 1"                  "<"    "Up 1 stack frame.")
  (gud-def gud-down   "frame select -r -1"                 ">"    "Down 1 stack frame.")
  (gud-def gud-print  "expression -- %e"                   "\C-p" "Evaluate C expression at point.")
  (gud-def gud-pstar  "expression -- *%e"                  "p"    "Evaluate C dereferenced pointer expression at point.")

  (gud-def gud-run    "process launch"                     "r"    "Run the program.")
  (gud-def gud-kill   "process kill"                       "k"    "Kill the program.")

  (gud-def gud-quit   (gud-lldb-quit)                      "Q"    "Quit the debugger")

  (setq comint-prompt-regexp  "^(lldb) *"
        paragraph-start comint-prompt-regexp)

  (run-hooks 'lldb-mode-hook))

(provide 'gud-lldb)
