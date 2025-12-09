;;; emms-mpv.el --- Support for multiple instances of mpv for EMMS  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.
;; Copyright (C) 2025 Alex Kost

;; Authors: Mike Kazantsev <mk.fraggod@gmail.com>
;;          Alex Kost <alezost@gmail.com>
;; Version: 0.1
;; Keywords: emms

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides EMMS backend for multiple instances of mpv player.
;; It originates from "emms-player-mpv.el" from EMMS source code.  The
;; main difference is that the original "emms-player-mpv.el" file
;; supports only one mpv instance, while this "emms-mpv.el" file will
;; start a new mpv process for every new EMMS playlist you create.  So
;; every time, you call "M-x emms-playlist-new", add some tracks there,
;; and play them, a new mpv player will be started.  You can control
;; each mpv player from the EMMS playlist it was started from.  When you
;; call global commands (like `emms-next', `emms-stop', etc.) outside an
;; EMMS playlist, then mpv player from the lastly visited playlist will
;; be controlled.
;;
;; Also this file handles pause/unpause events i.e., when you pause mpv
;; player itself, EMMS get a signal that the player is paused
;; ("emms-player-mpv.el" doesn't do it).  So your mode line will be
;; updated correctly for (un)pausing (if you use `emms-state-mode' or
;; `emms-playing-time-mode'+`emms-mode-line-mode' or something similar).

;; Technical details:
;;
;; EMMS itself doesn't actually support using multiple player processes.
;; To make it possible, the following hacks are used:
;;
;; - When a new playlist is created, we also create a new mpv IPC
;;   buffer, and these buffers are "linked" by cross-setting buffer-local
;;   `emms-playlist-buffer' and `emms-mpv-ipc-buffer' variables.
;;   This is done by advising `emms-playlist-new' with
;;   `emms-mpv-playlist-new-current' function.
;;
;; - Each EMMS playlist has the following buffer-local variables:
;;   `emms-player-playing-p', `emms-player-stopped-p',
;;   `emms-player-paused-p', and `emms-playing-time'.  This allows EMMS
;;   playlists to keep track only of the linked mpv process state.  Global
;;   values of these variables are updated every time an EMMS buffer is
;;   selected.  This is done by `emms-mpv-update-current-playlist'
;;   function added to `buffer-list-update-hook'.
;;
;; - Each IPC buffer has some internal buffer-local variables to control
;;   mpv process using JSON IPC protocol (see mpv manual), mainly:
;;   `emms-mpv-proc', `emms-mpv-ipc-proc', `emms-mpv-ipc-socket'.

;;; Code:

(require 'emms)
(require 'emms-player-simple)
(require 'emms-playing-time)
(require 'json)
(require 'seq)
(require 'cl-lib)

(defgroup emms-mpv nil
  "EMMS player for mpv."
  :group 'emms-player
  :prefix "emms-mpv-")

(defcustom emms-mpv
  (emms-player
   #'emms-mpv-start
   #'emms-mpv-stop
   #'emms-mpv-playable-p)
  "Parameters for mpv player."
  :type '(cons symbol alist))

(defcustom emms-mpv-command-name "mpv"
  "mpv binary to use. Can be absolute path or just binary name."
  :type 'file)

(defcustom emms-mpv-parameters
  '("--terminal=no")
  "Extra command-line arguments for started mpv process(es).
Either a list of strings or function returning such list.
Extra arguments --idle and --input-ipc-server are added automatically.
Note that unless --no-config option is specified here,
mpv will also use options from its configuration files.
For mpv binary path, see `emms-mpv-command-name'."
  :type '(choice (repeat :tag "List of mpv arguments" string)
                 function))

(defcustom emms-mpv-environment nil
  "List of extra environment variables (\"VAR=value\" strings) to pass on to
mpv process.

These are added on top of `process-environment' by default.
Adding nil as an element to this list will discard emacs
`process-environment' and only pass variables that are specified
in the list."
  :type '(repeat (choice string
                         (const :tag "Start from blank environment" nil))))

(defvar emms-mpv-proc-kill-delay 5
  "Delay until SIGKILL gets sent to `emms-mpv-proc',
if it refuses to exit cleanly on `emms-mpv-proc-stop'.")

(defvar emms-mpv-ipc-connect-delays
  '(0.1 0.1 0.1 0.1 0.1 0.1 0.2 0.2 0.3 0.3 0.5 1.0 1.0 2.0)
  "List of delays before initiating socket connection for new mpv process.")

(defvar emms-mpv-ipc-id 1
  ;; We use global IDs for all mpv processes without resetting
  ;; `emms-mpv-ipc-id' on mpv kill.
  "Auto-incremented counter for unique JSON request identifiers.
Use for for `request_id' and `observe_property' identifiers.
Use `emms-mpv-ipc-id-get' to get and increment this value,
instead of using it directly.
Wraps-around upon reaching `emms-mpv-ipc-id-max'
(unlikely to ever happen).")

(defvar emms-mpv-ipc-id-max (expt 2 30)
  "Max value for `emms-mpv-ipc-id' to wrap around after.
Should be fine with both mpv and Emacs, and probably never reached anyway.")

(defvar emms-mpv-observed-properties '(duration pause metadata)
  "List of properties to observe.
mpv will send \"property-change\" event for each of these properties.")

(defvar emms-mpv-event-connect-functions '(emms-mpv-observe-properties)
  "List of functions to call after establishing JSON IPC connection to mpv.
One argument is passed to each function - IPC process.
Runs before `emms-mpv-ipc-connect-command', if any.
Best place to send any `observe_property', `request_log_messages',
`enable_event' commands.
Use `emms-mpv-ipc-id-get' to get unique id values for these.
See also `emms-mpv-event-functions'.")

(defvar emms-mpv-event-functions nil
  "List of functions to call for each event emitted from JSON IPC.
One argument is passed to each function - JSON line,
as sent by mpv and decoded by `json-read-from-string'.
See also `emms-mpv-event-connect-functions'.")

(defvar emms-mpv-idle-delay 0.5
  "Delay before issuing `emms-mpv-stopped' when mpv unexpectedly goes idle.")


;;; Buffer-local variables

;; `emms-mpv-ipc-buffer' should be uniquely set in each EMMS playlist buffer.
;; The rest variables should be set in each IPC buffer.

(defvar-local emms-mpv-ipc-buffer nil
  "Buffer to associate with `emms-mpv-ipc-proc' socket process.")

(defvar-local emms-mpv-ipc-socket nil
  "Unix socket file to use with mpv --input-ipc-socket= option.")

(defvar-local emms-mpv-proc nil
  "Running mpv process, controlled over --input-ipc-server unix socket.")

(defvar-local emms-mpv-ipc-proc nil
  "Unix socket network process connected to running `emms-mpv-proc'
instance.")

(defvar-local emms-mpv-ipc-connect-timer nil
  "Timer for connection attempts to JSON IPC unix socket.")

(defvar-local emms-mpv-ipc-connect-command nil
  "JSON command for `emms-mpv-ipc-sentinel' to run when it connects to mpv.
I.e. last command that either initiated connection or was used while
connecting to mpv.
Set by `emms-mpv-start' and such,
cleared once it gets sent by `emms-mpv-ipc-sentinel'.")

(defvar-local emms-mpv-ipc-req-table nil
  "Auto-initialized hash table of outstanding API req_ids to their handler funcs.")

(defvar-local emms-mpv-ipc-stop-command nil
  "Internal flag to track when stop command starts/finishes before next loadfile.
Set to either nil, t or the playback start function to call on end-file event
after stop command.
This is a workaround for mpv-0.30+ behavior, where \"stop + loadfile\" only
runs \"stop\".")

(defvar-local emms-mpv-stopped nil
  "Non-nil if playback was stopped by call from emms.
Similar to `emms-player-stopped-p', but set for future async events,
to indicate that playback should stop instead of switching to next track.")

(defvar-local emms-mpv-idle-timer nil
  "Timer to delay `emms-mpv-stopped' when mpv unexpectedly goes idle.")


;;; Debug messages

(defvar emms-mpv-debug nil
  "Enable to print sent/received JSON lines and process
start/stop events to *Messages* buffer using `emms-mpv-debug-msg'.")

(defvar emms-mpv-debug-ts-offset nil
  "Timestamp offset for `emms-mpv-debug-msg'.
Set on first use, with intent to both shorten and obfuscate time in logs.")

(defun emms-mpv-debug-trim (s)
  (if (stringp s)
      (replace-regexp-in-string "\\(^[ \t\n\r]+\\|[ \t\n\r]+$\\)" "" s t t)
    s))

(defun emms-mpv-debug-msg (tpl-or-msg &rest tpl-values)
  "Print debug message to *Messages* if `emms-mpv-debug' is non-nil.
Message is only formatted if TPL-VALUES is non-empty.
Strips whitespace from start/end of TPL-OR-MSG and strings in TPL-VALUES."
  (when emms-mpv-debug
    (setq
     tpl-or-msg (emms-mpv-debug-trim tpl-or-msg)
     tpl-values (seq-map #'emms-mpv-debug-trim tpl-values))
    (unless tpl-values
      (setq tpl-or-msg (replace-regexp-in-string "%" "%%" tpl-or-msg t t)))
    (let ((ts (float-time)))
      (unless emms-mpv-debug-ts-offset
        (setq emms-mpv-debug-ts-offset ts))
      (apply #'message
             (concat "emms-mpv %.1f " tpl-or-msg)
             (- ts emms-mpv-debug-ts-offset)
             tpl-values))))


;;; mpv process

(defun emms-mpv-proc-playing-p (proc)
  "Return `mpv-playing' state for PROC."
  (and proc
       (process-get proc 'mpv-playing)))

(defun emms-mpv-proc-playing (proc state)
  "Set process `mpv-playing' state flag for `emms-mpv-proc-playing-p'."
  (when proc
    (process-put proc 'mpv-playing state)))

(defun emms-mpv-proc-symbol-id (proc sym)
  "Get unique id for SYM or nil if it was already requested."
  (let ((sym-id (intern (concat "mpv-sym-" (symbol-name sym)))))
    (unless (process-get proc sym-id)
      (let ((id (emms-mpv-ipc-id-get)))
        (process-put proc sym-id id)
        id))))

(defun emms-mpv-proc-sentinel (proc event)
  (let ((status (process-status proc))
        (playing (emms-mpv-proc-playing-p proc)))
    (emms-mpv-debug-msg
     "proc[%s]: %s (status=%s, playing=%s)" proc event status playing)
    (when (and (memq status '(exit signal))
               playing)
      (let* ((ipc-buf (with-current-buffer (process-buffer proc)
                        emms-mpv-ipc-buffer))
             (pl-buf (with-current-buffer ipc-buf
                       emms-playlist-buffer)))
        (with-current-buffer pl-buf
          (emms-mpv-stopped)
          (emms-mpv-update-global-state-maybe
           pl-buf 'stop))))))

(defun emms-mpv-proc-init (ipc-buf &rest media-args)
  "initialize new mpv process as `emms-mpv-proc'.
MEDIA-ARGS are used instead of --idle, if specified."
  (with-current-buffer ipc-buf
    (emms-mpv-proc-stop emms-mpv-proc)
    (unless (file-directory-p (file-name-directory emms-mpv-ipc-socket))
      (make-directory (file-name-directory emms-mpv-ipc-socket)))
    (let* ((argv emms-mpv-parameters)
           (argv (append
                  (list emms-mpv-command-name)
                  (if (functionp argv)
                      (funcall argv)
                    argv)
                  (list (format "--input-ipc-server=%s" emms-mpv-ipc-socket))
                  (or media-args '("--idle"))))
           (env emms-mpv-environment)
           (process-environment (append
                                 (unless (seq-some #'not env)
                                   process-environment)
                                 (seq-filter #'identity env)))
           (buffer (generate-new-buffer " *emms-mpv*")))
      (setq emms-mpv-proc
            (make-process :name "emms-mpv"
                          :buffer buffer
                          :command argv
                          :noquery t
                          :sentinel #'emms-mpv-proc-sentinel))
      (with-current-buffer buffer
        (setq emms-mpv-ipc-buffer ipc-buf))
      (emms-mpv-debug-msg "proc[%s]: start %s" emms-mpv-proc argv))))

(defun emms-mpv-proc-stop (proc)
  "Stop running PROC instance via SIGINT.
`delete-process' (SIGKILL) timer is started if
`emms-mpv-proc-kill-delay' is non-nil."
  (when proc
    (emms-mpv-debug-msg "proc[%s]: stop" proc)
    (if (not (process-live-p proc))
        (delete-process proc)
      (emms-mpv-proc-playing proc nil)
      (interrupt-process proc)
      (when emms-mpv-proc-kill-delay
        (run-at-time emms-mpv-proc-kill-delay nil
                     #'delete-process proc)))))


;;; IPC unix socket

(defun emms-mpv-ipc-sentinel (proc event)
  (emms-mpv-debug-msg "ipc[%s]: %s" proc event)
  (when (memq (process-status proc)
              '(open run))
    (with-current-buffer (process-buffer proc)
      (run-hook-with-args 'emms-mpv-event-connect-functions proc)
      (when emms-mpv-ipc-connect-command
        (let ((cmd emms-mpv-ipc-connect-command))
          (setq emms-mpv-ipc-connect-command nil)
          (emms-mpv-ipc-req-send proc cmd))))))

(defun emms-mpv-ipc-filter (proc s)
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((moving (= (point)
                         (process-mark proc))))
          (save-excursion
            (goto-char (process-mark proc))
            (insert s)
            (set-marker (process-mark proc)
                        (point)))
          (if moving (goto-char (process-mark proc))))
        ;; Process/remove all complete lines of json, if any
        (let ((p0 (point-min)))
          (while
              (progn
                (goto-char p0)
                (end-of-line)
                (equal (following-char)
                       ?\n))
            (let* ((p1 (point))
                   (json (buffer-substring p0 p1)))
              (delete-region p0 (+ p1 1))
              (emms-mpv-ipc-recv buf json))))))))

(defun emms-mpv-ipc-connect (buffer delays)
  "Make IPC connection process in BUFFER
If connection attempt fails, reschedule it by (car DELAYS).
Pass (cdr DELAYS) to the next connection attempt, so it can be
rescheduled further until function runs out of DELAYS values.
Set `emms-mpv-ipc-proc' local to BUFFER value to resulting process."
  (emms-mpv-debug-msg "ipc: connect-delay %s" (car delays))
  (with-current-buffer buffer
    (setq-local
     emms-mpv-ipc-proc
     (condition-case nil
         (make-network-process
          :name "emms-mpv-ipc"
          :family 'local
          :service emms-mpv-ipc-socket
          :nowait t
          :coding '(utf-8 . utf-8)
          :buffer buffer
          :noquery t
          :filter #'emms-mpv-ipc-filter
          :sentinel #'emms-mpv-ipc-sentinel)
       (file-error nil)))
    (unless (process-live-p emms-mpv-ipc-proc)
      (setq emms-mpv-ipc-proc nil))
    (when (and (null emms-mpv-ipc-proc)
               delays)
      (run-at-time (car delays)
                   nil
                   #'emms-mpv-ipc-connect
                   buffer (cdr delays)))))

(defun emms-mpv-ipc-init (ipc-buf)
  "Initialize new mpv ipc socket/file process and associated state."
  (with-current-buffer ipc-buf
    (emms-mpv-ipc-stop emms-mpv-ipc-proc)
    (emms-mpv-debug-msg "ipc: init")
    (erase-buffer)
    (when emms-mpv-ipc-connect-timer
      (cancel-timer emms-mpv-ipc-connect-timer))
    (setq
     emms-mpv-ipc-proc nil
     emms-mpv-ipc-req-table (make-hash-table)
     emms-mpv-ipc-connect-timer
     (run-at-time (car emms-mpv-ipc-connect-delays)
                  nil
                  #'emms-mpv-ipc-connect
                  ipc-buf (cdr emms-mpv-ipc-connect-delays)))))

(defun emms-mpv-ipc-stop (proc)
  (when (process-live-p proc)
    (emms-mpv-debug-msg "ipc: stop")
    (delete-process proc)))

(defun emms-mpv-ipc (ipc-buf)
  "Return open IPC process or nil, (re-)starting mpv/connection if necessary.

Return nil when starting async process/connection, and any
follow-up command should be stored to
`emms-mpv-ipc-connect-command' in this case."
  (with-current-buffer ipc-buf
    (unless (process-live-p emms-mpv-proc)
      (emms-mpv-proc-init ipc-buf))
    (unless (process-live-p emms-mpv-ipc-proc)
      (emms-mpv-ipc-init ipc-buf))
    (and emms-mpv-ipc-proc
         (memq (process-status emms-mpv-ipc-proc)
               '(open run))
         emms-mpv-ipc-proc)))


;;; IPC protocol

(defun emms-mpv-ipc-id-get ()
  "Get new connection-unique id value, tracked via `emms-mpv-ipc-id'."
  (let ((ipc-id emms-mpv-ipc-id))
    (setq emms-mpv-ipc-id
          (if (< emms-mpv-ipc-id emms-mpv-ipc-id-max)
              (1+ emms-mpv-ipc-id)
            1))
    ipc-id))

(defun emms-mpv-ipc-req-send (proc cmd &optional handler)
  "Send JSON IPC request to PROC and assign HANDLER to its response.

CMD value is encoded via `json-encode'.

HANDLER will be called with decoded response JSON as (handler data err),
where ERR will be either nil on \"success\", \"connection-error\" or
whatever is in JSON.

If HANDLER is nil, `emms-mpv-ipc-req-error-printer' will be used to log
errors.  Multiple commands can be batched in one list as
\\='(batch (cmd1 . handler1) ...).  In this case, HANDLER argument is
ignored."
  (with-current-buffer (process-buffer proc)
    (dolist (cmd-and-handler
             (if (and (listp cmd)
                      (eq (car cmd) 'batch))
                 (cdr cmd)
               `((,cmd . ,handler))))
      (cl-destructuring-bind (cmd . handler)
          cmd-and-handler
        (let* ((req-id (emms-mpv-ipc-id-get))
              (handler (or handler #'emms-mpv-ipc-req-error-printer))
              (json (concat (json-encode (list :command cmd
                                               :request_id req-id))
                            "\n")))
          (emms-mpv-debug-msg "json >> %s" json)
          (condition-case _err
              ;; On any disconnect, assume that mpv process is to blame
              ;; and force restart.
              (process-send-string proc json)
            (error
             (emms-mpv-proc-stop emms-mpv-proc)
             (setq emms-mpv-proc nil)
             (funcall handler nil 'connection-error)
             (setq handler nil)))
          (when handler (puthash req-id handler emms-mpv-ipc-req-table)))))))

(defun emms-mpv-ipc-req-resolve (req-id data err)
  "Run handler-func for specified req-id."
  (when emms-mpv-ipc-req-table
    (let ((handler (gethash req-id emms-mpv-ipc-req-table))
          (err (if (string= err "success")
                   nil err)))
      (remhash req-id emms-mpv-ipc-req-table)
      (when handler (funcall handler data err)))))

(defun emms-mpv-ipc-req-error-printer (_data err)
  "Simple default `emms-mpv-ipc-req-send' handler to log
errors, if any."
  (when err (message "emms-mpv ipc-error: %s" err)))

(defun emms-mpv-ipc-recv (ipc-buf json)
  "Handler for all JSON lines from mpv process."
  (emms-mpv-debug-msg "json << %s" json)
  (let ((json-data (json-read-from-string json)))
    (with-current-buffer ipc-buf
      (when-let* ((req-id (alist-get 'request_id json-data)))
        ;; Response to command
        (emms-mpv-ipc-req-resolve req-id
                                  (alist-get 'data json-data)
                                  (alist-get 'error json-data)))
      (when-let* ((event (alist-get 'event json-data)))
        ;; mpv event
        (emms-mpv-event-handler json-data)
        (run-hook-with-args 'emms-mpv-event-functions json-data)))))

(defun emms-mpv-observe-property (proc sym)
  "Send mpv observe_property command for property identified by SYM.
Only sends command once per process, removing any
potential duplication if used for same properties from different functions."
  (when-let* ((id (emms-mpv-proc-symbol-id proc sym)))
    (emms-mpv-ipc-req-send proc `(observe_property ,id ,sym))))

(defun emms-mpv-observe-properties (proc)
  "Observe properties from `emms-mpv-observed-properties'."
  (dolist (prop emms-mpv-observed-properties)
    (emms-mpv-observe-property proc prop)))

(defun emms-mpv-event-idle ()
  "Delayed check for switching tracks when mpv goes idle for no good reason."
  (emms-mpv-debug-msg "idle-check (stopped=%s)" emms-mpv-stopped)
  (unless emms-mpv-stopped
    (with-current-buffer emms-playlist-buffer
      (emms-mpv-stopped))))

(defun emms-mpv-event-playing-time-sync ()
  "Request and update `emms-playing-time'."
  (emms-mpv-ipc-req-send
   emms-mpv-ipc-proc
   '(get_property time-pos)
   (let ((pl-buf emms-playlist-buffer))
     (lambda (sec err)
       (unless err
         (with-current-buffer pl-buf
           (setq emms-playing-time sec)
           (emms-mpv-update-global-state-maybe
            pl-buf 'playing-time)))))))


;;; Event handlers

(defun emms-mpv-event-handler (json-data)
  "Handler for supported mpv events, including property changes.

Called before `emms-mpv-event-functions' and does same
thing as these hooks."
  (pcase (alist-get 'event json-data)
    ("playback-restart"
     ;; Separate emms-mpv-proc-playing state is used for emms
     ;; started/stopped signals, because start-file/end-file are also
     ;; emitted after track-change and for playlists, and don't
     ;; correspond to actual playback state.
     (unless (emms-mpv-proc-playing-p emms-mpv-proc)
       (emms-mpv-proc-playing emms-mpv-proc t)
       (with-current-buffer emms-playlist-buffer
         (emms-mpv-started emms-mpv)
         (emms-mpv-update-global-state-maybe
          emms-playlist-buffer 'start)))
     (emms-mpv-event-playing-time-sync))
    ("property-change"
     (emms-mpv-event-property-handler json-data))
    ("end-file"
     (with-current-buffer emms-playlist-buffer
       (emms-mpv-stopped)
       (pcase (alist-get 'reason json-data)
         ("eof"
          (emms-mpv-next-noerror))
         ("stop"
          (emms-mpv-update-global-state-maybe
           emms-playlist-buffer 'stop))))
     (when emms-mpv-ipc-stop-command
       (unless (eq emms-mpv-ipc-stop-command t)
         (funcall emms-mpv-ipc-stop-command))
       (setq emms-mpv-ipc-stop-command nil)))
    ("idle"
     ;; Can mean any kind of error before or during playback.  Example
     ;; can be access/format error, resulting in start+end without
     ;; playback-restart.
     (when emms-mpv-idle-timer
       (cancel-timer emms-mpv-idle-timer))
     (setq
      emms-mpv-idle-timer (run-at-time emms-mpv-idle-delay
                                       nil #'emms-mpv-event-idle)
      emms-mpv-ipc-stop-command nil))
    ("start-file"
     (when emms-mpv-idle-timer
       (cancel-timer emms-mpv-idle-timer)))))

(defun emms-mpv-event-property-handler (json-data)
  "Handler for property change mpv events."
  (pcase (alist-get 'name json-data)
    ("pause"
     ;; json returns either `t' or `:json-false' for pause value.
     (let ((pause (if (eq t (alist-get 'data json-data))
                      t nil)))
       (when pause
         (emms-mpv-event-playing-time-sync))
       (with-current-buffer emms-playlist-buffer
         (setq emms-player-paused-p pause)
         (emms-mpv-update-global-state-maybe
          emms-playlist-buffer 'pause))))
    ("duration"
     (with-current-buffer emms-playlist-buffer
       (let ((duration (alist-get 'data json-data))
             (track (emms-playlist-selected-track)))
         (when (and track (numberp duration) (> duration 0))
           (setq duration (round duration))
           (emms-track-set track 'info-playing-time duration)
           (emms-track-set track 'info-playing-time-min (/ duration 60))
           (emms-track-set track 'info-playing-time-sec (% duration 60))))))
    ("metadata"
     (when-let* ((info-alist (alist-get 'data json-data)))
       (with-current-buffer emms-playlist-buffer
         (emms-mpv-info-meta-update-track
          info-alist (emms-playlist-selected-track)))))))

(defun emms-mpv-info-meta-update-track (info-alist track)
  "Update TRACK with mpv metadata from INFO-ALIST.
`emms-playlist-current-selected-track' is used by default."
  (mapc (lambda (cc)
          (setcar cc (intern (downcase (symbol-name (car cc))))))
        info-alist)
  (cl-macrolet
      ((key (k)
         `(alist-get ',k info-alist))
       (set-track-info (track &rest body)
         (cons 'progn
               (cl-loop
                for (k v)
                on body by 'cddr collect
                `(let ((value ,v))
                   (when value
                     (emms-track-set ,track ',(intern (format "info-%s" k))
                                     value)))))))
    (set-track-info track
                    title (or (key title)
                              (unless (string= "" (key icy-title))
                                (key icy-title))
                              (key icy-name))
                    artist (or (key artist)
                               (key album_artist)
                               (key icy-name))
                    album (key album)
                    tracknumber (key track)
                    year (key date)
                    genre (key genre)
                    note (key comment))
    (emms-track-updated track)))


;;; Hacks to make EMMS work with multiple players+playlists

(defun emms-mpv-started (player)
  "Declare that EMMS PLAYER has started.
This is similar to `emms-player-started', except it does not call hooks."
  (setq emms-player-playing-p player
        emms-player-stopped-p nil
        emms-player-paused-p  nil))

(defun emms-mpv-stopped ()
  "Declare that the current EMMS player is finished.
This is similar to `emms-player-stopped', except it does not call hooks."
  (setq emms-player-playing-p nil
        emms-player-stopped-p t))

(defun emms-mpv-next-noerror ()
  "Start playing the next track in the EMMS playlist.
This is similar to `emms-next-noerror', except it uses
`emms-playlist-selected-track' instead of
`emms-playlist-current-selected-track'."
  (cond
   (emms-repeat-track
    (emms-player-start (emms-playlist-selected-track)))
   (emms-single-track
    (emms-player-stop))
   ((ignore-errors (emms-playlist-select-next) t)
    (emms-player-start (emms-playlist-selected-track)))
   (t
    (message "No next track in playlist"))))

(defun emms-mpv-update-global-state-maybe (buffer &rest keywords)
  "Update global state if BUFFER is the current EMMS playlist."
  (when (eq buffer (default-value 'emms-playlist-buffer))
    (apply #'emms-mpv-update-global-state keywords)))

(defun emms-mpv-update-global-state (&rest keywords)
  "Update some global EMMS variables and run some hooks.

Supported KEYWORDS:

  - `start' (or `stop'): update `emms-player-playing-p' and
    `emms-player-stopped-p' and run one of the following hooks:
    `emms-player-started-hook', `emms-player-stopped-hook',
    `emms-player-finished-hook';

  - `pause': update `emms-player-paused-p' and run
    `emms-player-paused-hook';

  - `playing-time': update `emms-playing-time' and run
    `emms-player-time-set-functions';

  - `track': run `emms-track-updated-functions';

  - `vars': update all the above variables but do not run hooks;

  - `all': update all the variables and run appropriate hooks."
  (emms-mpv-debug-msg "updating global state: %S" keywords)
  (let ((keywords (cond ((memq 'vars keywords)
                         '(vars))
                        ((memq 'all keywords)
                         '(all pause playing-time track))
                        (t keywords))))
    (with-current-buffer (default-value 'emms-playlist-buffer)
      (dolist (keyword keywords)
        (cl-case keyword
          (vars
           (setq-default
            emms-player-playing-p emms-player-playing-p
            emms-player-stopped-p emms-player-stopped-p
            emms-player-paused-p  emms-player-paused-p
            emms-playing-time     emms-playing-time))
          ((all start stop)
           (setq-default
            emms-player-playing-p emms-player-playing-p
            emms-player-stopped-p emms-player-stopped-p)
           (unless (eq keyword 'all)
             (if emms-player-playing-p
                 (run-hooks 'emms-player-started-hook)
               (if emms-player-stopped-p
                   (run-hooks 'emms-player-stopped-hook)
                 (run-hooks 'emms-player-finished-hook)))))
          (pause
           (setq-default emms-player-paused-p emms-player-paused-p)
           (run-hooks 'emms-player-paused-hook))
          (playing-time
           (setq-default emms-playing-time emms-playing-time)
           (run-hook-with-args 'emms-player-time-set-functions
                               emms-playing-time))
          (track
           (run-hook-with-args 'emms-track-updated-functions
                               (emms-playlist-selected-track))))))))

(defun emms-mpv-handle-kill-playlist ()
  "Do some cleaning actions before killing EMMS playlist.
Delete linked mpv and mpv ipc processes and make another EMMS playlist
the current one."
  (when emms-playlist-buffer-p
    (when-let* ((ipc-buf (emms-mpv-current-ipc-buffer)))
      (with-current-buffer ipc-buf
        (emms-mpv-proc-stop emms-mpv-proc)
        (emms-mpv-ipc-stop emms-mpv-ipc-proc)))
    (let ((buffer (current-buffer)))
      (when (eq (default-value 'emms-playlist-buffer)
                buffer)
        (setq-default
         emms-playlist-buffer
         (car (remove buffer emms-playlist-buffers)))))))

(add-hook 'kill-buffer-hook #'emms-mpv-handle-kill-playlist)

(defun emms-mpv-playlist-link (buffer)
  "Link mpv and EMMS playlist BUFFER.
This function should be called only once, when a new EMMS playlist is
created to associate the next mpv process (which does not exist yet)
with it."
  (let ((ipc-buf (generate-new-buffer "*emms-mpv-ipc*")))
    (with-current-buffer buffer
      (setq-local emms-mpv-ipc-buffer   ipc-buf
                  emms-player-playing-p nil
                  emms-player-paused-p  nil
                  emms-player-stopped-p t
                  emms-playing-time     0))
    (with-current-buffer ipc-buf
      (setq-local emms-playlist-buffer buffer
                  emms-mpv-ipc-socket
                  (concat (file-name-as-directory emms-directory)
                          (buffer-name ipc-buf) ".socket")))))

(defun emms-mpv-playlist-new-current (buffer)
  "Make EMMS playlist BUFFER current.
This function should be called only with `emms-playlist-new' output to
make the created playlist current and to attach it to a unique mpv
process that will be started later."
  (emms-mpv-playlist-link buffer)
  (setq-default emms-playlist-buffer buffer)
  (emms-mpv-update-global-state 'vars)
  buffer)

(advice-add 'emms-playlist-new
  :filter-return #'emms-mpv-playlist-new-current)

(defvar emms-mpv-playlist-buffer nil
  "Lastly selected EMMS playlist buffer.
This is internal variable for `emms-mpv-update-current-playlist'.")

(defun emms-mpv-update-current-playlist ()
  "If current buffer is an EMMS playlist, make it the current playlist."
  (when emms-playlist-buffer-p
    (let ((buffer (current-buffer)))
      (unless (eq buffer emms-mpv-playlist-buffer)
        (setq-default emms-playlist-buffer buffer)
        (emms-mpv-update-global-state 'all)
        (setq emms-mpv-playlist-buffer buffer)))))

(add-hook 'buffer-list-update-hook #'emms-mpv-update-current-playlist)

(defun emms-mpv-set-current-playlist (buffer)
  "Make BUFFER-OR-NAME the current EMMS playlist buffer."
  (interactive
   (list (get-buffer
          (completing-read
           "Set current playlist: "
           (mapcar #'buffer-name (emms-playlist-buffer-list))
           nil t))))
  (with-current-buffer buffer
    (emms-mpv-update-current-playlist)))

(defun emms-mpv-current-ipc-buffer ()
  "Return IPC buffer for the current EMMS playlist."
  (if emms-playlist-buffer-p
      emms-mpv-ipc-buffer
    (with-current-emms-playlist emms-mpv-ipc-buffer)))


;;; High-level EMMS interface

(defun emms-mpv-cmd (cmd &optional handler)
  "Send mpv command to process/connection if both are running,
or otherwise schedule start/connect and set
`emms-mpv-ipc-connect-command' for `emms-mpv-ipc-sentinel'.
Multiple commands can be batched in one list as \\='(batch (cmd1 . handler1) ...),
in which case common HANDLER argument is ignored."
  (if-let* ((ipc-buf (emms-mpv-current-ipc-buffer)))
      (with-current-buffer ipc-buf
        (setq emms-mpv-ipc-connect-command nil)
        (if-let* ((proc (emms-mpv-ipc ipc-buf)))
            (emms-mpv-ipc-req-send proc cmd handler)
          (setq emms-mpv-ipc-connect-command cmd)))
    ;; The following case should never happen because all EMMS playlist
    ;; buffers should always contain `emms-mpv-ipc-buffer' local
    ;; variable (as long as `emms-playlist-new' is advised).
    (emms-mpv-debug-msg "ERROR: `emms-mpv-ipc-buffer' not found")
    (emms-mpv-playlist-link emms-playlist-buffer)
    (emms-mpv-cmd cmd handler)))

(defun emms-mpv-playable-p (track)
  (memq (emms-track-type track)
        '(file url streamlist playlist)))

(defun emms-mpv-start (track)
  (with-current-emms-playlist
    (with-current-buffer emms-mpv-ipc-buffer
      (setq emms-mpv-stopped nil)
      (emms-mpv-proc-playing emms-mpv-proc nil)
      (let* ((track-name (emms-track-get track 'name))
             (play-cmd `(batch
                         ((loadfile ,track-name replace))
                         ((set pause no))))
             (start-func
              ;; Try running play-cmd and retry it on connection failure
              ;; e.g., if mpv died.
              (apply-partially 'emms-mpv-cmd play-cmd
                               (lambda (_mpv-data mpv-error)
                                 (when (eq mpv-error 'connection-error)
                                   (emms-mpv-cmd play-cmd))))))
        (funcall start-func)))))

(defun emms-mpv-stop ()
  (with-current-emms-playlist
    (with-current-buffer emms-mpv-ipc-buffer
      (setq
       emms-mpv-stopped t
       emms-mpv-ipc-stop-command t)
      (emms-mpv-proc-playing emms-mpv-proc nil)
      (emms-mpv-cmd `(stop)))
    (emms-mpv-stopped)))

(defun emms-mpv-pause ()
  (emms-mpv-cmd `(set pause yes)))

(defun emms-mpv-resume ()
  (emms-mpv-cmd `(set pause no)))

(defun emms-mpv-seek (sec)
  (emms-mpv-cmd `(seek ,sec relative)))

(defun emms-mpv-seek-to (sec)
  (emms-mpv-cmd `(seek ,sec absolute)))


(emms-player-set emms-mpv 'regex
                 (apply #'emms-player-simple-regexp
                        (cons "oga" emms-player-base-format-list)))
(emms-player-set emms-mpv 'pause   #'emms-mpv-pause)
(emms-player-set emms-mpv 'resume  #'emms-mpv-resume)
(emms-player-set emms-mpv 'seek    #'emms-mpv-seek)
(emms-player-set emms-mpv 'seek-to #'emms-mpv-seek-to)

(provide 'emms-mpv)

;;; emms-mpv.el ends here
