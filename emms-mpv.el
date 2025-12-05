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
;; only difference is that the original "emms-player-mpv.el" file
;; supports only one mpv instance, while this "emms-mpv.el" file will
;; start a new mpv process for every new EMMS playlist you create.
;;
;; Also this file handles pause/unpause events i.e., when you pause mpv
;; player itself, EMMS get a signal that the player is paused
;; ("emms-player-mpv.el" doesn't do it).

;;; Code:

(require 'emms)
(require 'emms-player-simple)
(require 'emms-playing-time)
(require 'json)
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
  "*Parameters for mpv player."
  :type '(cons symbol alist))

(defcustom emms-mpv-command-name "mpv"
  "mpv binary to use. Can be absolute path or just binary name."
  :type 'file)

(defcustom emms-mpv-parameters
  '("--quiet" "--really-quiet" "--no-audio-display")
  "Extra command-line arguments for started mpv process(es).
Either a list of strings or function returning such list.
Extra arguments --idle and --input-ipc-server are added automatically.
Note that unless --no-config option is specified here,
mpv will also use options from its configuration files.
For mpv binary path, see `emms-mpv-command-name'."
  :type '(choice (repeat :tag "List of mpv arguments" string)
                 function))

(defcustom emms-mpv-environment ()
  "List of extra environment variables (\"VAR=value\" strings) to pass on to
mpv process.

These are added on top of `process-environment' by default.
Adding nil as an element to this list will discard emacs
`process-environment' and only pass variables that are specified
in the list."
  :type '(repeat (choice string (const :tag "Start from blank environment" nil))))

(defcustom emms-mpv-ipc-socket
  (concat (file-name-as-directory emms-directory)
          "mpv-ipc.sock")
  "Unix socket path to use with mpv --input-ipc-socket= option."
  :type 'file)

(defvar emms-mpv-ipc-proc nil) ; to avoid warnings while keeping useful defs at the top

(defcustom emms-mpv-update-metadata nil
  "Update track info (artist, album, name, etc) from mpv events, when it
is played.

This allows to dynamically update stream info from ICY tags, for
example.  Uses `emms-mpv-event-connect-hook' and
`emms-mpv-event-functions' hooks."
  :type 'boolean
  :set (lambda (sym value)
         (set-default-toplevel-value sym value)
         (run-at-time 0.1 nil
                      (lambda (value)
                        (if value
                            (progn
                              (add-hook
                               'emms-mpv-event-connect-hook
                               #'emms-mpv-info-meta-connect-func)
                              (add-hook
                               'emms-mpv-event-functions
                               #'emms-mpv-info-meta-event-func)
                              (when (process-live-p emms-mpv-ipc-proc)
                                (emms-mpv-info-meta-connect-func)))
                          (progn
                            (remove-hook
                             'emms-mpv-event-connect-hook
                             #'emms-mpv-info-meta-connect-func)
                            (remove-hook
                             'emms-mpv-event-functions
                             #'emms-mpv-info-meta-event-func))))
                      value)))

(defcustom emms-mpv-use-playlist-option nil
  "Use --playlist option and loadlist mpv command for playlist files and URLs.

Use of this option is explicitly discouraged by mpv documentation for security
reasons, and should be unnecessary in most common cases with modern mpv.
Make sure to check mpv manpage for --playlist option before enabling this."
  :type 'boolean)

(defvar emms-mpv-proc nil
  "Running mpv process, controlled over --input-ipc-server unix socket.")

(defvar emms-mpv-proc-kill-delay 5
  "Delay until SIGKILL gets sent to `emms-mpv-proc',
if it refuses to exit cleanly on `emms-mpv-proc-stop'.")

(defvar emms-mpv-ipc-proc nil
  "Unix socket network process connected to running `emms-mpv-proc'
instance.")

(defvar emms-mpv-ipc-buffer " *emms-mpv-ipc*"
  "Buffer to associate with `emms-mpv-ipc-proc' socket process.")

(defvar emms-mpv-ipc-connect-timer nil
  "Timer for connection attempts to JSON IPC unix socket.")
(defvar emms-mpv-ipc-connect-delays
  '(0.1 0.1 0.1 0.1 0.1 0.1 0.2 0.2 0.3 0.3 0.5 1.0 1.0 2.0)
  "List of delays before initiating socket connection for new mpv process.")

(defvar emms-mpv-ipc-connect-command nil
  "JSON command for `emms-mpv-ipc-sentinel' to run when it connects to mpv.
I.e. last command that either initiated connection or was used while
connecting to mpv.
Set by `emms-mpv-start' and such,
cleared once it gets sent by `emms-mpv-ipc-sentinel'.")

(defvar emms-mpv-ipc-id 1
  "Auto-incremented counter for unique JSON request identifiers.
Use for for `request_id' and `observe_property' identifiers.
Use `emms-mpv-ipc-id-get' to get and increment this value,
instead of using it directly.
Wraps-around upon reaching `emms-mpv-ipc-id-max'
(unlikely to ever happen).")

(defvar emms-mpv-ipc-id-max (expt 2 30)
  "Max value for `emms-mpv-ipc-id' to wrap around after.
Should be fine with both mpv and Emacs, and probably never reached anyway.")

(defvar emms-mpv-ipc-req-table nil
  "Auto-initialized hash table of outstanding API req_ids to their handler funcs.")

(defvar emms-mpv-ipc-stop-command nil
  "Internal flag to track when stop command starts/finishes before next loadfile.
Set to either nil, t or the playback start function to call on end-file event
after stop command.
This is a workaround for mpv-0.30+ behavior, where \"stop + loadfile\" only
runs \"stop\".")

(defvar emms-mpv-observed-properties '(duration pause)
  "List of properties to observe.
mpv will send \"property-change\" event for each of these properties.")

(defvar emms-mpv-event-connect-hook '(emms-mpv-observe-properties)
  "Normal hook run right after establishing new JSON IPC connection to mpv.
Runs before `emms-mpv-ipc-connect-command', if any.
Best place to send any `observe_property', `request_log_messages',
`enable_event' commands.
Use `emms-mpv-ipc-id-get' to get unique id values for these.
See also `emms-mpv-event-functions'.")

(defvar emms-mpv-event-functions nil
  "List of functions to call for each event emitted from JSON IPC.
One argument is passed to each function - JSON line,
as sent by mpv and decoded by `json-read-from-string'.
See also `emms-mpv-event-connect-hook'.")

(defvar emms-mpv-stopped nil
  "Non-nil if playback was stopped by call from emms.
Similar to `emms-player-stopped-p', but set for future async events,
to indicate that playback should stop instead of switching to next track.")

(defvar emms-mpv-idle-timer (timer-create)
  "Timer to delay `emms-player-stopped' when mpv unexpectedly goes idle.")

(defvar emms-mpv-idle-delay 0.5
  "Delay before issuing `emms-player-stopped' when mpv unexpectedly goes idle.")


;;; Helpers

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

(defun emms-mpv-proc-playing-p (&optional proc)
  "Return whether playback in PROC or `emms-mpv-proc' is started,
which is distinct from \"start-command sent\" and \"process is running\" states.
Used to signal emms via `emms-player-started' and `emms-player-stopped' calls."
  (and-let* ((proc (or proc emms-mpv-proc)))
    (process-get proc 'mpv-playing)))

(defun emms-mpv-proc-playing (state &optional proc)
  "Set process mpv-playing state flag for `emms-mpv-proc-playing-p'."
  (when-let* ((proc (or proc emms-mpv-proc)))
    (process-put proc 'mpv-playing state)))

(defun emms-mpv-proc-symbol-id (sym &optional proc)
  "Get unique process-specific id integer for SYM or nil if it
was already requested."
  (let ((proc (or proc emms-mpv-proc))
        (sym-id (intern (concat "mpv-sym-" (symbol-name sym)))))
    (unless (process-get proc sym-id)
      (let ((id (emms-mpv-ipc-id-get)))
        (process-put proc sym-id id)
        id))))

(defun emms-mpv-proc-sentinel (proc ev)
  (let ((status (process-status proc))
        (playing (emms-mpv-proc-playing-p proc)))
    (emms-mpv-debug-msg
     "proc[%s]: %s (status=%s, playing=%s)" proc ev status playing)
    (when (and (memq status '(exit signal))
               playing)
      (emms-player-stopped))))

(defun emms-mpv-proc-init (&rest media-args)
  "initialize new mpv process as `emms-mpv-proc'.
MEDIA-ARGS are used instead of --idle, if specified."
  (emms-mpv-proc-stop)
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
                               (unless (seq-some 'not env)
                                 process-environment)
                               (seq-filter 'identity env))))
    (setq emms-mpv-proc
          (make-process :name "emms-mpv"
                        :buffer nil
                        :command argv
                        :noquery t
                        :sentinel #'emms-mpv-proc-sentinel))
    (emms-mpv-debug-msg "proc[%s]: start %s" emms-mpv-proc argv)))

(defun emms-mpv-proc-stop ()
  "Stop running `emms-mpv-proc' instance via SIGINT, if any.

`delete-process' (SIGKILL) timer is started if
`emms-mpv-proc-kill-delay' is non-nil."
  (when emms-mpv-proc
    (let ((proc emms-mpv-proc))
      (emms-mpv-debug-msg "proc[%s]: stop" proc)
      (if (not (process-live-p proc))
          (delete-process proc)
        (emms-mpv-proc-playing nil proc)
        (interrupt-process proc)
        (when emms-mpv-proc-kill-delay
          (run-at-time emms-mpv-proc-kill-delay nil
                       #'delete-process proc))))
    (setq emms-mpv-proc nil)))


;;; IPC unix socket

(defun emms-mpv-ipc-sentinel (proc ev)
  (emms-mpv-debug-msg "ipc[%s]: %s" proc ev)
  (when (memq (process-status proc)
              '(open run))
    (run-hooks 'emms-mpv-event-connect-hook)
    (when emms-mpv-ipc-connect-command
      (let ((cmd emms-mpv-ipc-connect-command))
        (setq emms-mpv-ipc-connect-command nil)
        (emms-mpv-ipc-req-send cmd nil proc)))))

(defun emms-mpv-ipc-filter (proc s)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
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
            (emms-mpv-ipc-recv json)))))))

(defun emms-mpv-ipc-connect (delays)
  "Make IPC connection attempt, rescheduling if there's no socket by (car DELAYS).
(cdr DELAYS) gets passed to next connection attempt,
so it can be rescheduled further until function runs out of DELAYS values.
Sets `emms-mpv-ipc-proc' value to resulting process on success."
  (emms-mpv-debug-msg "ipc: connect-delay %s" (car delays))
  (setq emms-mpv-ipc-proc
        (condition-case nil
            (make-network-process
             :name "emms-mpv-ipc"
             :family 'local
             :service emms-mpv-ipc-socket
             :nowait t
             :coding '(utf-8 . utf-8)
             :buffer (get-buffer-create emms-mpv-ipc-buffer)
             :noquery t
             :filter #'emms-mpv-ipc-filter
             :sentinel #'emms-mpv-ipc-sentinel)
          (file-error nil)))
  (unless (process-live-p emms-mpv-ipc-proc)
    (setq emms-mpv-ipc-proc nil))
  (when (and (not emms-mpv-ipc-proc)
             delays)
    (run-at-time (car delays)
                 nil
                 #'emms-mpv-ipc-connect
                 (cdr delays))))

(defun emms-mpv-ipc-init ()
  "Initialize new mpv ipc socket/file process and associated state."
  (emms-mpv-ipc-stop)
  (emms-mpv-debug-msg "ipc: init")
  (when emms-mpv-ipc-connect-timer
    (cancel-timer emms-mpv-ipc-connect-timer))
  (with-current-buffer (get-buffer-create emms-mpv-ipc-buffer)
    (erase-buffer))
  (setq
   emms-mpv-ipc-id 1
   emms-mpv-ipc-req-table nil
   emms-mpv-ipc-connect-timer nil
   emms-mpv-ipc-connect-timer
   (run-at-time (car emms-mpv-ipc-connect-delays)
                nil
                #'emms-mpv-ipc-connect
                (cdr emms-mpv-ipc-connect-delays))))

(defun emms-mpv-ipc-stop ()
  (when emms-mpv-ipc-proc
    (emms-mpv-debug-msg "ipc: stop")
    (delete-process emms-mpv-ipc-proc)
    (setq emms-mpv-ipc-proc nil)))

(defun emms-mpv-ipc ()
  "Return open IPC process or nil, (re-)starting mpv/connection if necessary.

Return nil when starting async process/connection, and any
follow-up command should be stored to
`emms-mpv-ipc-connect-command' in this case."
  (unless (process-live-p emms-mpv-proc)
    (emms-mpv-proc-init))
  (unless (process-live-p emms-mpv-ipc-proc)
    (emms-mpv-ipc-init))
  (and
   emms-mpv-ipc-proc
   (memq (process-status emms-mpv-ipc-proc) '(open run))
   emms-mpv-ipc-proc))


;;; IPC protocol

(defun emms-mpv-ipc-id-get ()
  "Get new connection-unique id value, tracked via `emms-mpv-ipc-id'."
  (let ((ipc-id emms-mpv-ipc-id))
    (setq emms-mpv-ipc-id
          (if (< emms-mpv-ipc-id emms-mpv-ipc-id-max)
              (1+ emms-mpv-ipc-id)
            1))
    ipc-id))

(defun emms-mpv-ipc-req-send (cmd &optional handler proc)
  "Send JSON IPC request and assign HANDLER to response for it, if any.

CMD value is encoded via `json-encode'.

HANDLER func will be called with decoded response JSON
as (handler data err), where ERR will be either nil on
\"success\", \"connection-error\" or whatever is in JSON.  If
HANDLER is nil, default `emms-mpv-ipc-req-error-printer'
will be used to at least log errors.  Multiple commands can be
batched in one list as \\='(batch (cmd1 . handler1) ...), in which
case common HANDLER argument is ignored.  PROC can be specified
to avoid `emms-mpv-ipc' call (e.g. from sentinel/filter
funcs)."
  (dolist
      (cmd-and-handler
       (if (and (listp cmd)
                (eq (car cmd)
                    'batch))
           (cdr cmd)
         `((,cmd . ,handler))))
    (cl-destructuring-bind (cmd . handler)
        cmd-and-handler
      (let ((req-id (emms-mpv-ipc-id-get))
            (req-proc (or proc (emms-mpv-ipc)))
            (handler (or handler #'emms-mpv-ipc-req-error-printer)))
        (unless emms-mpv-ipc-req-table
          (setq emms-mpv-ipc-req-table (make-hash-table)))
        (let ((json (concat (json-encode (list :command cmd
                                               :request_id req-id))
                            "\n")))
          (emms-mpv-debug-msg "json >> %s" json)
          (condition-case _err
              ;; On any disconnect, assume that mpv process is to blame
              ;; and force restart.
              (process-send-string req-proc json)
            (error
             (emms-mpv-proc-stop)
             (funcall handler nil 'connection-error)
             (setq handler nil))))
        (when handler (puthash req-id handler emms-mpv-ipc-req-table))))))

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

(defun emms-mpv-ipc-recv (json)
  "Handler for all JSON lines from mpv process."
  (emms-mpv-debug-msg "json << %s" json)
  (let ((json-data (json-read-from-string json)))
    (when-let* ((req-id (alist-get 'request_id json-data)))
      ;; Response to command
      (emms-mpv-ipc-req-resolve req-id
                                (alist-get 'data json-data)
                                (alist-get 'error json-data)))
    (when-let* ((ev (alist-get 'event json-data)))
      ;; mpv event
      (emms-mpv-event-handler json-data)
      (run-hook-with-args 'emms-mpv-event-functions json-data))))

(defun emms-mpv-observe-property (sym)
  "Send mpv observe_property command for property identified by SYM.
Only sends command once per process, removing any
potential duplication if used for same properties from different functions."
  (when-let* ((id (emms-mpv-proc-symbol-id sym)))
    (emms-mpv-ipc-req-send `(observe_property ,id ,sym))))

(defun emms-mpv-observe-properties ()
  "Observe properties from `emms-mpv-observed-properties'."
  (dolist (prop emms-mpv-observed-properties)
    (emms-mpv-observe-property prop)))

(defun emms-mpv-event-idle ()
  "Delayed check for switching tracks when mpv goes idle for no good reason."
  (emms-mpv-debug-msg "idle-check (stopped=%s)" emms-mpv-stopped)
  (unless emms-mpv-stopped (emms-player-stopped)))

(defun emms-mpv-event-playing-time-sync ()
  "Request and update `emms-playing-time' after playback
seek/restart or unpause."
  (emms-mpv-ipc-req-send
   '(get_property time-pos)
   (lambda (pos err)
     (unless err (emms-playing-time-set pos)))))

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
     (unless (emms-mpv-proc-playing-p)
       (emms-mpv-proc-playing t)
       (emms-player-started emms-mpv))
     (emms-mpv-event-playing-time-sync))
    ("property-change"
     (emms-mpv-event-property-handler json-data))
    ("end-file"
     (when (emms-mpv-proc-playing-p)
       (emms-mpv-proc-playing nil)
       (emms-player-stopped))
     (when emms-mpv-ipc-stop-command
       (unless (eq emms-mpv-ipc-stop-command t)
         (funcall emms-mpv-ipc-stop-command))
       (setq emms-mpv-ipc-stop-command nil)))
    ("idle"
     ;; Can mean any kind of error before or during playback.  Example
     ;; can be access/format error, resulting in start+end without
     ;; playback-restart.
     (cancel-timer emms-mpv-idle-timer)
     (setq
      emms-mpv-idle-timer
      (run-at-time emms-mpv-idle-delay nil #'emms-mpv-event-idle)
      emms-mpv-ipc-stop-command nil))
    ("start-file" (cancel-timer emms-mpv-idle-timer))))

(defun emms-mpv-event-property-handler (json-data)
  "Handler for property change mpv events."
  (pcase (alist-get 'name json-data)
    ("pause"
     ;; json returns either `t' or `:json-false' for pause value.
     (let ((pause (if (eq t (alist-get 'data json-data))
                      t nil)))
       (setq emms-player-paused-p pause)
       (emms-mpv-event-playing-time-sync)
       (run-hooks 'emms-player-paused-hook)))
    ("duration"
     (let ((duration (alist-get 'data json-data))
           (track (emms-playlist-current-selected-track)))
       (when (and track (numberp duration) (> duration 0))
         (setq duration (round duration))
         (emms-track-set track 'info-playing-time duration)
         (emms-track-set track 'info-playing-time-min (/ duration 60))
         (emms-track-set track 'info-playing-time-sec (% duration 60)))))))


;;; Metadata update hooks

(defun emms-mpv-info-meta-connect-func ()
  "Hook function for `emms-mpv-event-connect-hook' to update
metadata from mpv."
  (emms-mpv-observe-property 'metadata))

(defun emms-mpv-info-meta-event-func (json-data)
  "Hook function for `emms-mpv-event-functions' to update
metadata from mpv."
  (when (and (string= (alist-get 'event json-data)
                      "property-change")
             (string= (alist-get 'name json-data)
                      "metadata"))
    (when-let* ((info-alist (alist-get 'data json-data)))
      (emms-mpv-info-meta-update-track info-alist))))

(defun emms-mpv-info-meta-update-track (info-alist &optional track)
  "Update TRACK with mpv metadata from INFO-ALIST.
`emms-playlist-current-selected-track' is used by default."
  (mapc
   (lambda (cc)
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
    (unless track (setq track (emms-playlist-current-selected-track)))
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


;;; High-level EMMS interface

(defun emms-mpv-cmd (cmd &optional handler)
  "Send mpv command to process/connection if both are running,
or otherwise schedule start/connect and set
`emms-mpv-ipc-connect-command' for `emms-mpv-ipc-sentinel'.
Multiple commands can be batched in one list as \\='(batch (cmd1 . handler1) ...),
in which case common HANDLER argument is ignored."
  (setq emms-mpv-ipc-connect-command nil)
  (if-let* ((proc (emms-mpv-ipc)))
      (emms-mpv-ipc-req-send cmd handler proc)
    (setq emms-mpv-ipc-connect-command cmd)))

(defun emms-mpv-playable-p (track)
  (memq (emms-track-type track)
        '(file url streamlist playlist)))

(defun emms-mpv-start (track)
  (setq emms-mpv-stopped nil)
  (emms-mpv-proc-playing nil)
  (let* ((track-name (emms-track-get track 'name))
         (track-playlist-option
          (and emms-mpv-use-playlist-option
               (memq (emms-track-get track 'type)
                     '(streamlist playlist))))
         (play-cmd `(batch
                     ((,(if track-playlist-option 'loadlist 'loadfile)
                       ,track-name replace))
                     ((set pause no))))
         (start-func
          ;; Try running play-cmd and retry it on conn failure, e.g. if mpv died
          (apply-partially 'emms-mpv-cmd play-cmd
                           (lambda (_mpv-data mpv-error)
                             (when (eq mpv-error 'connection-error)
                               (emms-mpv-cmd play-cmd))))))
    (if emms-mpv-ipc-stop-command
        (setq emms-mpv-ipc-stop-command start-func)
      (funcall start-func))))

(defun emms-mpv-stop ()
  (setq
   emms-mpv-stopped t
   emms-mpv-ipc-stop-command t)
  (emms-mpv-proc-playing nil)
  (emms-mpv-cmd `(stop))
  (emms-player-stopped))

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
