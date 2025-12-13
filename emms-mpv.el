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
;;
;; Another difference from `emms-player-mpv' is that `emms-mpv' switches
;; between video tracks fast and smoothly, without recreating video frame.

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
;;   `emms-player-playing-p', `emms-player-stopped-p', and
;;   `emms-player-paused-p'.  This allows EMMS playlists to keep track
;;   only of the linked mpv process state.  Global values of these
;;   variables are updated every time an EMMS buffer is selected.  This
;;   is done by `emms-mpv-update-current-playlist' function added to
;;   `buffer-list-update-hook'.
;;
;; - Each IPC buffer has some internal buffer-local variables to control
;;   mpv process using JSON IPC protocol (see mpv manual), mainly:
;;   `emms-mpv-proc', `emms-mpv-ipc-proc', `emms-mpv-ipc-socket'.

;;; Code:

(require 'emms)
(require 'emms-player-simple)
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
  "Parameters for mpv player."
  :type '(cons symbol alist)
  :group 'emms-mpv)

(defcustom emms-mpv-command-name "mpv"
  "mpv binary to use. Can be absolute path or just binary name."
  :type 'file
  :group 'emms-mpv)

(defcustom emms-mpv-arguments
  ;; Using "--keep-open" argument and "eof-reached" event allows us to
  ;; switch between video files smoothly, without flickering, which
  ;; happens because mpv kills video frame when the previous file stops
  ;; and recreates it when a new file starts.
  '("--terminal=no" "--keep-open=always")
  "Extra command-line arguments for started mpv process(es).
Either a list of strings or function returning such list.
Extra arguments --idle and --input-ipc-server are added automatically.
Note that unless --no-config option is specified here,
mpv will also use options from its configuration files.
For mpv binary path, see `emms-mpv-command-name'."
  :type '(choice (repeat :tag "List of mpv arguments" string)
                 function)
  :group 'emms-mpv)

(defcustom emms-mpv-environment nil
  "List of extra environment variables (\"VAR=value\" strings) to pass on to
mpv process.

These are added on top of `process-environment' by default.
Adding nil as an element to this list will discard emacs
`process-environment' and only pass variables that are specified
in the list."
  :type '(repeat (choice string
                         (const :tag "Start from blank environment" nil)))
  :group 'emms-mpv)

(defcustom emms-mpv-stop-commands '(emms-stop)
  "List of interactive commands that should really stop mpv.
By default, EMMS stops+starts the player every time when you start a new
track.  This is a long and unnecessary process (at least, for mpv).
That's why `emms-mpv' ignores all stops except the ones that came
directly from these commands called by user."
  :type '(repeat function)
  :group 'emms-mpv)

(defvar emms-mpv-proc-kill-delay 3
  "Delay until SIGKILL gets sent to `emms-mpv-proc',
if it refuses to exit cleanly on `emms-mpv-proc-stop'.")

(defvar emms-mpv-ipc-connect-delays
  '(0.1 0.1 0.1 0.1 0.1 0.1 0.2 0.2 0.3 0.3 0.5 1 1)
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

(defvar emms-mpv-file-loaded-hook nil
  "Hook (list of functions) run after \"file-loaded\" event.
Actually, this hook will be run after the following \"playback-restart\"
event because the player is not ready for seeking or other actions right
after \"file-loaded\" event.")

(defvar emms-mpv-idle-delay 0.5
  "Delay before issuing `emms-mpv-stopped' when mpv unexpectedly goes idle.")

(defvar emms-mpv-event-handlers
  '(("playback-restart" . emms-mpv-handle-event-playback-restart)
    ("property-change"  . emms-mpv-handle-event-property-change)
    ("seek"             . emms-mpv-handle-event-seek)
    ("file-loaded"      . emms-mpv-handle-event-file-loaded)
    ("start-file"       . emms-mpv-handle-event-start-file)
    ("end-file"         . emms-mpv-handle-event-end-file)
    ("idle"             . emms-mpv-handle-event-idle))
  "Alist of mpv events and their handlers.
Each handler is a function of one argument, mpv JSON data for this
event.  Handlers are called with `emms-mpv-ipc-buffer' as the current
buffer so all variables for the current mpv process are available.")

(defvar emms-mpv-property-handlers
  '(("pause"            . emms-mpv-handle-property-pause)
    ("eof-reached"      . emms-mpv-handle-property-eof-reached)
    ("duration"         . emms-mpv-handle-property-duration)
    ("metadata"         . emms-mpv-handle-property-metadata))
  "Alist of mpv properties and their handlers.
See `emms-mpv-handle-event-handlers' for details.")


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

(defvar-local emms-mpv-ipc-req-table nil
  "Auto-initialized hash table of outstanding API req_ids to their handler funcs.")

(defvar-local emms-mpv-stopped nil
  "Non-nil if playback was stopped by call from emms.
Similar to `emms-player-stopped-p', but set for future async events,
to indicate that playback should stop instead of switching to next track.")

(defvar-local emms-mpv-idle-timer nil
  "Timer to delay `emms-mpv-stopped' when mpv unexpectedly goes idle.")

(defvar-local emms-mpv-file-loaded-p nil
  "State of the latest \"file-loaded\" event.
If t, then we are somewhere between \"file-loaded\" and
\"playback-restart\" events.")

(defvar-local emms-mpv-seek-p nil
  "State of the latest \"seek\" event.
If t, then we are somewhere between \"seek\" and
\"playback-restart\" events.")


;;; General utils

(defun emms-mpv-buffer-if-live (buffer)
  "Return BUFFER if it is a live buffer, return nil otherwise."
  (and buffer
       (buffer-live-p buffer)
       buffer))

(defun emms-mpv-ipc-id-get ()
  "Get new connection-unique id value, tracked via `emms-mpv-ipc-id'."
  (let ((ipc-id emms-mpv-ipc-id))
    (setq emms-mpv-ipc-id
          (if (< emms-mpv-ipc-id emms-mpv-ipc-id-max)
              (1+ emms-mpv-ipc-id)
            1))
    ipc-id))

(defun emms-mpv-playlist-buffers ()
  "Return a list of live EMMS playlist buffers.
This is similar to `emms-playlist-buffer-list' except it doesn't check
for new buffers."
  (setq emms-playlist-buffers
        (seq-filter #'buffer-live-p emms-playlist-buffers)))


;;; Debug messages

(defvar emms-mpv-debug nil
  "If non-nil, print debug messages to *Messages* buffer.")

(defvar emms-mpv-start-time nil
  "Starting time for `emms-mpv-debug-msg'.
Set on first use, with intent to shorten timestamps in debug messages.")

(defun emms-mpv-debug-msg (format-string &rest args)
  "Print debug message to *Messages* if `emms-mpv-debug' is non-nil.
See `message' for the meaning of FORMAT-STRING and ARGS."
  (when emms-mpv-debug
    (let ((time (float-time))
          (args (mapcar (lambda (arg)
                          (if (stringp arg)
                              (string-trim arg)
                            arg))
                        args)))
      (unless emms-mpv-start-time
        (setq emms-mpv-start-time time))
      (apply #'message
             (concat "emms-mpv [%.1f] " format-string)
             (- time emms-mpv-start-time)
             args))))


;;; mpv process

(defun emms-mpv-proc-property-id (proc prop)
  "Get unique id for PROP or nil if it was already requested."
  (let ((sym-id (intern (concat "mpv-sym-" prop))))
    (unless (process-get proc sym-id)
      (let ((id (emms-mpv-ipc-id-get)))
        (process-put proc sym-id id)
        id))))

(defun emms-mpv-proc-sentinel (proc event)
  (let ((status (process-status proc)))
    (emms-mpv-debug-msg
     "proc[%s]: %s (status=%s)" proc event status)
    (when-let* ((exit    (memq status '(exit signal)))
                (buf     (emms-mpv-buffer-if-live (process-buffer proc)))
                (ipc-buf (emms-mpv-buffer-if-live
                          (with-current-buffer buf
                            emms-mpv-ipc-buffer)))
                (pl-buf  (emms-mpv-buffer-if-live
                          (with-current-buffer ipc-buf
                            emms-playlist-buffer))))
      (with-current-buffer pl-buf
        (emms-mpv-stopped)
        (emms-mpv-update-global-state-maybe
         pl-buf 'stop)))))

(defun emms-mpv-proc-init (ipc-buf)
  "Initialize new mpv process.
IPC-BUF is the buffer where `emms-mpv-proc' will be set to the started process."
  (with-current-buffer ipc-buf
    (emms-mpv-proc-stop emms-mpv-proc)
    (unless (file-directory-p (file-name-directory emms-mpv-ipc-socket))
      (make-directory (file-name-directory emms-mpv-ipc-socket)))
    (let* ((args (append
                  (list emms-mpv-command-name)
                  (if (functionp emms-mpv-arguments)
                      (funcall emms-mpv-arguments)
                    emms-mpv-arguments)
                  (list (format "--input-ipc-server=%s" emms-mpv-ipc-socket)
                        "--idle")))
           (process-environment (if (memq nil emms-mpv-environment)
                                    (delq nil emms-mpv-environment)
                                  (append emms-mpv-environment
                                          process-environment)))
           (buffer (generate-new-buffer " *emms-mpv*")))
      (setq emms-mpv-proc
            (make-process :name "emms-mpv"
                          :buffer buffer
                          :command args
                          :noquery t
                          :sentinel #'emms-mpv-proc-sentinel))
      (with-current-buffer buffer
        (setq emms-mpv-ipc-buffer ipc-buf))
      (emms-mpv-debug-msg "proc[%s]: start %s" emms-mpv-proc args))))

(defun emms-mpv-proc-stop (proc)
  "Stop running PROC instance via SIGINT.
`delete-process' (SIGKILL) timer is started if
`emms-mpv-proc-kill-delay' is non-nil."
  (when proc
    (emms-mpv-debug-msg "proc[%s]: stop" proc)
    (if (not (process-live-p proc))
        (delete-process proc)
      (interrupt-process proc)
      (run-at-time emms-mpv-proc-kill-delay nil
                   #'delete-process proc))))


;;; IPC unix socket

(defun emms-mpv-ipc-sentinel (proc event)
  (let ((status (process-status proc)))
    (emms-mpv-debug-msg "ipc[%s] status: %s; event: %s" proc status event)
    (when (memq status '(open run))
      (with-current-buffer (process-buffer proc)
        (mapc (lambda (assoc)
                (emms-mpv-observe-property proc (car assoc)))
              emms-mpv-property-handlers)))))

(defun emms-mpv-ipc-filter (proc s)
  (when-let* ((buf (emms-mpv-buffer-if-live (process-buffer proc))))
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
            (emms-mpv-ipc-recv buf json)))))))

(defun emms-mpv-ipc-connect (delays)
  "Make IPC connection process in the current buffer.
This is a subroutine of `emms-mpv-ipc-init'.

If connection attempt fails, wait for (car DELAYS) and pass (cdr DELAYS)
to the next connection attempt.

Set `emms-mpv-ipc-proc' to the resulting process or throw error if
connection is not established."
  (if (null delays)
      (error "Cannot connect to socket file %S" emms-mpv-ipc-socket)
    (let ((status (and emms-mpv-ipc-proc
                       (process-status emms-mpv-ipc-proc))))
      (emms-mpv-debug-msg "ipc status: %S" status)
      (unless (eq 'open status)
        (when (or (null status) (eq 'failed status))
          (if (or (null emms-mpv-proc)
                  (eq 'exit (process-status emms-mpv-proc)))
              (error "Stopped connecting to socket file, no mpv process")
            (setq emms-mpv-ipc-proc
                  (make-network-process
                   :name "emms-mpv-ipc"
                   :family 'local
                   :service emms-mpv-ipc-socket
                   :nowait t
                   :coding '(utf-8 . utf-8)
                   :buffer (current-buffer)
                   :noquery t
                   :filter #'emms-mpv-ipc-filter
                   :sentinel #'emms-mpv-ipc-sentinel))))
        (sleep-for (car delays))
        (emms-mpv-debug-msg "ipc: connect-delay %s" (car delays))
        (emms-mpv-ipc-connect (cdr delays))))))

(defun emms-mpv-ipc-init (ipc-buf)
  "Initialize new mpv ipc socket/file process and associated state."
  (with-current-buffer ipc-buf
    (emms-mpv-ipc-stop emms-mpv-ipc-proc)
    (emms-mpv-debug-msg "ipc: init")
    (erase-buffer)
    (setq
     emms-mpv-ipc-proc nil
     emms-mpv-ipc-req-table (make-hash-table))
    (emms-mpv-ipc-connect emms-mpv-ipc-connect-delays)))

(defun emms-mpv-ipc-stop (proc)
  (when (process-live-p proc)
    (emms-mpv-debug-msg "ipc: stop")
    (delete-process proc)))

(defun emms-mpv-ipc (ipc-buf)
  "Return open IPC process in IPC-BUF buffer.
Start mpv/connection if necessary."
  (with-current-buffer ipc-buf
    (unless (process-live-p emms-mpv-proc)
      (emms-mpv-proc-init ipc-buf))
    (unless (process-live-p emms-mpv-ipc-proc)
      (emms-mpv-ipc-init ipc-buf))
    emms-mpv-ipc-proc))


;;; IPC protocol

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
      (when-let* ((event   (alist-get 'event json-data))
                  (handler (alist-get event emms-mpv-event-handlers
                                      nil nil #'string=)))
        (funcall handler json-data)))))

(defun emms-mpv-observe-property (proc prop)
  "Send mpv observe_property command for property PROP.
Only sends command once per process, removing any
potential duplication if used for same properties from different functions."
  (when-let* ((id (emms-mpv-proc-property-id proc prop)))
    (emms-mpv-ipc-req-send proc (list 'observe_property id prop))))

(defun emms-mpv-event-idle ()
  "Delayed check for switching tracks when mpv goes idle for no good reason."
  (emms-mpv-debug-msg "idle-check (stopped=%s)" emms-mpv-stopped)
  (unless emms-mpv-stopped
    (with-current-buffer emms-playlist-buffer
      (emms-mpv-stopped))))

(defun emms-mpv-sync-playing-time-maybe ()
  "Request playng time and call `emms-player-time-set-functions' if needed.
It is needed only when `emms-playlist-buffer' for the current mpv
instance is the global playlist."
  (when (eq emms-playlist-buffer
            (default-value 'emms-playlist-buffer))
    (emms-mpv-cmd
     '(get_property time-pos)
     (lambda (sec err)
       (unless err
         (run-hook-with-args 'emms-player-time-set-functions
                             sec))))))


;;; Event handlers

(defun emms-mpv-handle-event-playback-restart (_json-data)
  "Handler for \"playback-restart\" event."
  (with-current-buffer emms-playlist-buffer
    (unless emms-player-playing-p
      (emms-mpv-started emms-mpv)
      (emms-mpv-update-global-state-maybe
       (current-buffer) 'start)))
  (when emms-mpv-file-loaded-p
    (setq emms-mpv-file-loaded-p nil)
    (with-current-buffer emms-playlist-buffer
      (run-hooks 'emms-mpv-file-loaded-hook)))
  (when emms-mpv-seek-p
    (setq emms-mpv-seek-p nil)
    (emms-mpv-sync-playing-time-maybe)))

(defun emms-mpv-handle-event-seek (_json-data)
  "Handler for \"seek\" event."
  (setq emms-mpv-seek-p t))

(defun emms-mpv-handle-event-file-loaded (_json-data)
  "Handler for \"file-loaded\" event."
  (setq emms-mpv-file-loaded-p t))

(defun emms-mpv-handle-event-end-file (json-data)
  "Handler for \"end-file\" event."
  (setq emms-mpv-seek-p nil
        emms-mpv-file-loaded-p nil)
  (with-current-buffer emms-playlist-buffer
    (emms-mpv-stopped)
    (pcase (alist-get 'reason json-data)
      ("eof"
       (emms-mpv-next-noerror))
      ("stop"
       (emms-mpv-update-global-state-maybe
        emms-playlist-buffer 'stop))
      ("error"
       (let* ((track (emms-playlist-selected-track))
              (info (alist-get 'file_error json-data))
              (err-msg (concat "ERROR during playing \""
                               (emms-track-get track 'name)
                               "\""
                               (and info (concat ": " info)))))
         (message err-msg)
         (emms-mpv-next-noerror))))))

(defun emms-mpv-handle-event-idle (_json-data)
  "Handler for \"idle\" event."
  ;; Can mean any kind of error before or during playback.  Example
  ;; can be access/format error, resulting in start+end without
  ;; playback-restart.
  (when emms-mpv-idle-timer
    (cancel-timer emms-mpv-idle-timer))
  (setq
   emms-mpv-idle-timer (run-at-time emms-mpv-idle-delay
                                    nil #'emms-mpv-event-idle)))

(defun emms-mpv-handle-event-start-file (_json-data)
  "Handler for \"start-file\" event."
  (when emms-mpv-idle-timer
    (cancel-timer emms-mpv-idle-timer)))

(defun emms-mpv-handle-event-property-change (json-data)
  "Handler for \"property-change\" event."
  (when-let* ((property (alist-get 'name json-data))
              (handler  (alist-get property
                                   emms-mpv-property-handlers
                                   nil nil #'string=)))
    (funcall handler json-data)))

(defun emms-mpv-handle-property-pause (json-data)
  "Handler for \"pause\" property change."
  ;; json returns either `t' or `:json-false' for pause value.
  (let ((pause (eq t (alist-get 'data json-data))))
    (when pause
      (emms-mpv-sync-playing-time-maybe))
    (with-current-buffer emms-playlist-buffer
      (setq emms-player-paused-p pause)
      (emms-mpv-update-global-state-maybe
       (current-buffer) 'pause))))

(defun emms-mpv-handle-property-eof-reached (json-data)
  "Handler for \"eof-reached\" property change."
  ;; XXX Sometimes, eof is not reached during over-seeking,
  ;; playback simply stops several seconds before eof.
  ;; Probably, mpv bug.
  (when-let* ((eof (eq t (alist-get 'data json-data))))
    (with-current-buffer emms-playlist-buffer
      (emms-mpv-stopped)
      (emms-mpv-next-noerror))))

(defun emms-mpv-handle-property-duration (json-data)
  "Handler for \"duration\" property change."
  (with-current-buffer emms-playlist-buffer
    (let ((duration (alist-get 'data json-data))
          (track (emms-playlist-selected-track)))
      (when (and track (numberp duration) (> duration 0))
        (setq duration (round duration))
        (emms-track-set track 'info-playing-time duration)
        (emms-track-set track 'info-playing-time-min (/ duration 60))
        (emms-track-set track 'info-playing-time-sec (% duration 60))))))

(defun emms-mpv-handle-property-metadata (json-data)
  "Handler for \"metadata\" property change."
  (when-let* ((info-alist (alist-get 'data json-data)))
    (with-current-buffer emms-playlist-buffer
      (emms-mpv-info-meta-update-track
       info-alist (emms-playlist-selected-track)))))

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

  - `track': run `emms-track-updated-functions';

  - `vars': update all the above variables but do not run hooks;

  - `all': update all the variables and run appropriate hooks."
  (when-let* ((pl-buf (emms-mpv-buffer-if-live
                       (default-value 'emms-playlist-buffer)))
              (keywords (cond ((memq 'vars keywords)
                               '(vars))
                              ((memq 'all keywords)
                               '(all pause track))
                              (t keywords))))
    (emms-mpv-debug-msg "updating global state: %S" keywords)
    (with-current-buffer pl-buf
      (dolist (keyword keywords)
        (cl-case keyword
          (vars
           (setq-default
            emms-player-playing-p emms-player-playing-p
            emms-player-stopped-p emms-player-stopped-p
            emms-player-paused-p  emms-player-paused-p))
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
          (track
           (run-hook-with-args 'emms-track-updated-functions
                               (emms-playlist-selected-track))))))))

(defun emms-mpv-handle-kill-playlist ()
  "Do some cleaning actions before killing EMMS playlist.
Delete linked mpv and mpv ipc processes and make another EMMS playlist
the current one."
  (when emms-playlist-buffer-p
    (when-let* ((ipc-buf (emms-mpv-buffer-if-live
                          (emms-mpv-current-ipc-buffer))))
      (with-current-buffer ipc-buf
        (emms-mpv-proc-stop emms-mpv-proc)
        (emms-mpv-ipc-stop emms-mpv-ipc-proc))
      (kill-buffer ipc-buf))
    (let ((buffer (current-buffer)))
      (when (eq (default-value 'emms-playlist-buffer)
                buffer)
        (let ((next-pl (car (remove buffer (emms-mpv-playlist-buffers)))))
          (setq-default emms-playlist-buffer next-pl)
          (if next-pl
              (emms-mpv-update-global-state 'all)
            (emms-mpv-stopped)
            (run-hooks 'emms-player-finished-hook)))))))

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
                  emms-player-stopped-p t))
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
    ;; Demote errors just in case since error in
    ;; `buffer-list-update-hook' could lead to a situation when a user
    ;; can't switch buffers.
    (with-demoted-errors "ERROR during updating EMMS playlist: %S"
      (let ((buffer (current-buffer)))
        (unless (eq buffer emms-mpv-playlist-buffer)
          (setq-default emms-playlist-buffer buffer)
          (emms-mpv-update-global-state 'all)
          (when emms-player-playing-p
            (emms-mpv-sync-playing-time-maybe))
          (setq emms-mpv-playlist-buffer buffer))))))

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
    (with-current-buffer emms-playlist-buffer
      emms-mpv-ipc-buffer)))


;;; High-level EMMS interface

(defun emms-mpv-cmd (cmd &optional handler)
  "Send CMD command to mpv process.
Multiple commands can be batched in one list as \\='(batch (cmd1 . handler1) ...),
in which case common HANDLER argument is ignored."
  (if-let* ((ipc-buf (emms-mpv-current-ipc-buffer)))
      (emms-mpv-ipc-req-send (emms-mpv-ipc ipc-buf) cmd handler)
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
  (let* ((track-name (emms-track-get track 'name))
         (play-cmd `(batch
                     ((loadfile ,track-name replace))
                     ((set pause no)))))
    (emms-mpv-cmd play-cmd)))

(defun emms-mpv-stop ()
  (when (memq this-command emms-mpv-stop-commands)
    (emms-mpv-cmd `(stop))))

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
