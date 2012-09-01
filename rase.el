;;; rase.el --- Run At Sun Event
;;; -*- lexical-bind: t -*-

;; Author   : Andrey Kotlarski <m00naticus@gmail.com>
;; URL      : https://github.com/m00natic/rase/
;; Version  : 0.1
;; Keywords : solar, sunrise, sunset

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This is an Emacs extension that allows user provided set of
;; functions to be run at some sun events.

;;; Usage:
;; create a one argument function to be invoked at sun events, like
;; (defun switch-themes (sun-event)
;;   (cond ((eq sun-event 'sunrise)
;;	 ...set ligthish theme...)
;;	((eq sun-event 'sunset)
;;	 ...set darkish theme...)))

;; sign this function to be invoked on sun events
;; (add-to-list rase-hook 'switch-themes)

;; start the run-at-sun-event daemon
;; (rase-start t)

;;; Code:

(require 'solar)

;;;###autoload
(defcustom rase-hook nil
  "List of one argument functions to run at sun event."
  :group 'rase :type 'list)

(defvar *rase-timer* nil
  "Timer for the next sun event.")

(defmacro rase-set-timer (event time)
  "Set timer for sun EVENT at TIME."
  `(setq *rase-timer* (run-at-time ,time nil 'rase-daemon ,event)))

(defun rase-run-hooks (event)
  "Run `rase-hook' functions for the current sun EVENT."
  (mapc (lambda (hook) (funcall hook event))
	rase-hook))

(defun rase-solar-time-to-24 (time-str)
  "Convert solar type string TIME-STR to 24 hour format."
  (if (string-match "\\(.*\\)[/:-]\\(..\\)\\(.\\)" time-str)
      (format "%02d:%s"
	      (if (string-equal (match-string 3 time-str) "p")
		  (+ 12 (string-to-number (match-string 1 time-str)))
		(string-to-number (match-string 1 time-str)))
	      (match-string 2 time-str))
    time-str))

(defun rase-daemon (event &optional just-timer)
  "Execute `rase-hook' for EVENT and set timer for the next sun event.
If JUST-TIMER is non-nil, don't execute hook now."
  (cond
   ((eq event 'sunrise)
    (or just-timer (rase-run-hooks 'sunrise))
    (let* ((solar-info (solar-sunrise-sunset (calendar-current-date)))
	   (sunset-string (solar-time-string
			   (car (cadr solar-info))
			   (cadr (cadr solar-info)))))
      (rase-set-timer 'sunset sunset-string)))
   ((eq even 'sunset)
    (or just-timer (rase-run-hooks 'sunset))
    (let* ((tomorrow (calendar-current-date 1))
	   (solar-rise (car (solar-sunrise-sunset tomorrow)))
	   (sunrise (rase-solar-time-to-24 (solar-time-string
					    (car solar-rise)
					    (cadr solar-rise)))))
      (rase-set-timer 'sunrise
		      (encode-time 0 (string-to-number
				      (substring sunrise 3 5))
				   (string-to-number
				    (substring sunrise 0 2))
				   (cadr tomorrow) (car tomorrow)
				   (car (cddr tomorrow))))))))

;;;###autoload
(defun rase-start (&optional immediately)
  "Start run-at-sun-event daemon.  If IMMEDIATELY is non-nil,\
execute hooks for the previous event."
  (let ((solar-info (solar-sunrise-sunset (calendar-current-date))))
    (let ((sunrise-string (solar-time-string (caar solar-info)
					     (car (cdar solar-info))))
	  (sunset-string (solar-time-string (car (cadr solar-info))
					    (cadr (cadr solar-info))))
	  (current-time-string (format-time-string "%H:%M")))
      (cond ((string-lessp current-time-string ; before dawn
			   (rase-solar-time-to-24 sunrise-string))
	     (if immediately
		 (rase-run-hooks 'sunset))
	     (rase-set-timer 'sunrise sunrise-string))
	    ((string-lessp current-time-string ; daytime
			   (rase-solar-time-to-24 sunset-string))
	     (if immediately
		 (rase-run-hooks 'sunrise))
	     (rase-set-timer 'sunset sunset-string))
	    (t (rase-daemon 'sunset (not immediately))))))) ; evening

;;;###autoload
(defun rase-stop ()
  "Stop the run-at-sun-event daemon."
  (when *rase-timer*
    (cancel-timer *rase-timer*)
    (setq *rase-timer* nil)))

(provide 'rase)

;;; rase.el ends here
