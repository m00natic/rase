;;; rase.el --- Run At Sun Event
;;; -*- lexical-bind: t -*-

;; Author   : Andrey Kotlarski <m00naticus@gmail.com>
;; URL      : https://github.com/m00natic/rase/
;; Keywords : solar, sunrise, sunset, midday, midnight

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
;; the `solar' built-in package is used, these variables must be set:
;; (custom-set-variables
;;  '(calendar-latitude 42.7)
;;  '(calendar-longitude 23.3))
;;
;; create a two-argument function to be invoked at sun events, like
;; (defun switch-themes (sun-event first-run)
;;   (cond ((eq sun-event 'sunrise)
;;	 ;; ...set lightish theme...
;;	 )
;;	((eq sun-event 'sunset)
;;	 ;; ...set darkish theme...
;;	 )
;;	((eq sun-event 'midday)
;;	 (when first-run
;;	     ;; ...set lightish theme...
;;	   )
;;	 ;; ...lunch...
;;	 )
;;	((eq sun-event 'midnight)
;;	 (when first-run
;;	     ;; ...set darkish theme...
;;	   )
;;	 ;; ...howl...
;;	 )))
;;
;; sign this function to be invoked on sun events
;; (add-to-list rase-hook 'switch-themes)
;;
;; start the run-at-sun-event daemon, invoking hooks immediately
;; (rase-start t)

;;; Code:

(require 'solar)

;;;###autoload
(defcustom rase-hook nil
  "List of two-argument functions to run at sun event.
Possible values for the first argument are the symbols
`sunrise', `midday', `sunrise' and `midnight'.
The second argument is non-nil only the very start of rase daemon."
  :group 'rase :type 'list)

(defvar *rase-timer* nil
  "Timer for the next sun event.")

(defun rase-run-hooks (event &optional first-run)
  "Run `rase-hook' functions for the current sun EVENT.
FIRST-RUN indicates if this is the very start of the rase daemon."
  (mapc (lambda (hook) (funcall hook event first-run))
	rase-hook))

(defun rase-set-timer (event time &optional tomorrow)
  "Set timer for sun EVENT at TIME.
If TOMORROW is non-nil, schedule it for the next day."
  (let ((time-normalize (round (* 60 time)))
	(date (calendar-current-date (if tomorrow 1))))
    (setq *rase-timer*
	  (run-at-time (encode-time 0 (% time-normalize 60)
				    (/ time-normalize 60)
				    (cadr date) (car date)
				    (nth 2 date))
		       nil 'rase-daemon event))))

(defun rase-insert-event (event time event-list)
  "Insert EVENT with TIME in EVENT-LIST keeping it sorted on time."
  (catch 'inserted
    (let ((result-list nil))
      (while event-list
	(let ((event-time (cdar event-list)))
	  (if (< time event-time)
	      (throw 'inserted
		     (nconc result-list
			    (cons (cons event time) event-list)))
	    (setq result-list (nconc result-list
				     (list (car event-list)))
		  event-list (cdr event-list)))))
      (nconc result-list (list (cons event time))))))

(defun rase-build-event-list (sunrise sunset)
  "Build ordered list of sun events using time of SUNRISE and SUNSET."
  (let* ((early-sunset (< sunset sunrise))
	 (mid (+ (min sunrise sunset)
		 (/ (abs (- sunset sunrise)) 2)))
	 (anti-mid (+ mid (if (< mid 12) 12 -12))))
    (rase-insert-event 'sunrise sunrise
		       (rase-insert-event
			'sunset sunset
			(rase-insert-event
			 (if early-sunset 'midnight 'midday) mid
			 (list (cons (if early-sunset 'midday
				       'midnight)
				     anti-mid)))))))

(defun rase-set-next-timer (event event-list)
  "Set timer for event after current EVENT in EVENT-LIST."
  (let ((first (car event-list))
	(next (catch 'next
		(while event-list
		  (let ((event-name (caar event-list)))
		    (if (eq event event-name)
			(throw 'next (cadr event-list))))
		  (setq event-list (cdr event-list))))))
    (if next
	(rase-set-timer (car next) (cdr next))
      (rase-set-timer (car first) (cdr first) t))))

(defun rase-daemon (event)
  "Execute `rase-hook' for EVENT and set timer the next sun event."
  (rase-run-hooks event)
  (let ((solar-info (solar-sunrise-sunset (calendar-current-date))))
    (let ((sunrise (car solar-info))
	  (sunset (cadr solar-info)))
      (cond ((not sunset)			; polar day
	     (if (eq event 'sunrise)
		 (rase-set-timer 'midday 12)
	       (rase-set-timer 'sunrise 0 t)))
	    ((not sunrise)			; polar night
	     (if (eq event 'sunset)
		 (rase-set-timer 'midnight 12)
	       (rase-set-timer 'sunset 0 t)))
	    (t (rase-set-next-timer
		event (rase-build-event-list (car sunrise)
					     (car sunset))))))))

;;;###autoload
(defun rase-start (&optional immediately)
  "Start run-at-sun-event daemon.  If IMMEDIATELY is non-nil,\
execute hooks for the previous event."
  (let ((solar-info (solar-sunrise-sunset (calendar-current-date)))
	(current-time (decode-time (current-time))))
    (let ((sunrise (car solar-info))
	  (sunset (cadr solar-info)))
      (cond
       ((not sunset)			; polar day
	(if (< (nth 2 current-time) 12)
	    (progn (if immediately (rase-run-hooks 'sunrise t))
		   (rase-set-timer 'midday 12))
	  (if immediately (rase-run-hooks 'midday t))
	  (rase-set-timer 'sunrise 0 t)))
       ((not sunrise)			; polar night
	(if (< (nth 2 current-time) 12)
	    (progn (if immediately (rase-run-hooks 'sunset t))
		   (rase-set-timer 'midnight 12))
	  (if immediately (rase-run-hooks 'midnight t))
	  (rase-set-timer 'sunset 0 t)))
       (t (let* ((event-list (rase-build-event-list (car sunrise)
						    (car sunset)))
		 (current-time (/ (+ (* 60 (nth 2 current-time))
				     (cadr current-time))
				  60.0))
		 (current-event (caar (last event-list))))
	    (catch 'found
	      (dolist (event-info event-list)
		(if (< current-time (cdr event-info))
		    (throw 'found nil)
		  (setq current-event (car event-info)))))
	    (if immediately (rase-run-hooks current-event t))
	    (rase-set-next-timer current-event event-list)))))))

;;;###autoload
(defun rase-stop ()
  "Stop the run-at-sun-event daemon."
  (when *rase-timer*
    (if (timerp *rase-timer*)
	(cancel-timer *rase-timer*))
    (setq *rase-timer* nil)))

(provide 'rase)

;;; rase.el ends here
