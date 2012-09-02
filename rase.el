;;; rase.el --- Run At Sun Event
;;; -*- lexical-bind: t -*-

;; Author   : Andrey Kotlarski <m00naticus@gmail.com>
;; URL      : https://github.com/m00natic/rase/
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
;; the `solar' built-in package is used
;; these variables must be set
;; (custom-set-variables
;;  '(calendar-latitude 42.7)
;;  '(calendar-longitude 23.3))
;;
;; create an one-argument function to be invoked at sun events, like
;; (defun switch-themes (sun-event)
;;   (cond ((eq sun-event 'sunrise)
;;	 ;; ...set lightish theme...
;;	 )
;;	((eq sun-event 'sunset)
;;	 ;; ...set darkish theme...
;;	 )
;;	((eq sun-event 'midday)
;;	 ;; ...lunch...
;;	 )
;;	((eq sun-event 'midnight)
;;	 ;; ...howl...
;;	 )))
;;
;; sign this function to be invoked on sun events
;; (add-to-list rase-hook 'switch-themes)
;;
;; start the run-at-sun-event daemon
;; (rase-start t)

;;; Code:

(require 'solar)

;;;###autoload
(defcustom rase-hook nil
  "List of one-argument functions to run at sun event.
Possible values for the argument are the symbols
`sunrise', `midday', `sunrise' and `midnight'."
  :group 'rase :type 'list)

(defvar *rase-timer* nil
  "Timer for the next sun event.")

(defun rase-run-hooks (event)
  "Run `rase-hook' functions for the current sun EVENT."
  (mapc (lambda (hook) (funcall hook event))
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

(defun rase-daemon (event)
  "Execute `rase-hook' for EVENT and set timer the next sun event."
  (rase-run-hooks event)
  (let ((solar-info (solar-sunrise-sunset (calendar-current-date))))
    (let ((sunrise (car solar-info))
	  (sunset (cadr solar-info)))
      (cond
       ((not sunset)
	(if (eq event 'sunrise)
	    (rase-set-timer 'midday 12)
	  (rase-set-timer 'sunrise 0 t)))
       ((not sunrise)
	(if (eq event 'sunset)
	    (rase-set-timer 'midnight 12)
	  (rase-set-timer 'sunset 0 t)))
       (t (let* ((sunrise (car sunrise))
		 (sunset (car sunset))
		 (midday (+ sunrise (/ (- sunset sunrise) 2)))
		 (early-midnight (< midday 12))
		 (midnight (+ midday (if early-midnight 12 -12))))
	    (cond ((eq event 'midnight)
		   (rase-set-timer 'sunrise sunrise early-midnight))
		  ((eq event 'sunrise)
		   (rase-set-timer 'midday midday))
		  ((eq event 'midday)
		   (rase-set-timer 'sunset sunset))
		  ((eq event 'sunset)
		   (rase-set-timer 'midnight midnight
				   (not early-midnight))))))))))

;;;###autoload
(defun rase-start (&optional immediately)
  "Start run-at-sun-event daemon.  If IMMEDIATELY is non-nil,\
execute hooks for the previous event."
  (let ((solar-info (solar-sunrise-sunset (calendar-current-date)))
	(current-time (decode-time (current-time))))
    (let ((sunrise (car solar-info))
	  (sunset (cadr solar-info)))
      (cond
       ((not sunset)
	(if (< current-time 12)
	    (progn (if immediately (rase-run-hooks 'sunrise))
		   (rase-set-timer 'midday 12))
	  (if immediately (rase-run-hooks 'midday))
	  (rase-set-timer 'sunrise 0 t)))
       ((not sunrise)
	(if (< current-time 12)
	    (progn (if immediately (rase-run-hooks 'sunset))
		   (rase-set-timer 'midnight 12))
	  (if immediately (rase-run-hooks 'midnight))
	  (rase-set-timer 'sunset 0 t)))
       (t (let* ((sunrise (car sunrise))
		 (sunset (car sunset))
		 (midday (+ sunrise (/ (- sunset sunrise) 2)))
		 (early-midnight (< midday 12))
		 (midnight (+ midday (if early-midnight 12 -12)))
		 (current-time (/ (+ (* 60 (nth 2 current-time))
				     (cadr current-time))
				  60.0)))
	    (if (< current-time midday)
		(cond ((< sunrise current-time)
		       (if immediately (rase-run-hooks 'sunrise))
		       (rase-set-timer 'midday midday))
		      ((and (not early-midnight)
			    (< current-time midnight))
		       (if immediately (rase-run-hooks 'sunset))
		       (rase-set-timer 'midnight midnight))
		      (t (if immediately (rase-run-hooks 'midnight))
			 (rase-set-timer 'sunrise sunrise)))
	      (cond ((< current-time sunset)
		     (if immediately (rase-run-hooks 'midday))
		     (rase-set-timer 'sunset sunset))
		    ((and early-midnight (< midnight current-time))
		     (if immediately (rase-run-hooks 'midnight))
		     (rase-set-timer 'sunrise sunrise t))
		    (t (if immediately (rase-run-hooks 'sunset))
		       (rase-set-timer 'midnight midnight
				       (not early-midnight)))))))))))

;;;###autoload
(defun rase-stop ()
  "Stop the run-at-sun-event daemon."
  (when *rase-timer*
    (if (timerp *rase-timer*)
	(cancel-timer *rase-timer*))
    (setq *rase-timer* nil)))

(provide 'rase)

;;; rase.el ends here
