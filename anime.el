;;; anime.el --- watch anime via emacs -*- lexical-binding: t; -*-

;;; Author: Benjamin (svn)
;;; License: UNLICENSE (https://unlicense.org/)

;;; Commentary:

;; Watch anime via emacs
;; Based on https://github.com/pystardust/ani-cli/blob/master/ani-cli

;; expects "mpv" to be in $PATH

;; anime-- = private
;; anime// = public

;;; Code:

(defconst *ANIME//VERSION* "1.0.0")  ;; major changes . new additions . minor bugfixes

(defconst *ANIME//VIDEO-PLAYER* "mpv")
(defconst *ANIME//USER-AGENT* "anime.el firefox")

(setq base_api "https://hianime.at")
(setq search_api (concat base_api "/search?keyword="))

(defclass anime--response ()
  ((code :initarg :code
         :accessor anime--response-code
         :type number
         :documentation "The HTTP response code")
   (content :initarg :content
            :accessor anime--response-content
            :type string
            :initform ""
            :documentation "Contents of the response")))

(defun anime--get (url)
  "Returns request to a given `URL'"
  (with-current-buffer (url-retrieve-synchronously url t)
    (prog1
        (make-instance 'anime--response
                       :code url-http-response-status
                       :content (buffer-substring-no-properties url-http-end-of-headers (point-max)))
      (kill-buffer))))

(defun anime--html-string-to-list (content)
  (with-temp-buffer
    (insert content)
    (libxml-parse-html-region (point-min) (point-max))))

(defun anime--minibuffer-get-choice (question choices)
  "Returns user selected to given list `CHOICES'"
  (completing-read (concat question ": ") choices))

(defun anime--minibuffer-ask (question)
  "Returns user entered string to a given `QUESTION'"
  (read-string (concat question ": ")))

(defun anime--fixup-string (str)
  "Returns a url acceptable string based off `STR'"
  (replace-regexp-in-string " " "%20" (downcase str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq search (anime--get (concat search_api (anime--fixup-string (anime--minibuffer-ask "Enter anime name")))))

(setq content (anime--response-content search))

(cdddr (anime--html-string-to-list content))

;;; anime.el ends here
