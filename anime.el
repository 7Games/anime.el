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

(defconst **ANIME//BASEURL** "https://hianime.at")
(defconst **ANIME//SEARCH** (concat **ANIME//BASEURL** "/search?keyword="))

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

(defclass anime--entry ()
  ((name :initarg :name
         :accessor anime--entry-name
         :type string
         :documentation "Name of the anime")
   (url :initarg :url
        :accessor anime--entry-url
        :type string
        :documentation "URL where it is")))

;; Assuming it looks something like this
;; (a ((href . "[URL]") (title . "[ENG NAME?]") (class . "dynamic-name") (data-jname . "[JP NAME?]")) "[ENG NAME AGAIN?]")
(defun anime--parse-entry-link (link)
  (let* ((data (nth 1 link))
         (url (cdr (nth 0 data)))
         (eng-name (cdr (nth 1 data))))
    (make-instance 'anime--entry :name eng-name :url url)))

(defun anime--get (url)
  "Returns request to a given `URL'"
  (with-current-buffer (url-retrieve-synchronously url t)
    (prog1
        (make-instance 'anime--response
                       :code url-http-response-status
                       :content (buffer-substring-no-properties url-http-end-of-headers (point-max)))
      (kill-buffer))))

;; Was gonna be generic but eh let's just hard code some stuff ;)
;; also holy unreadable
(defun anime--minibuffer-get-choice (question choices)
  "Returns user selection to given list `CHOICES'"
  ;; Embed (hide) url in the list so we can grab it later
  (let* ((choices-but-cool (mapcar (lambda (e)
                                     (concat (anime--entry-name e)
                                             (propertize (concat "|,|" (anime--entry-url e)) 'invisible t)))
                                   choices))
         ;; Stop completing-read from sorting the list
         ;; Courtesy of https://emacs.stackexchange.com/questions/41801/how-to-stop-completing-read-ivy-completing-read-from-sorting
         (completion-table
          (lambda (string pred action)
            (if (eq action 'metadata)
                '(metadata (display-sort-function . identity)
                           (cycle-sort-function . identity))
              (complete-with-action
               action choices-but-cool string pred)))))
    (completing-read question completion-table nil t)))

(defun anime--fixup-string (str)
  "Returns a url acceptable string based off `STR'"
  (replace-regexp-in-string " " "%20" (downcase str)))

;; Request part

(defun anime--request-anime-list ()
  "GET a user chosen list of anime"
  (anime--get (concat **ANIME//SEARCH** (anime--fixup-string (read-string "What anime? ")))))

(defun anime--response-to-entry (resp)
  "Converts `RESP' into a list of anime--entry"
  (let ((content (anime--response-content resp)))
    (with-temp-buffer
      (insert content)
      (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
             (all-divs (dom-by-tag dom 'div))
             (target-element (dom-by-class dom "dynamic-name"))) ;; link name for some reason
        (mapcar 'anime--parse-entry-link target-element)))))

(defun anime--get-anime-list ()
  "Returns list from our anime source"
  (anime--response-to-entry (anime--request-anime-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TESTING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'dom)

(defun anime/test/get-anime-list ()
  (interactive)
  (setq entries (anime--get-anime-list))
  (setq chosen-option (split-string  (anime--minibuffer-get-choice "Pick one: " entries) "|,|")))

(anime/test/get-anime-list)

;;; anime.el ends here
