;; -*- Emacs-Lisp -*-
;; -*- coding: utf-8; -*-


;;
;; Author: savior <michael.savior@gmail.com>
;; Keywords: webqq

;; version: 0.0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(require 'url)
(require 'json)
(require 'cl)
(require 'async)
(require 'pp)


(defvar webqq-debug t)
(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
(defgroup webqq-group nil
  "webqq group"
  :group 'entertainment)


;;;; This part is webqq log tools

(defconst webqq-log-buffer-name "*webqq-log-buffer*")
(defcustom webqq-cache-path  "~/.webqq/"
  "webqq cache path"
  :type 'string
  :group 'webqq-group)


(defconst webqq-log-file-name (concat webqq-cache-path "log"))


(defcustom webqq-login-status "online"
  "webqq default login status"
  :type 'string
  :group 'webqq-group)

(defconst webqq-face-image-cache-path (concat webqq-cache-path "face-cache"))


(defun emacs-log (&rest args)
  (let* ((log-buffer-name  webqq-log-buffer-name)
         (log-buffer (get-buffer-create log-buffer-name))
         (log-file-name webqq-log-file-name)
         (log-str (concat
                   (current-time-string)
                   " -- "
                   (apply 'format args)
                   "\n")))
    (save-excursion
      (with-current-buffer log-buffer
        (goto-char (point-max))
        (insert log-str))
      (with-temp-buffer
        (insert log-str)
        (write-region (point-min) (point-max) log-file-name t
                      'nomessage)))))




;;;;This is use to keep web session


(defvar webqq-cookies nil)
(defvar webqq-account-info nil)


(defconst webqq-referer-proxy-url "http://d.web2.qq.com/proxy.html?v=20110331002&callback=1&id=3")
(defconst webqq-user-agent  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1700.123 Safari/537.3")
(defconst webqq-http-header (list (cons "Referer" webqq-referer-proxy-url)
                                (cons "User-Agent" webqq-user-agent)
                                        ;(cons "Host" "ssl.ptlogin2.qq.com")
                                (cons "Connection" "Keep-Alive")
                                ))

(defun webqq-update-cookie (cookies)
  (let ((inter (cl-intersection  cookies webqq-cookies :key 'car :test 'string=)))
    (setf webqq-cookies (append (cl-set-difference webqq-cookies inter :key 'car :test 'string=) cookies))))


(defvar webqq-session
  '(("clientid" . nil) ("sessionid" . nil) ("vfwebqq" . nil) ("ptwebqq" . nil) ("loginsig" . nil) ("cfacekey" . nil) ("cfacesig" . nil)
    ("emailauthkey" . nil) ("index" . nil) ("port" . nil) ("pollerrorcnt" . nil) ("status" . nil)))


(defun webqq-session-value (key)
  (cdr (assoc key webqq-session)))

(defun webqq-cookie-value (key)
  (cdr (assoc key webqq-cookies)))



(defun webqq-update-loginsig-session (str)
  (string-match "g_login_sig=encodeURIComponent(\"\\(.+\\)\")" str)
  (let ((sig (match-string 1 str)))
    (when sig (setf (cdr (assoc "loginsig" webqq-session)) sig))))


(defun webqq-parse-response-set-cookie (response)
  (remove-if 'null (mapcar (lambda (str)
                             (when (string-match-p "Set-Cookie" str)
                               (progn
                                 (string-match "Set-Cookie: \\(.+?\\);" str)
                                 (let* ((c (match-string 1 str))
                                        (clist (split-string c "=" )))
                                   (if (< 0 (length (cadr clist))) (cons (car clist) (cadr clist)))))))
                           (split-string response "\n" ""))))


(defun webqq-update-session-from-json (json)
  "This is the function used when sucessful login and then update the http session."
  (when webqq-debug (message "webqq-update-session-from-json begin"))
  (when (= 0 (cdr (assoc 'retcode json)))
    (let ((result (cdr (assoc 'result json))))
      (setf webqq-account-info (list (cons "uin" (cdr (assoc 'uin result)))
                                     (cons "qq" (cdr (assoc 'uin result)))))
      (setf (cdr (assoc "sessionid" webqq-session) ) (cdr (assoc 'psessionid result)))
      (setf (cdr (assoc "vfwebqq" webqq-session) ) (cdr (assoc 'vfwebqq result)))
      (setf (cdr (assoc "status" webqq-session) ) (cdr (assoc 'status result)))
      (setf (cdr (assoc "index" webqq-session) ) (cdr (assoc 'index result)))
      (setf (cdr (assoc "port" webqq-session) ) (cdr (assoc 'port result))))
    ))

(defun webqq-curl-shell-command (url url-args  http-header cookies character-code &optional show-detail)
  "This function is used to make curl command for call."
  (setq url-request-param
        (if url-args
            (concat " -d " "'" (mapconcat #'(lambda (arg)
                                              (concat (car arg) "=" (cdr arg))
                                              ) url-args "&") "'")
          ""))
  (setq url-header-param
        (if http-header
            (concat  (mapconcat #'(lambda (arg)
                                    (concat " -H '" (car arg) ":" (cdr arg) "'")) http-header " "))
          ""))
  (setq cookie-param
        (if cookies
            (concat " --cookie " "'" (mapconcat #'(lambda (arg)
                                                    (concat (car arg) "=" (cdr arg))) cookies ";") "' ")
          ""))
  (setq detail-info (if show-detail " -v " ""))
  (concat "curl -s  " detail-info url-request-param url-header-param cookie-param (concat " '" url "'")))




(defun webqq-curl-post (url url-args  http-header cookies character-code &optional show-detail)
  "This is make the curl commmand string to call curl in system.
The post-data is the alist the key and the value both the string."

  (let ((cmd  (webqq-curl-shell-command url url-args  http-header cookies character-code show-detail)))
    (when webqq-debug (print cmd))
    (let ((rep (shell-command-to-string cmd)))
      (when webqq-debug (print rep))
      rep)))


(defun http-body-data (data)
  (substring data (+ 2 (string-match "\n\n" data)) (length data)))

(defun http-get-response (url)
  "Get the http response info."
  (let
      ((url-request-method "GET")
       (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
       (buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (concat (buffer-substring-no-properties (point) (point-max))))))



(defun webqq-get-json (url &optional url-args callback callback-args)
  "get json data "
  (let ((url-request-method "GET"))
    (if url-args
        (setq url-request-data (mapconcat #'(lambda (arg)
                                              (concat (url-hexify-string (car arg))
                                                      "="
                                                      (url-hexify-string (cdr arg))))
                                          url-args "&")))
    (if callback
        (url-retrieve url callback callback-args)
      (url-retrieve-synchronously url))))


(defun webqq-post-url (url &optional url-args callback callback-args)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Accept-Encoding" . "gzip, deflate")
           ("Accept-Charset" . "UTF-8")
           ("Accept-Language" . "zh-cn")
           ("Cache-Control" . "no-cache")
           ("Connection" . "Keep-Alive")
           ("User-Agent:" . "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1700.123 Safari/537.3"))))
    (if url-args
        (setq url-request-data
              (mapconcat #'(lambda (arg)
                             (concat (url-hexify-string (car arg)) "=" (url-hexify-string (cdr arg)))) url-args "&")))
    (if callback
        (url-retrieve url callback callback-args)
      (url-retrieve-synchronously url))))

(defun switch-to-buffer-for-responce (status)
  (switch-to-buffer
   (current-buffer)))



(defun webqq-get-json-success-p (json)
  (when (= 0 (cdr (assoc 'retcode json)))
    (cdr (assoc 'result json))))

;;; This part is For WEBQQ login



(defconst webqq-random-size 10000000000000000)

(defvar webqq-username nil)
(defvar webqq-password nil)

(defvar webqq-base-path (file-name-directory load-file-name))




(defvar webqq-img-temp-buffer-name "webqq-tmp-img")
(defconst webqq-verify-image-url "http://captcha.qq.com/getimage?aid=1003903&r=%s&uin=%s")
(defconst webqq-login-channel-url "http://d.web2.qq.com/channel/login2")

(defconst webqq-gateway-url "https://ui.ptlogin2.qq.com/cgi-bin/login?daid=164&target=self&style=5&mibao_css=m_webqq&appid=1003903&enable_qlogin=0&no_verifyimg=1&s_url=http%3A%2F%2Fweb2.qq.com%2Floginproxy.html&f_url=loginerroralert&strong_login=1&login_state=10&t=20130723001")
(defconst webqq-check-verify-url "https://ssl.ptlogin2.qq.com/check?uin=%s&appid=1003903&js_ver=10038&r=%s&js_type=0&login_sig=%s")
(defvar webqq-check-verify-url-u1 "&u1=http%3A%2F%2Fweb2.qq.com%2Floginproxy.html")
(defvar webqq-login-url "https://ssl.ptlogin2.qq.com/login?from_ui=1&t=1&u=%s&p=%s&verifycode=%s&login_sig=%s&g=1&login2qq=1&pttype=1&webqq_type=10&daid=164&dumy=&ptlang=2052&aid=1003903&h=1&js_type=0&action=4-28-1632882&remember_uin=1&mibao_css=m_webqq&js_ver=10038&fp=loginerroralert&ptredirect=0")
(defvar webqq-login-url-u1 "&u1=http%3A%2F%2Fweb.qq.com%2Floginproxy.html%3Flogin2qq%3D1%26webqq_type%3D10")
(defvar webqq-pt4-auth-url  "http://ptlogin2.qq.com/pt4_auth")

(defvar webqq-mail-login-url "http://mail.qq.com/cgi-bin/login?fun=passport&from=webqq")

(defun response-webqq-check-login-info (str)
  (let ((a nil)
        (b nil)
        (c nil))
    (string-match "ptui_checkVC('\\(.+?\\)'.*'\\(.+?\\)'.*'\\(.+?\\)')" str)
    (setq a (match-string 1 str))
    (setq b (match-string 2 str))
    (setq c (match-string 3 str))
    (values a b c)))

(defun webqq-check-verify (username sessionid)
  (let* ((r (webqq-create-random))
         (header (list (cons "Referer" webqq-referer-proxy-url)
                       (cons "User-Agent" webqq-user-agent)
                       (cons "Host" "ssl.ptlogin2.qq.com")))
         (url (concat (format webqq-check-verify-url username r sessionid) webqq-check-verify-url-u1)))
    (let ((response (webqq-curl-post url nil header webqq-cookies 'utf-8 t)))
      (webqq-update-cookie (webqq-parse-response-set-cookie response))
      (response-webqq-check-login-info response))))


(defun create-verify-img (main-buffer)
  "verify-image-url random-size webqq-username switch-to-buffer generate-new-buffer  img-tmp-buffer  insert-image create-image get-http-body get-response format random 1.0 jpeg t"
  (switch-to-buffer main-buffer)
  (end-of-buffer)
  (let ((response (http-get-response (format webqq-verify-image-url (webqq-create-random) webqq-username))))
    (webqq-update-cookie (webqq-parse-response-set-cookie response))
    (insert-image (create-image (http-body-data response) 'jpeg t))))


(defun encoding-password (password hvcode v2code)
  "This is the function use to encoding the password by some kind of md5."
  (let ((hex-code (replace-regexp-in-string "\\\\\\x" "" hvcode)))
    (shell-command-to-string (format "%sresources/encode.sh %s %s '%s'" webqq-base-path password hex-code v2code))))

(defun webqq-create-random ()
  (/ (* (random webqq-random-size) 1.0) webqq-random-size))

(defun webqq-login-success-p (str)
  (progn
    (string-match "ptuiCB('\\(.\\)'.*?'\\(.+?\\)'.*?'\\(.+?\\)'" str)
    (setq result (match-string 1 str))
    (setq redirect-url (match-string 3 str)))
  (cond ;((string= "0" result) nil)
   ((string= "3" result) (message "password is wrong") )
   ((string= "4" result)(message "verify code is wrong") )
   ((string= "7" result) (message "IO error"))
   (t (message "inval user")))
  (when (string= "0" result)
    redirect-url))


(defun webqq-post-auth (url)
  (let ((response (webqq-curl-post url nil webqq-http-header webqq-cookies 'uft-8 t)))
    (webqq-update-cookie (webqq-parse-response-set-cookie response))))


(defun webqq-login-mail ()
  (let ((response (webqq-curl-post webqq-mail-login-url nil
                                   '(("Referer" . "https://mail.qq.com/cgi-bin/loginpage")
                                     ("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1700.123 Safari/537.3"))
                                   webqq-cookies 'utf-8 t)))
    response))


(defun webqq-try-to-login (pd vcode)
  (let ((response-data (webqq-curl-post
                        (concat (format webqq-login-url webqq-username pd (url-hexify-string vcode) (webqq-session-value "loginsig"))
                                webqq-login-url-u1)
                        nil
                        (list (cons "Referer" webqq-referer-proxy-url)
                              (cons "User-Agent" webqq-user-agent)
                              (cons "Host" "ssl.ptlogin2.qq.com")) ;;;header
                        webqq-cookies
                        'utf-8 t)))

    (let ((redirect-url (webqq-login-success-p response-data)))
      (when redirect-url
        (webqq-update-cookie (webqq-parse-response-set-cookie response-data))
        (emacs-log "try to login success")
        (webqq-post-auth redirect-url)))))


(defun webqq-login-gateway (url)
  (let* ((response (webqq-curl-post url nil nil nil nil t))
         (result (string-match-p "< HTTP/1.1 200 OK" response)))
    (when webqq-debug (print response))
    (if result
        (progn
          (webqq-update-cookie (webqq-parse-response-set-cookie response))
          (webqq-update-loginsig-session response)
          (emacs-log "login gateway success"))
      (error "can not login the gateway!"))
    result))


(defun webqq-login-channel ()
  (when webqq-debug (message "webqq-login-channel begin"))
  (setf (cdr (assoc "clientid" webqq-session))  (+ 100000000 (random 900000000)))
  (let ((r (json-encode (list

                         ;;hidden online

                         :status "online"
                         :ptwebqq (webqq-cookie-value "ptwebqq")
                         :passwd_sig ""
                         :clientid (webqq-session-value "clientid")
                         :psessionid ""))))
    (let ((response (webqq-curl-post webqq-login-channel-url (list (cons "clientid" (int-to-string (webqq-session-value "clientid")))
                                                                   (cons "psessionid" (webqq-session-value "sessionid"))
                                                                   (cons "r" r)) webqq-http-header webqq-cookies 'utf-8 t)))
      (webqq-update-session-from-json (json-read-from-string
                                       (substring response  (+ (string-match "Closing connection 0\n" response)
                                                               (length  "Closing connection 0\n")))))))
  (emacs-log "webb-login-channel success"))



(defun webqq-hash-time (str)
  (let ((h 0))
    (loop for c across str
          do  (setf h (+ (* 33 h) c)))
    (int-to-string (mod h 4294967296))))



(defun webqq-pt4-auth ()
  (webqq-pt4-parse-response
   (webqq-curl-post webqq-pt4-auth-url (list (cons "daid" "4")
                                             (cons "appid" "1")
                                             (cons "auth_token" (webqq-hash-time (webqq-cookie-value "supertoken"))))
                    webqq-http-header
                    webqq-cookies
                    'utf-8
                    t)))


(defun webqq-pt4-parse-response (str)
  (string-match "ptui_auth_CB('\\(.*\\)'.*?'\\(.*\\)')" str)
  (let ((retcode (match-string 1 str))
        (pt4-url (match-string 2 str)))
    (if (string= "0" retcode)
        pt4-url
      (error str))))



;;;###autoload
(defun webqq-login2 (main-buffer)
  (setf webqq-username (read-string (format "Input your QQ username: "))) (setf webqq-password (read-passwd (format "Input your QQ password: ")))
  (setq gateway-response (webqq-login-gateway webqq-gateway-url))
  (emacs-log "login gateway and the response is : %s" gateway-response)
  (multiple-value-bind (vtype verify-code verify-hex-code) (webqq-check-verify webqq-username (webqq-session-value "loginsig"))
    (emacs-log vtype verify-code verify-hex-code)
    (let* ((vcode (if (string= vtype "0")
                      (progn
                        (emacs-log "without verify log")
                        verify-code)

                    (progn
                      (emacs-log "you have to input the verify code")
                      (create-verify-img (get-buffer main-buffer))
                      (read-string (format "Input the verify code : ")))))
           (pd (encoding-password webqq-password verify-hex-code vcode)))
                                        ;(message  vtype verify-code verify-hex-code)
      (webqq-try-to-login pd vcode)))
  (webqq-login-channel)
  (webqq-face-sig)
                                        ;(webqq-groups-init)
  (emacs-log "login finish!!")
  (message "login finish"))

(defconst webqq-face-sig-url "http://d.web2.qq.com/channel/get_gface_sig2?clientid=%s&t=%s&psessionid=%s")

(defun webqq-face-sig ()
  (let* ((response (webqq-curl-post
                    (format webqq-face-sig-url (webqq-session-value "clientid") (int-to-string (truncate (float-time))) (webqq-session-value "sessionid"))
                    nil
                    webqq-http-header webqq-cookies 'utf-8 nil))
         (json (webqq-get-json-success-p (json-read-from-string response))))

    (when  json
      (setf (cdr (assoc "cfacekey" webqq-session)) (cdr (assoc 'gface_key json)))
      (setf (cdr (assoc "cfacesig" webqq-session)) (cdr (assoc 'gface_sig json))))))



;; This part is used for send and receive message






(defvar webqq-friend-list nil)
(defconst webqq-user-categories-url "http://s.web2.qq.com/api/get_user_friends2")
(defconst webqq-send-msg-to-user-url "http://d.web2.qq.com/channel/send_buddy_msg2")
(defconst webqq-poll-msg-url "http://d.web2.qq.com/channel/poll2")
(defconst webqq-friend-info-url "http://s.web2.qq.com/api/get_friend_info2")



(defvar webqq-user nil)
(defconst webqq-user-info-url "http://s.web2.qq.com/api/get_friend_info2")
(defconst webqq-user-image-url "http://face10.qun.qq.com/cgi/svr/face/getface")



(defun webqq-parse-friend-list-uin-nickname ()
  (let ((lst (map 'list  #'(lambda (x) (append (cdr (assoc 'users x)))) webqq-friend-list)))))

(defun webqq-get-friend-info-from-uin (uin)
  (let ((friendlist (remove-if 'null (mapcar #'(lambda (x) (cadr (assoc 'users x))) webqq-friend-list))))
    (do ((l friendlist (cdr friendlist))
         (friend nil))
        ((or (null l) friend) friend)
      (if (= (cdr (assoc 'uin  (car friendlist))) uin)
          (setf friend (car friendlist))))))

(defun webqq-friend-nickname (friend-info)
  (if (null friend-info)
      nil
    (cdr (assoc 'nick friend-info))))

(defun webqq-parse-friend-nickname (uin)
  (webqq-friend-nickname (webqq-get-friend-info-from-uin uin)))

(defun webqq-get-account-info ()
  (let ((json (json-read-from-string (webqq-curl-post webqq-user-info-url
                                                      (list (cons "tuin" (int-to-string (cdr (assoc "uin" webqq-account-info))))
                                                            (cons "verifysession" "")
                                                            (cons "code" "")
                                                            (cons "vfwebqq" (webqq-session-value "vfwebqq"))
                                                            (cons "t" (int-to-string (truncate (float-time)))))
                                                      webqq-http-header webqq-cookies 'utf-8 nil))))
    (webqq-get-json-success-p json)))

(defun webqq-download-friends-face-async ()
  (let ((lst (map #'(lambda (x) (int-to-string (cdr x))) webqq-friend-list-uin-nickname-plist)))
    (dolist (user lst)
      (let ((cmd (concat (webqq-curl-shell-command webqq-user-image-url
                                                   (list (cons "uin" user)
                                                         (cons "vfwebqq" (webqq-session-value "vfwebqq"))
                                                         (cons "t" (int-to-string (truncate (float-time))))
                                                         (cons "cache" "0")
                                                         (cons "type" "1")
                                                         (cons "fid" "0"))
                                                   webqq-http-header
                                                   webqq-cookies
                                                   'utf-8
                                                   nil) " > " webqq-face-image-cache-path "/" user ".jpg"))))

      (async-start
       `(lambda ()
          (set 'cmd ,cmd)
          (concat "" (shell-command-to-string cmd)))
       (lambda (result) ())))))



(defun webqq-get-user-categories-json ()
  "WARN that this function change the cookie value to the session value and i do not konw that this will effect future function or not."
  (setf (cdr (assoc "ptwebqq" webqq-cookies)) (webqq-session-value "vfwebqq"))
  (let* ((send-json (json-encode-list
                     (list (cons "h" "hello")
                           (cons "vfwebqq" (webqq-session-value "vfwebqq"))
                           (cons "hash" (webqq-hash (cdr (assoc "uin" webqq-account-info))
                                                    (webqq-session-value "vfwebqq"))))) )
         (json (json-read-from-string (webqq-curl-post webqq-user-categories-url (list (cons "r" send-json))
                                                       webqq-http-header
                                                       webqq-cookies
                                                       'utf-8
                                                       nil))))
    ;(emacs-log "webqq-get-user categories data is : %s "  json)
    (webqq-get-json-success-p json)))





(defun webqq-hash (b str)
  (setq a (make-vector 4 0))
  (do* ((index 0 (1+ index)))
      ((= index (length str)) a)
    (setf (aref a (mod index 4))
          (logxor (aref a (mod index 4))
                  (string-to-char (substring str index (+ 1 index))))))
  (setq j ["EC" "OK"])
  (setq d (make-vector 4 nil))
  (setf (aref d 0)
        (logxor (logand (ash b -24) 255)
                (string-to-char (substring (aref j 0) 0  1))))

  (setf (aref d 1) (logxor (logand (ash b -16) 255)
                           (string-to-char (substring (aref j 0) 1  2))))

  (setf (aref d 2) (logxor (logand (ash b -8) 255)
                           (string-to-char (substring (aref j 1) 0  1))))
  (setf (aref d 3) (logxor (logand b 255)
                           (string-to-char (substring (aref j 1) 1  2))))
  (setf  j (make-vector 8 nil))
  (do ((s 0 (1+ s)))
      ((= s 8) nil)
    (setf (aref j s) (if (evenp s) (aref a (ash s -1)) (aref d (ash s -1)))))

  (do ((s 0 (1+ s))
       (d "" ))
      ((= s (length j)) d)
    (setf d (concat d (format "%X%X" (logand 15 (ash (aref j s) -4)) (logand 15 (aref j s)))))))

(defvar webqq-friend-list-uin-nickname-plist nil)
(defvar webqq-friend-list-nickname-uin-plist nil)



(defun webqq-parse-friend-categories (json)
  (let ((friend-info (webqq-left-join-info (coerce (cdr (assoc 'info json)) 'list)
                                           (coerce (cdr (assoc 'friends json) ) 'list)))
        (group-info (mapcar #'(lambda (x)
                                (append x (list (cons 'users nil))))
                            (append '(((name . "default friend") (sort . 0) (index . 0)))
                                    (cl-sort (coerce (cdr (assoc 'categories json)) 'list)
                                             #'< :key #'(lambda (x) (cdr (assoc 'sort x)))))))
        )

    (dolist (f friend-info)
      (setf webqq-friend-list-uin-nickname-plist (append webqq-friend-list-uin-nickname-plist
                                                         (list (cons (cdr (assoc 'uin f))
                                                                     (cdr (assoc 'nick f))))))
      (setf webqq-friend-list-nickname-uin-plist (append webqq-friend-list-nickname-uin-plist
                                                         (list (cons (cdr (assoc 'nick f))
                                                                     (cdr (assoc 'uin f))))))
      (dolist (g group-info)
        (if (= (cdr (assoc 'categories f)) (cdr (assoc 'index g) ))
            (setf (cdr (assoc 'users g))
                  (append (cdr (assoc 'users g)) (list f))))))
    group-info))



(defun webqq-left-join-info (apair bpair)
  (let ((lst (map 'list #'(lambda (x) (cdr (assoc 'uin x))) bpair)))
    (map 'list #'(lambda (x)
                   (if (member (cdr (assoc 'uin x)) lst)
                       (append x
                               (remove-if #'(lambda (x) (equalp 'uin (car x)))
                                          (car (remove-if nil (map 'list #'(lambda (xx)
                                                                             (if (= (cdr (assoc 'uin x)) (cdr (assoc 'uin xx))) xx)) bpair))))
                               ))) apair)))





(defun webqq-init-friend-list ()
  (emacs-log "begin to init the friend list")
  (webqq-dispaly-info-on-base-buffer "begin to init the friend list , please to wait for a moment. ")
  (setf webqq-friend-list nil)
  (do ((json (webqq-get-user-categories-json))
       (i 0 (+ 1 i)))
      ((or (not (null json))
           (> i 10)) (setf webqq-friend-list (webqq-parse-friend-categories json)))
    (sleep-for 1))
  (if webqq-friend-list
      (progn
        (webqq-dispaly-info-on-base-buffer (message "friend list init successed. "))
        (emacs-log "friend list is %s" webqq-friend-list))
    (progn
      (webqq-dispaly-info-on-base-buffer (message "friend list init failed , please check the reason."))
      (emacs-log "friend list init failed.")))

  webqq-friend-list)


(defconst webqq-send-message-warm-message "来自星星的 Emacs {只能接收纯文本}")

(defun webqq-send-message-to-user (uin msg)
  (let* ((send-json (json-encode (list :to uin
                                       :face 0
                                       :content (list
                                                 (url-encode-url msg)
                                                 "\\r\\n(Sent from Emacs webqq)\\r\\n"
                                                 (url-encode-url webqq-send-message-warm-message)
                                                 ""
                                                 (list "font" (list :name   (url-encode-url "宋体")  :size  10 :style (list 0 0 0) :color "000000")))
                                       :msg_id "91310001"
                                       :clientid (int-to-string (webqq-session-value "clientid"))
                                       :psessionid (webqq-session-value "sessionid"))))
         (response (webqq-curl-post webqq-send-msg-to-user-url
                                    (list (cons "clientid" (int-to-string (webqq-session-value "clientid")))
                                          (cons "psessionid" (webqq-session-value "sessionid"))
                                          (cons "r" (url-encode-url (fix-curl-send-json-data send-json))))
                                    webqq-http-header webqq-cookies 'utf-8 t)))
    response))



(defvar webqq-group-list-uin-nickname-plist nil)
(defvar webqq-group-list-nickname-uin-plist nil)


(defvar webqq-remain-group-list nil)


(defconst webqq-send-message-to-group-url "http://d.web2.qq.com/channel/send_qun_msg2")
(defun webqq-send-message-to-group (uin msg)
  (let* ((send-json (json-encode (list :group_uin uin
                                       :key (webqq-session-value "cfacekey")
                                       :sig (webqq-session-value "cfacesig")
                                       :content (list
                                                 (url-encode-url msg)
                                                 "\\r\\n(Sent from Emacs webqq)\\r\\n"
                                                 (url-encode-url webqq-send-message-warm-message)
                                                 ""
                                                 (list "font" (list :name   (url-encode-url "宋体")  :size  10 :style (list 0 0 0) :color "000000")))
                                       :msg_id "91310001"
                                       :clientid (int-to-string (webqq-session-value "clientid"))
                                       :psessionid (webqq-session-value "sessionid"))))
         (response (webqq-curl-post webqq-send-message-to-group-url
                                    (list (cons "clientid" (int-to-string (webqq-session-value "clientid")))
                                          (cons "psessionid" (webqq-session-value "sessionid"))
                                          (cons "r" (url-encode-url (fix-curl-send-json-data send-json))))
                                    webqq-http-header webqq-cookies 'utf-8 nil)))
    response))



(defun fix-curl-send-json-data (send-json)
  "This function is used to fix the shell command use some simble like * {} [] which is json data."
  (let ((begin (cl-search "["  send-json))
        (end (cl-search "]" send-json :from-end t)))
    (concat (substring send-json 0 begin) "\""
            (mapconcat #'(lambda (c) c) (split-string (substring send-json begin (+ 1 end)) "\"") "\\\"")
            "\""
            (substring send-json (+ 1 end)))))


(defun webqq-get-user-info (uin)
  "This is used to get user infomation."
  (let ((json (json-read-from-string (webqq-curl-post webqq-friend-info-url
                                                      (list (cons "tuin" uin )
                                                            (cons "verifysession" "")
                                                            (cons "code"  "")
                                                            (cons "vfwebqq"  (webqq-session-value "vfwebqq"))
                                                            (cons "t" (int-to-string (truncate (float-time)))))
                                                      webqq-http-header webqq-cookies 'utf-8 nil))))
    (if (= 0 (cdr (assoc 'retcode json)))
        (cdr (assoc 'result json)))))





(defconst webqq-get-group-list-url "http://s.web2.qq.com/api/get_group_name_list_mask2")


(defvar webqq-grouplist nil)



(defun webqq-find-group-code (gid)
  (cdr (assoc 'code (webqq-find-group gid))))


(defun webqq-find-group (gid)
  (car (cl-member-if #'(lambda (x) (= gid (cdr (assoc 'gid x)))) webqq-grouplist)))

(defun webqq-remain-need-to-get-group-info ()
  "This is to find the download failed groups used to redownload."
  (remove-if 'null (map 'list #'(lambda (x) (if (null (assoc 'member x))
                                (list (assoc 'gid x)
                                      (assoc 'name x)))) webqq-grouplist)))




(defun webqq-update-group-member (gid mlist)
  (block nil
    (do* ((l webqq-grouplist (cdr l)))
        ((null l))
      (let ((group (car l)))
        (if (= gid (cdr (assoc 'gid group)))
            (progn
                                        ;(setf group (acons 'member mlist group))
              (let ((memb (assoc 'member group)))
                (if memb
                    (setf (cdr memb) mlist)
                  (progn
                    (setf (cdr (last group)) (list (cons 'member mlist)) )
                    (return-from nil))))))))))



(defun webqq-init-group-list ()
;  (interactive)
  (setf tt (webqq-get-group-list))
  (when tt
    (webqq-dispaly-info-on-base-buffer "webqq-try-to-get-grouplist finish!")
    (emacs-log "fetch the group list and the result is : %s " tt))
  (webqq-try-get-all-groups-member)

  (when nil
    (progn
      (emacs-log "fetch all groups member and the result is : %s" webqq-grouplist)
      (webqq-dispaly-info-on-base-buffer "webqq-groups init finish!!"))
    (progn
      (emacs-log "fetch all groups member and the result is : %s" webqq-grouplist)
      (webqq-dispaly-info-on-base-buffer "webqq-groups init failed!!")))
  t)


(defun webqq-get-group-list ()
  (let* ((json-send (json-encode (list :vfwebqq (webqq-session-value "vfwebqq")))))
    (do ((json (webqq-get-json-success-p
                (json-read-from-string
                 (webqq-curl-post webqq-get-group-list-url
                                  (list (cons "r" json-send))
                                  webqq-http-header webqq-cookies 'utf-8 nil))))
         )
        ((not (null json)) (setf webqq-grouplist (coerce (cdr (assoc 'gnamelist json)) 'list)))
      (sit-for 0.1))))








(defun webqq-get-group-info-async (gcode gid)
  "This function is used to update the group members for gcode."
  (emacs-log "begin to get used async group member for %s " gcode)
  (webqq-dispaly-info-on-base-buffer (format "trying to get group member for [%s]." gcode))
  (let ((cmd (webqq-curl-shell-command  (format webqq-get-group-info-url
                                                (int-to-string (truncate (float-time)))
                                                (int-to-string gcode)
                                                (webqq-session-value "vfwebqq"))  nil
                                                webqq-http-header webqq-cookies 'utf-8 nil )))

    (async-start

     `(lambda ()
        (set 'cmd ,cmd)
        (concat "" (shell-command-to-string cmd)))

     `(lambda (result)
        (set 'gcode ,gcode)
        (set 'gid ,gid)

        (when result
          ;;;;My Emacs will insert this to the terminal which make the json data error. And I do not know how to fix it .
          (let* ((retfix (mapconcat 'identity (split-string result "Loading vc-git...\n") ""))
                (json (ignore-errors (json-read-from-string retfix) )))
            (if json
                (progn
                  (when webqq-debug
                    (princ  retfix))
                  (emacs-log "get group %s data success..." gid)
                  (let ((ret (webqq-get-json-success-p json)))
                    (if ret
                        (webqq-get-group-info-async-callback ret gid)
                      ;(webqq-get-group-info-async gcode)
                      )))
              (emacs-log "return failed and the [%s]group info get result is : %s "gid retfix))))))))


(defun webqq-get-group-info-async-callback (result gid)
  (webqq-dispaly-info-on-base-buffer (format "get group  %s member  successed! \n  \n" gid))
  (emacs-log "get group member for %s " gid)
  (let ((mlist (webqq-group-info-merge result)))
    (webqq-update-group-member gid mlist))
  (emacs-log "group %s update success" gid))

(defun webqq-try-get-all-groups-member ()
  (dolist (group webqq-grouplist)
    (let ((code (cdr (assoc 'code group)))
          (gid (cdr (assoc 'gid group))))
     (webqq-get-group-info-async code gid)))
  t)



(defun webqq-group-info-merge (json)
  (setf lst (coerce (cdr (assoc 'minfo json)) 'list))
  (setf clst (coerce (cdr (assoc 'cards json)) 'list))
  (mapcar (lambda (x) (let* ((uin (cdr (assoc 'uin x)))
                            (card-info (car (cl-member-if #'(lambda (y) (= uin (cdr (assoc 'muin y)))) clst ))))
                        (if card-info
                          (acons 'card  (cdr (assoc 'card card-info)) x)
                          x))) lst))


(defconst webqq-get-group-info-url "http://s.web2.qq.com/api/get_group_info_ext2?t=%s&gcode=%s&vfwebqq=%s")

(defun webqq-get-group-info (gcode)
  "This function is used to update the group members for gcode."
  (emacs-log "get group member for %s " gcode)
  (webqq-dispaly-info-on-base-buffer (format "trying to get group member for [%s]." gcode))
  (let ((ret))
    (while (null ret)
      (setf ret (webqq-get-json-success-p (json-read-from-string
                                           (webqq-curl-post (format webqq-get-group-info-url
                                                                    (int-to-string (truncate (float-time)))
                                                                    (int-to-string gcode)
                                                                    (webqq-session-value "vfwebqq"))
                                                            nil
                                                            webqq-http-header webqq-cookies 'utf-8 nil )))))
    (webqq-dispaly-info-on-base-buffer (format "get group  %s member  successed! \n  \n" gcode))
    (emacs-log "get group member for %s " gcode)
    (webqq-group-info-merge ret)))

(defun webqq-group-info-check-finish ()
  (do ((i 0 (+ 1 i)))
      ((or (= i (length webqq-grouplist))
           (null (assoc 'member (aref webqq-grouplist i))))
       (when (= i (length webqq-grouplist))
         (coerce webqq-grouplist 'list)))))

(defun webqq-group-gid (nickname)
  (cdr (assoc 'gid (car (cl-member-if #'(lambda (x) (string= nickname (cdr (assoc 'name x)))) webqq-grouplist)))))




(defun webqq-group-member-nickname (gid uin)
  (setq memb (car (cl-member uin
                             (cdr (assoc 'member
                                         (car
                                          (cl-member gid webqq-grouplist
                                                     :test #'= :key #'(lambda (x) (cdr (assoc 'gid x)))))))
                             :test #'= :key #'(lambda (x) (cdr (assoc 'uin x))))))
  (values (cdr (assoc 'nick memb))))



(defun webqq-group-name (gid)
  (cdr (assoc 'name
              (car (cl-member gid webqq-grouplist
                              :test #'= :key #'(lambda (x) (cdr (assoc 'gid x))))))))


;;;; This part is ALL the buffer command


(defconst webqq-base-buffer-name "webqq" "webqq base buffer name")
(defconst webqq-friend-list-buffer-name "*[webqq-friend-list]*" "webqq friend list buffer name")
(defconst webqq-group-list-buffer-name "*[webqq-group-list]*" )



(defconst webqq-welcome-content "Welcome to use Emacs WebQQ!\n\n这个东西目前还只是一个小玩具,欢迎大家提供意见和提交代码\n\n")


(defun webqq-show-group-list ()
  "This function is used to display the webqq group list in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create  webqq-group-list-buffer-name)
    (webqq-mode)
    (switch-to-buffer (current-buffer))
    (erase-buffer)
    (insert "#######################")
    (dolist (group webqq-grouplist)
      (insert "\n" (cdr (assoc 'name group)))
      (make-text-button (+ 1 (- (buffer-size) (length (cdr (assoc 'name group))))) (+ 1 (buffer-size))
                        'action (lambda (x) (webqq-create-send-msg-to-group-buffer  (button-label x)))
                        'follow-link t))))


(defun webqq-show-friend-list ()
  "This function is used to display the webqq friend list data in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create webqq-friend-list-buffer-name)
    (webqq-mode)
    (erase-buffer)
    (insert "#####################################")
    (dolist (category webqq-friend-list)
      (insert "\n" (cdr (assoc 'name category)) )
      (make-text-button (+ 1 (- (buffer-size) (length (cdr (assoc 'name category))))) (+ 1 (buffer-size))
                        'action (lambda (x) (webqq-toggle-friend-list-in-category (button-label x)))
                        'follow-link t)
      (insert "(" (int-to-string (length (cdr (assoc 'users category)))) ")"))
;    (delete-region (- (buffer-size) 1) (buffer-size))
    (emacs-log "webqq get friendlist buffer")
    (switch-to-buffer (current-buffer))))


(defun webqq-toggle-friend-list-in-category (category)
  (if (webqq-sublist-display-p)
      (webqq-hidden-friend-list-in-category)
    (webqq-show-friend-list-in-category category)))

(defun webqq-show-friend-list-in-category (category)
  (let ((lst (cdr (assoc 'users (car (member-if #'(lambda (x) (string= (cdr (assoc 'name x)) category)) webqq-friend-list))))))
    (with-current-buffer (get-buffer webqq-friend-list-buffer-name)
      (save-excursion
        (end-of-line)
        (dolist (friend lst)
          (insert "\n  " (cdr (assoc 'nick friend)) )
          (make-text-button (- (point) (length (cdr (assoc 'nick friend)))) (point)
                            'action (lambda (x) (webqq-create-send-msg-to-friend-buffer (button-label x)))))))))



(defun webqq-create-send-msg-to-friend-buffer (nickname)
  (with-current-buffer (get-buffer-create (concat "**" nickname "**"))
    (webqq-mode)
    (switch-to-buffer (current-buffer))
    (webqq-create-send-message-to-friend-buffer)))


(defun webqq-create-send-msg-to-group-buffer (groupname)
  (with-current-buffer (get-buffer-create (concat "***" groupname "***"))
    (webqq-mode)
    (switch-to-buffer (current-buffer))
    (webqq-create-send-message-to-group-buffer)))



(defun webqq-sublist-display-p ()
  (when (not (= (line-end-position) (+ 1 (buffer-size))))
    (let ((has-next-line (and (null (next-line) )
                              (null (beginning-of-line)))))
      (if has-next-line
          (let ((ret (not (button-at (point)))))
            (previous-line)
            ret)))))


(defun webqq-hidden-friend-list-in-category ()
  (interactive)
  (while (webqq-sublist-display-p)
    (next-line)
    (delete-region (- (line-beginning-position) 1) (line-end-position)))
  (beginning-of-line))



(defun webqq-dispaly-info-on-base-buffer (msg)
  (with-current-buffer (get-buffer webqq-base-buffer-name)
    (end-of-buffer)
    (insert msg )
    (insert "\n")))

(defvar webqq-group-list-finish nil)
(defun webqq-dispatch-msg (msg)
  (progn
    (string-match "retcode.*?:\\(\\w+?\\),"  msg)
    (setq retcode (match-string 1 msg)))
  (if (= 0 (string-to-number retcode))
      (progn
        (emacs-log "receive message is : %s" msg)
        (string-match "poll_type.*?:\"\\(.*?\\)\"," msg)
        (setq type  (match-string 1 msg))
        (cond ((string= type "message") (webqq-append-message-to-friend-buffer msg) )
              ((string= type "group_message") (webqq-append-message-to-group-buffer msg))
              (t (emacs-log "can not support this message : %s" msg)))
        )
    (emacs-log "Empty message"))
  (webqq-message-buffer-refresh))



(defun webqq-append-message-to-friend-buffer (message)
  (emacs-log "webqq-append message to msg buffer \n" message )
  (when (> (length message) 0)
    (let ((uin)
          (nickname)
          (data))
      (progn
        (string-match "\"content\":\\(.*?\\)}}]}" message)
        (setq m  (aref (json-read-from-string (match-string 1 message)) 1))
        (string-match "from_uin.*?:\\(\\w+?\\)," message)
        (setq uin (match-string 1 message))
        (setq nickname (cdr (assoc (string-to-int uin) webqq-friend-list-uin-nickname-plist)))
        (setq data (concat "\n========= " nickname " [" uin "]=========\n" (cond ((stringp m) m)
                                                                                 ((vectorp m) (json-encode m))) ""))
        (emacs-log "This message is : %s" data))
      (when nickname
        (with-current-buffer (get-buffer-create (concat "**" nickname "**"))
          (webqq-mode)
          (end-of-buffer)
          (insert data)
          ;;I do not know that why this message will be two lines in minibuffer
          (message "A message come from %s ." nickname))))))

(defun webqq-show-remain-need-to-get-group-info ()
  (interactive)
  (let ((info (webqq-remain-need-to-get-group-info)))
    (if info
      (with-current-buffer (get-buffer-create "*remain-group-info*")
        (pp-display-expression info  (buffer-name (current-buffer)))
        (webqq-mode)
        (switch-to-buffer (current-buffer)))
      (let ((buffer (get-buffer  "*remain-group-info*")))
        (when buffer
          (kill-buffer buffer))))))



(defun webqq-reget-group-info-with-select ()
  (interactive)
  (let* ((gid (string-to-number (current-word)))
         (mlist (webqq-get-group-info (webqq-find-group-code gid))))
    (when mlist
      (webqq-update-group-member gid mlist)
      (webqq-show-remain-need-to-get-group-info))))

(defun webqq-append-message-to-group-buffer (msg)
  (emacs-log "webqq-append message to group buffer \n" msg)
  (when (> (length msg) 0)
    (let ((uin)
          (nickname)
          (data))
      (progn
        (string-match "\"content\":\\(.*?\\)}}]}" msg)
        (setq m  (aref (json-read-from-string (match-string 1 msg)) 1))
        (string-match "from_uin.*?:\\(\\w+?\\)," msg)
        (setq uin (match-string 1 msg))
        (string-match "send_uin\":\\(.*?\\)," msg)
        (setq member-uin (match-string 1 msg))
        (setq nickname (webqq-group-name (string-to-number uin)))
        (multiple-value-bind (member-nickname) (webqq-group-member-nickname (string-to-number uin) (string-to-number member-uin))
          (setq data (concat "\n========= " nickname " [" uin "]=========\n"
                             member-nickname " say: "(cond ((stringp m) m)
                                                           ((vectorp m) (json-encode m)))
                             ""))
          (emacs-log "This message is : %s" data)
          (when nickname
            (with-current-buffer (get-buffer-create (concat "***" nickname "***"))
              (webqq-mode)
              (end-of-buffer)
              (insert data)
              ;;I do not know that why this message will be two lines in minibuffer
              (message "A message come from group %s ." nickname))))))))


(defun webqq-create-send-message-to-friend-buffer ()
  (interactive)
  (let* ((info-buffer (current-buffer))
         (uid (cdr (assoc (substring (buffer-name info-buffer) 2 (-  (length (buffer-name info-buffer)) 2))
                          webqq-friend-list-nickname-uin-plist ))))
    (with-current-buffer (get-buffer-create (concat "*[" (int-to-string uid) "]*"))
      (webqq-mode)
      (let ((c (current-buffer)))
        (split-window-vertically -10)
        (set-window-buffer (next-window) c)
        (other-window 1)))))

(defun webqq-create-send-message-to-group-buffer ()
  (interactive)
  (let* ((info-buffer (current-buffer))
         (gid (webqq-group-gid (substring (buffer-name info-buffer) 3 (- (length (buffer-name info-buffer)) 3)) ) ))
    (with-current-buffer (get-buffer-create (concat "*[[" (int-to-string gid) "]]*"))
      (webqq-mode)
      (let ((c (current-buffer)))
        (split-window-vertically -10)
        (set-window-buffer (next-window) c)
        (other-window 1)))))

(defun webqq-kill-close-window ()
  (interactive)
  (kill-buffer)
  (condition-case err
      (delete-window)
    (error nil)))




(defun webqq-buffer-send-friend-message ()
  (interactive)
  (let* ((buffer-name (buffer-name (current-buffer)))
         (uid (string-to-number (substring buffer-name 2 (- (length buffer-name) 2))))
         (nickname (cdr (assoc uid webqq-friend-list-uin-nickname-plist)))
         (chat-buffer (get-buffer (concat "**" nickname "**")))
         (msg (buffer-substring-no-properties 1 (+ 1 (buffer-size)))))
  (with-current-buffer chat-buffer
    (end-of-buffer)
    (webqq-send-message-to-user uid msg)
    (insert (concat "\n============ I said: ============\n" msg))
    )
  (webqq-kill-close-window)))






(defun webqq-buffer-send-group-message ()
  (interactive)
  (let* ((buffer-name (buffer-name (current-buffer)))
         (gid (string-to-number (substring buffer-name 3 (- (length buffer-name) 3))))
         (nickname (webqq-group-name gid))
         (chat-buffer (get-buffer (concat "***" nickname "***")))
         (msg (buffer-substring-no-properties 1 (+ 1 (buffer-size)))))
  (with-current-buffer chat-buffer
    (webqq-send-message-to-group gid msg)
    (insert (concat "\n============ I said: ============\n" msg)))
  (webqq-kill-close-window)))



(defun webqq-fetch-msg-poll ()
  (let* ((json-send (json-encode (list :clientid (int-to-string (webqq-session-value "clientid"))
                                       :psessionid (webqq-session-value "sessionid")
                                       :key 0
                                       :ids [])))
         (cmd (webqq-curl-shell-command webqq-poll-msg-url
                                        (list
                                         (cons "clientid" (int-to-string (webqq-session-value "clientid")))
                                         (cons "psessionid" (webqq-session-value "sessionid"))
                                         (cons "r" json-send))
                                        webqq-http-header webqq-cookies 'utf-8 nil)))

    (async-start
     ;; notice the backquote!
     `(lambda ()
        (set  'cmd  ,cmd)
        (concat "" (shell-command-to-string cmd)))

     (lambda (result)
       (webqq-dispatch-msg  result)))))


(defun webqq-message-buffer-refresh ()
  (emacs-log "=============refresh begin=================")
  (webqq-fetch-msg-poll))


(defun webqq-start ()
  (interactive)
  (if (not (file-exists-p webqq-face-image-cache-path))
      (mkdir webqq-face-image-cache-path t))
  (if (not (file-exists-p webqq-log-file-name))
      (shell-command (concat "touch " webqq-log-file-name)))
  (with-current-buffer (get-buffer-create webqq-base-buffer-name)
    (unless (eq major-mode 'webqq-mode)
      (webqq-mode)
      (erase-buffer)
      (insert webqq-welcome-content)
      (switch-to-buffer (current-buffer))
      (when (and (webqq-login2 (get-buffer webqq-base-buffer-name))
                 (webqq-init-friend-list)
                 (webqq-init-group-list))
        (webqq-dispaly-info-on-base-buffer "get message  now ......")
        (webqq-message-buffer-refresh)
        ))))

(defun webqq-restart ()
  (interactive)
  (kill-buffer webqq-base-buffer-name)
  (kill-buffer webqq-friendlist-buffer-name)
  (webqq-start))


(defun webqq-send-message ()
  (interactive)
  (let ((buffer-name (buffer-name (current-buffer))))
    (if (string= "*[["  (substring  buffer-name 0 3 ))
        (webqq-buffer-send-group-message)
      (webqq-buffer-send-friend-message))))





(defvar webqq-mode-map nil
  "Keymap for webqq mode")

(setq webqq-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map "L" 'webqq-start)
        (define-key map "F" 'webqq-create-friendlist-buffer)
        (define-key map "\C-c\C-c" 'webqq-send-message)
        (define-key map "\C-c\C-f" 'webqq-create-send-message-to-friend-buffer)
        (define-key map "\C-c\C-g" 'webqq-create-send-message-to-group-buffer)
        (define-key map "\C-c\C-r" 'webqq-reget-group-info-with-select)
        (define-key map "\C-c\C-n" 'webqq-show-remain-need-to-get-group-info)



        map))

(define-derived-mode webqq-mode fundamental-mode "webQQ-Mode"
  "Major mode for displaing webqq"
  (use-local-map webqq-mode-map))











(provide 'webqq-emacs)
