;;; lain.el --- Cool personal assistant

;; Author: Sebastian Gonzalez <sebastian2zen@gmail.com>
;; Keywords: logbook org
;; Version: 0.1
;; Package: lain

;; Maintainer: sebastian2zen@gmail.com

;;; Code:

(require 'elnode)


;;; This functions should be defined on the emacs file, i didn't added to lain because they have a more general use

(defalias 'lain-look-and-feel 'high-bright-look-and-feel)

(defvar lain-agenda-buffer-name "TASKS.html")
;;(defvar lain-agenda-buffer-name "*Org Agenda*")

(defun lain-extract-periodic-scheduling ()
  (let ((scheduled (org-entry-get (point) "SCHEDULED")))
    (if (string-match "<.*\\(\\+.+\\)>" scheduled 0)
        (match-string 1 scheduled))))

(defun lain-extract-date-scheduling ()
  (let ((scheduled (org-entry-get (point) "SCHEDULED")))
    (if (string-match "<\\(....-..-..\\)[[:blank:]]+.*>" scheduled 0)
        (match-string 1 scheduled))))

(defun org-log-note-update (state date hour newstate)
  (re-search-forward (org-item-beginning-re) nil t)
  (let ((regex (concat "\\(.+\\)" state "\\(.+\\)\\[[0-9]+-[0-9]+-[0-9]+ \\(.+\\) [0-9]+:[0-9]+\\]")))
    (re-search-forward regex nil t)
    (if newstate
        (setq state newstate))
    (replace-match (concat (match-string 1) state (match-string 2) "[" date " " (match-string 3) " " hour "]"))))

(defun lain-goto-to-task (text)
;; i don't understand why this doesn't work with ert
 (switch-to-buffer (get-buffer-create lain-agenda-buffer-name))
 (beginning-of-buffer)
 (re-search-forward text)
 (org-agenda-switch-to)
 (org-narrow-to-subtree))

(defun lain-create-agenda-view (text)
  (lain-goto-to-task text)
  (switch-to-buffer (current-buffer))
  (org-agenda-write-tmp "/tmp/org/ORG-TASK.html"))

(defun lain-destroy-agenda-view (text)
  (switch-to-buffer (get-buffer-create "TASKS.html"))
  (message (buffer-name (current-buffer)))
  (beginning-of-buffer)
  (re-search-forward text)
  (org-agenda-switch-to)
  (widen)
  (delete-file "/tmp/org/ORG-TASK.html"))

(defun lain-reschedule-task (text date)
  (lain-goto-to-task text)
  (let ((repeat (lain-extract-periodic-scheduling)))
        (org-schedule '(4))
        (if repeat 
            (org-schedule '() (concat date "" repeat))
          (org-schedule '() date)))
  (save-buffer)
  (lain-create-agenda-view text))

(defun org-add-log-note-tmp (&optional _purpose)
  "Pop up a window for taking a note, and add this note later."
  (remove-hook 'post-command-hook 'org-add-log-note)
  (setq org-log-note-window-configuration (current-window-configuration))
  (delete-other-windows)
  (move-marker org-log-note-return-to (point))
  (pop-to-buffer-same-window (marker-buffer org-log-note-marker))
  (goto-char org-log-note-marker)
  (org-switch-to-buffer-other-window "*Org Note*")
  (erase-buffer)
  (if (memq org-log-note-how '(time state))
      (org-store-log-note)
    (let ((org-inhibit-startup t)) (org-mode))
    (insert (format "# Insert note for %s.
# Finish with C-c C-c, or cancel with C-c C-k.\n\n"
		    (cond
		     ((eq org-log-note-purpose 'clock-out) "stopped clock")
		     ((eq org-log-note-purpose 'done)  "closed todo item")
		     ((eq org-log-note-purpose 'state)
		      (format "state change from \"%s\" to \"%s\""
			      (or org-log-note-previous-state "")
			      (or org-log-note-state "")))
		     ((eq org-log-note-purpose 'reschedule)
		      "rescheduling")
		     ((eq org-log-note-purpose 'delschedule)
		      "no longer scheduled")
		     ((eq org-log-note-purpose 'redeadline)
		      "changing deadline")
		     ((eq org-log-note-purpose 'deldeadline)
		      "removing deadline")
		     ((eq org-log-note-purpose 'refile)
		      "refiling")
		     ((eq org-log-note-purpose 'note)
		      "this entry")
		     (t (error "This should not happen")))))
    (when org-log-note-extra (insert org-log-note-extra))
    (setq-local org-finish-function 'org-store-log-note)
    (run-hooks 'org-log-buffer-setup-hook)))

(defun lain-update-task(text date time link state)
  (lain-goto-to-task text)
  (org-todo 'right)
  ;; this is suppsed to be executed as a hook but it is not running when the command is invoked non interactively.
;  (set-marker org-log-note-marker (point))   
;;  (org-add-log-note)
  (org-narrow-to-subtree)
  (switch-to-buffer (current-buffer))
  (org-log-note-update "DONE" date time
                       ;; the if evaluates true executed either the field is empty or not
                       (if link
                           (if (not (string-empty-p link))
                               (concat "[[" link "]" "[" state "]]"))))
  (save-buffer)
  (lain-create-agenda-view text))

(defun lain-done-task (text date time link)
  (lain-update-task text date time link "DONE"))

(defun lain-itried-task (text date time link)
  (lain-update-task text date time link "ITRIED"))

(defun lain-canceled-task (text date time link)
  (lain-update-task text date time link "CANCELED"))

(defun lain-kill-named-buffers(org-buffer)
    (if (string-match ".*PROJECT.org" org-buffer 0)
        (kill-buffer x))
    (if (string-match ".*PERIODIC.org" org-buffer 0)
        (kill-buffer x)))

(defun lain-kill-org-buffers()
  (dolist (x (buffer-list))
    (setq org-buffer (buffer-name x))
    ;; there are buffers without name and the string match cannot handle this case
    (if org-buffer
	(lain-kill-named-buffers org-buffer))))

(defun org-agenda-write-tmp (file &optional open nosettings agenda-bufname)
  (org-let (if nosettings nil org-agenda-exporter-settings)
    '(save-excursion
       (save-window-excursion
	 (let ((bs (copy-sequence (buffer-string))) beg content)
	   (with-temp-buffer
	     (rename-buffer org-agenda-write-buffer-name t)
	     (set-buffer-modified-p nil)
	     (insert bs)
	     (org-agenda-remove-marked-text 'org-filtered)
	     (run-hooks 'org-agenda-before-write-hook)
	     (cond
	      ((string-match "\\.html?\\'" file)
	       (require 'htmlize)
	       (set-buffer (htmlize-buffer (current-buffer)))
	       (when org-agenda-export-html-style
		 (goto-char (point-min))
		      (kill-region (- (search-forward "<style") 6)
			      (search-forward "</style>"))
		      (insert org-agenda-export-html-style))
	       (write-file file)
	       (kill-buffer (current-buffer))
	       (message "HTML written to %s" file))
	      (t
	       (let ((bs (buffer-string)))
		 (find-file file)
		 (erase-buffer)
		 (insert bs)
		 (save-buffer 0)
		 (kill-buffer (current-buffer))
		 (message "Plain text written to %s" file))))))))))


(setq elnode-webserver-docroot "/tmp/org/")

(defvar 
   my-app-routes 
   '(("^.+//lain/\\(.*\\)" . task-handler)
     ("^.+//done/\\(.*\\)" . periodic-done-handler)
     ("^.+//canceled/\\(.*\\)" . periodic-canceled-handler)
     ("^.+//itried/\\(.*\\)" . periodic-itried-handler)
     ("^.+//reschedule/\\(.*\\)" . task-reschedule-handler)
     ("^.+//calendar/\\(.*\\)" . calendar-view)
     ("^.+//periodic/\\(.*\\)" . periodic-view)
     ("^.+//todo/\\(.*\\)" . todo-view)
     ("^.+//chores/\\(.*\\)" . chores-view)
     ("^.+//signal/\\(.*\\)" . signal-view)
     ("^.+//base.html" . cookie-handler)
     ("^.*//\\(.*\\)" . elnode-webserver)))

(setq org-html-head-extra "<meta http-equiv=\"Content-Security-Policy\" content=\"default-src * 'unsafe-inline' 'unsafe-eval'; script-src * 'unsafe-inline' 'unsafe-eval'; connect-src * 'unsafe-inline'; img-src * data: blob: 'unsafe-inline'; frame-src *; style-src * 'unsafe-inline';\">
<script src=\"http://code.jquery.com/jquery-latest.min.js\" type=\"text/javascript\"></script>
<script type=\"text/javascript\">
    $(document).ready(function(){
        $(\"span.org-todo\").click(function(el){
           var text = $(el.target)[0].nextSibling.textContent;
           text = text.replace(/\\[.+\\]/g,'').replace(/^\\s+/g,'').replace(/\\?/g,'\\\\?').replace(/\\+/g,'\\\\+').replace(\"\\n\", \"\").replace(/(.+)\\s+PROJECT:\\s+/, \"$1\").trim();
           text = encodeURIComponent(text);
           $.ajax({
               type: \"GET\", 
               url: \"/lain/?text=\" + text, 
               headers: {
                 \"apikey\": \"mykey\"
               }
             }).done(function(){
               window.location = \"ORG-TASK.html\";
           });
        });

        $(\".org-agenda-calendar-event,.org-scheduled-today,.org-scheduled,.org-agenda-done,.org-scheduled-previously,.org-warning\").click(function(el){
           var text = $(el.target).text().replace(/\\[.+\\]/g,'').replace(/^\\s+/g,'').replace(/\\?/g,'\\\\?');
           text = encodeURIComponent(text);
           $.ajax({
               type: \"GET\", 
               url: \"/lain/?text=\" + text, 
               headers: {
                 \"apikey\": \"mykey\"
               }
             }).done(function(){
               window.location = \"ORG-TASK.html\";
           });
        });

        var setTaskTimeStamp = function(){
            var x = new Date()
            var y = [x.getHours(), x.getMinutes()]
            var z = y.join(\":\")
            $(\"input.time\").val(z)
             
            var x = new Date()
            var y = [x.getUTCFullYear(), x.getUTCMonth() < 10 ? \"0\" + (x.getUTCMonth() + 1) : (x.getUTCMonth() + 1), x.getUTCDate()]
            var z = y.join(\"-\")
            $(\"input.date\").val(z)
        }

        var getTaskTimeStamp = function(){
            return {
               \"date\":  $(\"input.date\").val(),
               \"time\":  $(\"input.time\").val()
            }
        }

        var e = window.location.href.split(\"/\");
        if(e[e.length - 1] == \"ORG-TASK.html\"){
             var x = $(\"body\").html();
             $(\"body\").html(\"<button class=\\\"done\\\">Check task</button><button class=\\\"itried\\\">I tried</button><button class=\\\"canceled\\\">Cancel task</button><button class=\\\"reschedule\\\">Reschedule task</button><input type=\\\"date\\\" class=\\\"date\\\"/><input type=\\\"time\\\" class=\\\"time\\\"/><br><input type=\\\"text\\\" class=\\\"link\\\"/>\" + x );
             setTaskTimeStamp();
        }
         
        $(\"button.done\").click(function(){
            var matches = $(\"pre\").text().match(/^\\*+\\s+PERIODIC\\s+(.+)\\n/);
            var text = matches[1].replace(/\\[.+\\]/g,'').replace(/^\\s+/g,'').replace(/\\?/g,'\\\\?');
            text = encodeURIComponent(text);
         
            var timestamp = getTaskTimeStamp();

            var url = \"text=\" + text; 
            url = url + \"&date=\" + timestamp.date; 
            url = url + \"&time=\" + timestamp.time; 
            url = url + \"&link=\" + $(\"input.link\").val(); 

            $.ajax({
                type: \"GET\", 
                url: \"/done/?\" + url, 
                headers: {
                  \"apikey\": \"mykey\"
                }
            }).done(function(){
               alert(\"state updated\");
               window.location = \"ORG-TASK.html\";
            });
        });

        $(\"button.reschedule\").click(function(){
            var matches = $(\"pre\").text().match(/^\\*+\\s+(PERIODIC|TODO|IN_PROGRESS|CHECK|LATER)\\s+(.+)\\n/);
            var text = matches[2].replace(/\\[.+\\]/g,'').replace(/^\\s+/g,'').replace(/\\?/g,'\\\\?');
            text = encodeURIComponent(text);
         
            var timestamp = getTaskTimeStamp();

            var url = \"text=\" + text; 
            url = url + \"&date=\" + timestamp.date; 

            $.ajax({
                type: \"GET\", 
                url: \"/reschedule/?\" + url, 
                headers: {
                  \"apikey\": \"mykey\"
                }
            }).done(function(){
               alert(\"scheduled updated\");
               window.location = \"ORG-TASK.html\";
            });
        });

        $(\"button.itried\").click(function(){
            var matches = $(\"pre\").text().match(/^\\*+\\s+PERIODIC\\s+(.+)\\n/);
            var text = matches[1].replace(/\\[.+\\]/g,'').replace(/^\\s+/g,'').replace(/\\?/g,'\\\\?');
            text = encodeURIComponent(text);
         
            var timestamp = getTaskTimeStamp();

            var url = \"text=\" + text; 
            url = url + \"&date=\" + timestamp.date; 
            url = url + \"&time=\" + timestamp.time;
            url = url + \"&link=\" + $(\"input.link\").val(); 

            $.ajax({
                type: \"GET\", 
                url: \"/itried/?\" + url, 
                headers: {
                  \"apikey\": \"mykey\"
                }
            }).done(function(){
               alert(\"state updated\");
               window.location = \"ORG-TASK.html\";
            });
        });

        $(\"button.canceled\").click(function(){
            var matches = $(\"pre\").text().match(/^\\*+\\s+PERIODIC\\s+(.+)\\n/);
            var text = matches[1].replace(/\\[.+\\]/g,'').replace(/^\\s+/g,'').replace(/\\?/g,'\\\\?');
            text = encodeURIComponent(text);
         
            var timestamp = getTaskTimeStamp();

            var url = \"text=\" + text; 
            url = url + \"&date=\" + timestamp.date; 
            url = url + \"&time=\" + timestamp.time; 
            url = url + \"&link=\" + $(\"input.link\").val();

            $.ajax({
                type: \"GET\", 
                url: \"/canceled/?\" + url, 
                headers: {
                  \"apikey\": \"mykey\"
                }
            }).done(function(){
               alert(\"state updated\");
               window.location = \"ORG-TASK.html\";
            });
        });

        var replaceLinks = function(){
            var a = $(\"a\").filter(function(i, e){return e.href.match(/small\\/SMALL\\/images/g)})
            var b = a.map(function(e, x){m = x.href.match(/small\\/SMALL\\/images\\/(.+)/); return \"/images/\" + m[1]});
            for(i = 0; i < a.length; i++){
                a[i].href = b[i] 			
            }
        }
         
        replaceLinks();


    });
</script>")

(setq lain-cookie-html "<html>
<head>
<meta http-equiv=\"Content-Security-Policy\" content=\"default-src * 'unsafe-inline' 'unsafe-eval'; script-src * 'unsafe-inline' 'unsafe-eval'; connect-src * 'unsafe-inline'; img-src * data: blob: 'unsafe-inline'; frame-src *; style-src * 'unsafe-inline';\">
<script src=\"http://code.jquery.com/jquery-latest.min.js\" type=\"text/javascript\"></script>
<script type=\"text/javascript\">
    $(document).ready(function(){
         $(\"input:text\").change(function(a){
             document.cookie = \"Authorization=\" + $(a.target).val() +  \"; expires=Mon, 3 Aug 2020 20:47:11 UTC; path=/;\"
         });
 
         $(\"input:submit\").click(function(a){
             window.location = \"/calendar/\";
         });
    });
</script>
</head>
<body>
  Insert the cookie:<br>
  <input type=\"text\" name=\"firstname\" value=\"Mickey\">
  <br>
  <br><br>
  <input type=\"submit\" value=\"Submit\">
</body>
</html>")

(setq htmlize-head-tags org-html-head-extra)

(defun cookie-handler (httpcon)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon lain-cookie-html))

(defun org-base-view (lain-org-files view-type)
  (lain-look-and-feel)
  (lain-kill-org-buffers)
  (dolist (file lain-org-files)
      (find-file file))
  (let ((org-agenda-files lain-org-files)
        (org-agenda-buffer-tmp-name lain-agenda-buffer-name))
    (funcall view-type)))

(defun base-view(name agendas-list agenda-type)
  (org-base-view agendas-list agenda-type)
  (save-excursion
    (set-buffer (get-buffer-create lain-agenda-buffer-name))
    (org-agenda-write (concat "/tmp/org/" name ".html"))))

;; i might needed when working on the android client
(defun periodic-view (httpcon)
  (base-view "PERIODIC" '("/small/SMALL/PERIODIC.org") 'org-agenda-list) 
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (concat "<html><a href=" "/PERIODIC.html" ">Periodic View</a></html>")))

(defun calendar-view (httpcon)
  (org-base-view '("/small/SMALL/WORK/PROJECT.org" "/small/SMALL/THINGS/PROJECT.org" "/small/SMALL/SKILLS/PROJECT.org"
		   "/small/SMALL/WORK/notes.org" "/small/SMALL/THINGS/MAINTAINANCE/chores/PROJECT.org")
		 'org-agenda-list)
  (save-excursion
    (set-buffer (get-buffer-create "TASKS.html"))
    (org-agenda-write "/tmp/org/VIEW.html"))
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (concat "<html><a href=" "/VIEW.html" ">Agenda View</a></html>")))

(defun todo-view (httpcon)
  (org-base-view '("/small/SMALL/WORK/PROJECT.org" "/small/SMALL/THINGS/PROJECT.org" "/small/SMALL/SKILLS/PROJECT.org" "/small/SMALL/WORK/notes.org") 'org-todo-list)
  (save-excursion
    (set-buffer (get-buffer-create "TASKS.html" ))
    (org-agenda-write "/tmp/org/TODO.html" nil nil"TASKS.html"))
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (concat "<html><a href=" "/TODO.html" ">Todo View</a></html>")))

(defun chores-view (httpcon)
  (org-base-view '("/small/SMALL/THINGS/MAINTAINANCE/chores/PROJECT.org") 'org-todo-list)
  (save-excursion
    (set-buffer (get-buffer-create "TASKS.html"))
    (org-agenda-write "/tmp/org/CHORES.html" nil nil"TASKS.html"))
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (concat "<html><a href=" "/CHORES.html" ">Chores View</a></html>")))

(defun signal-view (httpcon)
  (lain-look-and-feel)
  (find-file "/small/SMALL/SIGNAL.org")
  (org-agenda-write-tmp "/tmp/org/SIGNAL.html")
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (elnode-http-return httpcon (concat "<html><a href=" "/SIGNAL.html" ">Signal View</a></html>")))

(defun task-reschedule-handler (httpcon)
  (lain-look-and-feel)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (lain-reschedule-task (elnode-http-param httpcon "text") (elnode-http-param httpcon "date"))
  (elnode-http-return httpcon (concat "<html><b>" "</b></html>")))

(defun periodic-itried-handler (httpcon)
  (lain-look-and-feel)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (lain-itried-task (elnode-http-param httpcon "text") (elnode-http-param httpcon "date") (elnode-http-param httpcon "time") (elnode-http-param httpcon "link"))
  (elnode-http-return httpcon (concat "<html><b>" "</b></html>")))

(defun periodic-done-handler (httpcon)
  (lain-look-and-feel)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (lain-done-task (elnode-http-param httpcon "text") (elnode-http-param httpcon "date") (elnode-http-param httpcon "time") (elnode-http-param httpcon "link"))
  (elnode-http-return httpcon (concat "<html><b>" "</b></html>")))

(defun periodic-canceled-handler (httpcon)
  (lain-look-and-feel)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (lain-canceled-task (elnode-http-param httpcon "text") (elnode-http-param httpcon "date") (elnode-http-param httpcon "time") (elnode-http-param httpcon "link"))
  (elnode-http-return httpcon (concat "<html><b>" "</b></html>")))

(defun task-handler (httpcon)
  (lain-look-and-feel)
  (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  (lain-create-agenda-view (elnode-http-param httpcon "text"))
  (elnode-http-return httpcon (concat "<html><b>" "</b></html>")))

(defun periodic-agenda ()
  (interactive)
  (org-periodic-view))

(defun calendar-agenda ()
  (interactive)
  (org-calendar-view))

(defun todo-agenda ()
  (interactive)
  (org-todo-view))
 
(defun root-handler (httpcon)
  (elnode-hostpath-dispatcher httpcon my-app-routes))
 
(elnode-start 'root-handler :port 8080 :host "0.0.0.0")

(provide 'lain)
;;; lain.el ends here
