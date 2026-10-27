;;; gitlab-org.el --- GitLab issues in org-mode via glab -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Oleksii Koval

;; Author: Oleksii Koval <alex@koval.kharkov.ua>
;; URL: https://github.com/avkoval/emacs-org-mode-gitlab
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, outlines
;; SPDX-License-Identifier: MIT

;; This file is tangled from gitlab-org.org; edit that file instead.

;;; Commentary:

;; Browse GitLab projects and issues, keep issues as org tasks (with a
;; GITLAB_ISSUE property), read an issue with its comments as an org
;; buffer, and submit org-clock time to GitLab time tracking.  All
;; requests go through `glab api', reusing glab's login.  Set
;; `gitlab-org-host' to the instance's hostname.

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'parse-time)
(require 'thingatpt)
(require 'org)
(require 'org-clock)

(defgroup gitlab-org nil
  "GitLab issues in Org."
  :group 'tools
  :prefix "gitlab-org-")

(defcustom gitlab-org-host nil
  "GitLab hostname without scheme, e.g. \"gitlab.example.com\".
Must be a host `glab' is logged in to (see `glab auth status')."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'gitlab-org)

(defcustom gitlab-org-glab-program "glab"
  "Name or path of the `glab' executable."
  :type 'string
  :group 'gitlab-org)

(defcustom gitlab-org-max-items 500
  "Stop paginating a listing once this many items have been fetched."
  :type 'integer
  :group 'gitlab-org)

(defconst gitlab-org--property "GITLAB_ISSUE"
  "Org property holding the issue reference, e.g. \"group/project#12\".")

(defun gitlab-org--host ()
  "Return `gitlab-org-host', or signal a user error when it is unset."
  (or gitlab-org-host
      (user-error "Set `gitlab-org-host' first (e.g. in your private init file)")))

(defun gitlab-org--base-url ()
  "Return the web base URL of the GitLab instance."
  (format "https://%s" (gitlab-org--host)))

(defun gitlab-org--parse-json (string)
  "Parse JSON STRING into alists and vectors, or nil when STRING is blank."
  (unless (string-blank-p string)
    (json-parse-string string
                       :object-type 'alist
                       :array-type 'array
                       :null-object nil
                       :false-object nil)))

(defun gitlab-org--glab-api (args &optional input)
  "Run `glab api ARGS' against `gitlab-org-host' and return the parsed JSON.
INPUT, when non-nil, is a string sent to glab on stdin."
  (let ((program (or (executable-find gitlab-org-glab-program)
                     (user-error "Cannot find `%s'; see `gitlab-org-glab-program'"
                                 gitlab-org-glab-program)))
        (args (append (list "api" "--hostname" (gitlab-org--host)) args))
        (err-file (make-temp-file "gitlab-org-stderr"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (unwind-protect
        (with-temp-buffer
          (let ((status
                 (if input
                     (let ((out (current-buffer)))
                       (with-temp-buffer
                         (insert input)
                         (apply #'call-process-region (point-min) (point-max)
                                program nil (list out err-file) nil args)))
                   (apply #'call-process program nil (list t err-file) nil args))))
            (unless (eql status 0)
              (error "GitLab: %s"
                     (string-trim (with-temp-buffer
                                    (insert-file-contents err-file)
                                    (buffer-string)))))
            (gitlab-org--parse-json (buffer-string))))
      (delete-file err-file))))

(defun gitlab-org--get (endpoint)
  "GET REST ENDPOINT (relative to /api/v4, e.g. \"/user\")."
  (gitlab-org--glab-api (list endpoint)))

(defun gitlab-org--get-all (endpoint)
  "GET every page of REST ENDPOINT and return the items as a list.
Stops after `gitlab-org-max-items' items."
  (let ((sep (if (string-search "?" endpoint) "&" "?"))
        (per-page 100)
        (page 1)
        items done)
    (while (not done)
      (let ((batch (gitlab-org--get (format "%s%sper_page=%d&page=%d"
                                            endpoint sep per-page page))))
        (setq items (nconc items (append batch nil)))
        (if (or (< (length batch) per-page)
                (>= (length items) gitlab-org-max-items))
            (setq done t)
          (setq page (1+ page)))))
    items))

(defun gitlab-org--graphql (query &optional variables)
  "Run GraphQL QUERY with VARIABLES (an alist) and return its `data'."
  (let* ((body (json-encode `((query . ,query) (variables . ,variables))))
         (response (gitlab-org--glab-api
                    '("graphql" "-X" "POST" "--input" "-"
                      "-H" "Content-Type: application/json")
                    body))
         (errors (alist-get 'errors response)))
    (when (> (length errors) 0)
      (error "GitLab GraphQL: %s"
             (mapconcat (lambda (e) (alist-get 'message e)) errors "; ")))
    (alist-get 'data response)))

(defun gitlab-org--parse-ref (ref)
  "Split REF \"group/project#12\" into (\"group/project\" . \"12\")."
  (if (string-match "\\`\\([^#[:space:]]+\\)#\\([0-9]+\\)\\'" ref)
      (cons (match-string 1 ref) (match-string 2 ref))
    (user-error "Not a GitLab issue reference (group/project#123): %s" ref)))

(defun gitlab-org--issue-ref (issue)
  "Return the full reference of ISSUE, e.g. \"group/project#12\"."
  (alist-get 'full (alist-get 'references issue)))

(defun gitlab-org--issue-endpoint (ref)
  "REST endpoint of the issue REF."
  (pcase-let ((`(,path . ,iid) (gitlab-org--parse-ref ref)))
    (format "/projects/%s/issues/%s" (url-hexify-string path) iid)))

(defun gitlab-org--issue-url (ref)
  "Web URL of the issue REF."
  (pcase-let ((`(,path . ,iid) (gitlab-org--parse-ref ref)))
    (format "%s/%s/-/issues/%s" (gitlab-org--base-url) path iid)))

(defun gitlab-org-search-projects (query)
  "Return my projects matching QUERY, most recently active first."
  (append (gitlab-org--get
           (format "/projects?membership=true&simple=true&search_namespaces=true&order_by=last_activity_at&per_page=100&search=%s"
                   (url-hexify-string query)))
          nil))

(defun gitlab-org-get-project-issues (project-id params)
  "Return issues of PROJECT-ID filtered by query PARAMS, newest first."
  (gitlab-org--get-all
   (format "/projects/%s/issues?order_by=created_at&sort=desc&%s" project-id params)))

(defun gitlab-org-get-my-issues ()
  "Return open issues assigned to me in any project, recently updated first."
  (gitlab-org--get-all "/issues?scope=assigned_to_me&state=opened&order_by=updated_at&sort=desc"))

(defun gitlab-org-get-issue (ref)
  "Return the issue REF."
  (gitlab-org--get (gitlab-org--issue-endpoint ref)))

(defun gitlab-org-get-issue-notes (ref)
  "Return the comments on issue REF, oldest first, without system notes."
  (seq-remove (lambda (note) (alist-get 'system note))
              (gitlab-org--get-all
               (concat (gitlab-org--issue-endpoint ref)
                       "/notes?sort=asc&order_by=created_at"))))

(defun gitlab-org-get-project-labels (project-id)
  "Return the label names available in PROJECT-ID."
  (mapcar (lambda (label) (alist-get 'name label))
          (gitlab-org--get-all (format "/projects/%s/labels" project-id))))

(defconst gitlab-org--timelogs-query
  "query($path: ID!, $iid: String!) {
  project(fullPath: $path) {
    issue(iid: $iid) {
      id
      timelogs(last: 100) { nodes { spentAt timeSpent summary user { username } } }
    }
  }
}")

(defconst gitlab-org--timelog-create-mutation
  "mutation($id: IssuableID!, $time: String!, $at: Time!, $summary: String!) {
  timelogCreate(input: {issuableId: $id, timeSpent: $time, spentAt: $at, summary: $summary}) {
    errors
    timelog { timeSpent }
  }
}")

(defun gitlab-org-get-issue-timelogs (ref)
  "Return (GLOBAL-ID . TIMELOGS) for issue REF.
GLOBAL-ID is the issue's GraphQL id; TIMELOGS is a list of the most
recent 100 time logs."
  (pcase-let* ((`(,path . ,iid) (gitlab-org--parse-ref ref))
               (data (gitlab-org--graphql gitlab-org--timelogs-query
                                          `((path . ,path) (iid . ,iid))))
               (issue (alist-get 'issue (alist-get 'project data))))
    (unless issue
      (user-error "Issue %s not found" ref))
    (cons (alist-get 'id issue)
          (append (alist-get 'nodes (alist-get 'timelogs issue)) nil))))

(defun gitlab-org-add-timelog (issue-gid start-time minutes summary)
  "Log MINUTES spent at START-TIME on the issue with GraphQL id ISSUE-GID."
  (let* ((data (gitlab-org--graphql
                gitlab-org--timelog-create-mutation
                `((id . ,issue-gid)
                  (time . ,(format "%dm" minutes))
                  (at . ,(format-time-string "%FT%T%:z" start-time))
                  (summary . ,summary))))
         (result (alist-get 'timelogCreate data))
         (errors (alist-get 'errors result)))
    (when (> (length errors) 0)
      (error "GitLab: %s" (mapconcat #'identity errors "; ")))
    (alist-get 'timelog result)))

(defvar gitlab-org--username nil
  "Cached (HOST . USERNAME) of the current GitLab user.")

(defun gitlab-org--username ()
  "Return my GitLab username on `gitlab-org-host' (cached)."
  (unless (equal (car gitlab-org--username) (gitlab-org--host))
    (setq gitlab-org--username
          (cons (gitlab-org--host)
                (alist-get 'username (gitlab-org--get "/user")))))
  (cdr gitlab-org--username))

(defun gitlab-org--completing-read (prompt candidates)
  "Read one of CANDIDATES (an alist keyed by label) with PROMPT.
Return the selected value.  The candidates keep their order."
  (let* ((labels (mapcar #'car candidates))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata (display-sort-function . identity)
                                 (cycle-sort-function . identity))
                    (complete-with-action action labels string pred)))))
    (cdr (assoc (completing-read prompt table nil t) candidates))))

(defun gitlab-org--select-project ()
  "Search my projects and return the selected one."
  (let* ((query (read-string "Search GitLab projects (empty for recent): "))
         (projects (or (gitlab-org-search-projects query)
                       (user-error "No projects match \"%s\"" query))))
    (gitlab-org--completing-read
     "GitLab project: "
     (mapcar (lambda (p) (cons (alist-get 'path_with_namespace p) p)) projects))))

(defun gitlab-org--select-filter (project-id)
  "Ask how to filter the issues of PROJECT-ID; return REST query params."
  (pcase (completing-read "Show issues: "
                          '("Open" "Assigned to me, open" "Assigned to me"
                            "Closed" "By label..." "Search...")
                          nil t nil nil "Open")
    ("Assigned to me, open" "state=opened&scope=assigned_to_me")
    ("Assigned to me" "scope=assigned_to_me")
    ("Closed" "state=closed")
    ("By label..."
     (concat "state=opened&labels="
             (url-hexify-string
              (completing-read "Label: " (gitlab-org-get-project-labels project-id)
                               nil t))))
    ("Search..."
     (concat "state=opened&search="
             (url-hexify-string (read-string "Search issues: "))))
    (_ "state=opened")))

(defun gitlab-org--issue-label (issue &optional full-ref)
  "One-line description of ISSUE.
Show the full reference when FULL-REF is non-nil, else just #IID."
  (let ((labels (alist-get 'labels issue)))
    (concat (string-join
             (delq nil
                   (list (if full-ref
                             (gitlab-org--issue-ref issue)
                           (format "#%s" (alist-get 'iid issue)))
                         (when (equal (alist-get 'state issue) "closed") "[closed]")
                         (when (> (length labels) 0)
                           (format "[%s]" (string-join labels ",")))))
             " ")
            "  "
            (alist-get 'title issue))))

(defun gitlab-org--select-issue (issues &optional full-ref)
  "Read one of ISSUES and return it.  FULL-REF as in `gitlab-org--issue-label'."
  (unless issues
    (user-error "No matching issues"))
  (gitlab-org--completing-read
   "GitLab issue: "
   (mapcar (lambda (issue) (cons (gitlab-org--issue-label issue full-ref) issue))
           issues)))

(defun gitlab-org--browse-issue ()
  "Pick a project, a filter and one of the matching issues; return the issue."
  (let* ((project (gitlab-org--select-project))
         (project-id (alist-get 'id project))
         (params (gitlab-org--select-filter project-id)))
    (gitlab-org--select-issue (gitlab-org-get-project-issues project-id params))))

(defun gitlab-org--org-task (issue)
  "Return ISSUE as an org TODO subtree carrying its reference."
  (format "* TODO %s\n:PROPERTIES:\n:%s: %s\n:END:\n"
          (alist-get 'title issue) gitlab-org--property (gitlab-org--issue-ref issue)))

(defun gitlab-org--yank-org-task (issue)
  "Put ISSUE on the kill ring as an org TODO subtree."
  (kill-new (gitlab-org--org-task issue))
  (message "Yanked %s — use C-c C-x C-y (org-paste-subtree) to insert."
           (gitlab-org--issue-ref issue)))

(defun gitlab-org-insert-task ()
  "Browse a project's issues and yank the selected one as an org task."
  (interactive)
  (gitlab-org--yank-org-task (gitlab-org--browse-issue)))

(defun gitlab-org-insert-my-task ()
  "Pick one of my open issues (any project) and yank it as an org task."
  (interactive)
  (gitlab-org--yank-org-task (gitlab-org--select-issue (gitlab-org-get-my-issues) t)))

(defvar-local gitlab-org--buffer-ref nil
  "Reference of the issue shown in a `gitlab-org-view-issue' buffer.")

(defun gitlab-org--ref-at-point ()
  "Return a GitLab issue reference like \"group/project#12\" at point, or nil."
  (when (thing-at-point-looking-at
         "[[:alnum:]_.-]+\\(?:/[[:alnum:]_.-]+\\)+#[0-9]+" 300)
    (match-string-no-properties 0)))

(defun gitlab-org--read-ref ()
  "Return the issue reference to act on.
Use the GITLAB_ISSUE property of the org heading (inherited), else the
issue shown in this buffer, else ask, defaulting to a reference at point."
  (or (and (derived-mode-p 'org-mode)
           (ignore-errors (org-entry-get nil gitlab-org--property t)))
      gitlab-org--buffer-ref
      (let* ((at-point (gitlab-org--ref-at-point))
             (ref (read-string (if at-point
                                   (format "GitLab issue (default %s): " at-point)
                                 "GitLab issue (group/project#123): ")
                               nil nil at-point)))
        (when (string-blank-p ref)
          (user-error "No issue given"))
        ref)))

(defun gitlab-org-open-issue (ref)
  "Open the GitLab issue REF in a browser.
Interactively, see `gitlab-org--read-ref' for how REF is found."
  (interactive (list (gitlab-org--read-ref)))
  (browse-url (gitlab-org--issue-url ref)))

(defun gitlab-org--relevel-headings (level)
  "Shift org headings in the buffer so the topmost one is at LEVEL + 1."
  (goto-char (point-min))
  (let (top)
    (while (re-search-forward "^\\(\\*+\\) " nil t)
      (setq top (min (or top most-positive-fixnum) (length (match-string 1)))))
    (when top
      (let ((delta (- (1+ level) top)))
        (goto-char (point-min))
        (while (re-search-forward "^\\*+ " nil t)
          (goto-char (match-beginning 0))
          (if (> delta 0)
              (insert (make-string delta ?*))
            (delete-char (- delta)))
          (forward-line 1))))))

(defun gitlab-org--markdown-to-org (markdown level)
  "Convert MARKDOWN to org, its headings re-levelled to sit below LEVEL."
  (cond
   ((string-blank-p (or markdown "")) "")
   ((and (executable-find "pandoc")
         (with-temp-buffer
           (insert markdown)
           (let ((coding-system-for-read 'utf-8)
                 (coding-system-for-write 'utf-8))
             (when (eql 0 (call-process-region
                           (point-min) (point-max) "pandoc" t '(t nil) nil
                           "-f" "gfm-gfm_auto_identifiers" "-t" "org"
                           "--wrap=preserve"))
               (gitlab-org--relevel-headings level)
               (buffer-string))))))
   (t (format "#+begin_src markdown\n%s\n#+end_src\n" markdown))))

(defun gitlab-org--format-time (iso)
  "Format the ISO 8601 timestamp ISO as local \"YYYY-MM-DD HH:MM\"."
  (when iso
    (format-time-string "%Y-%m-%d %H:%M" (parse-iso8601-time-string iso))))

(defun gitlab-org--names (users)
  "Comma-separated display names of the USERS vector."
  (mapconcat (lambda (u) (alist-get 'name u)) users ", "))

(defun gitlab-org-view-issue (ref)
  "Show the GitLab issue REF with its comments in an org buffer.
Interactively, see `gitlab-org--read-ref' for how REF is found."
  (interactive (list (gitlab-org--read-ref)))
  (let* ((issue (gitlab-org-get-issue ref))
         (notes (gitlab-org-get-issue-notes ref))
         (labels (alist-get 'labels issue))
         (fields `(("State" . ,(alist-get 'state issue))
                   ("Author" . ,(alist-get 'name (alist-get 'author issue)))
                   ("Assignees" . ,(gitlab-org--names (alist-get 'assignees issue)))
                   ("Labels" . ,(and (> (length labels) 0) (string-join labels ", ")))
                   ("Milestone" . ,(alist-get 'title (alist-get 'milestone issue)))
                   ("Due" . ,(alist-get 'due_date issue))
                   ("Created" . ,(gitlab-org--format-time (alist-get 'created_at issue)))
                   ("Updated" . ,(gitlab-org--format-time (alist-get 'updated_at issue)))
                   ("Time spent" . ,(alist-get 'human_total_time_spent
                                               (alist-get 'time_stats issue)))
                   ("URL" . ,(format "[[%s]]" (alist-get 'web_url issue)))))
         (buffer (get-buffer-create (format "*GitLab %s*" ref))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: %s %s\n\n" ref (alist-get 'title issue)))
        (pcase-dolist (`(,name . ,value) fields)
          (unless (or (null value) (string-empty-p value))
            (insert (format "- %s :: %s\n" name value))))
        (insert "\n* Description\n"
                (gitlab-org--markdown-to-org (alist-get 'description issue) 1)
                "\n")
        (insert (format "* Comments (%d)\n" (length notes)))
        (dolist (note notes)
          (insert (format "** %s — %s\n"
                          (alist-get 'name (alist-get 'author note))
                          (gitlab-org--format-time (alist-get 'created_at note)))
                  (gitlab-org--markdown-to-org (alist-get 'body note) 2)
                  "\n"))
        (org-mode)
        (setq gitlab-org--buffer-ref ref)
        (setq buffer-read-only t)
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun gitlab-org--get-all-clock-entries ()
  "Return the clock entries of the current org heading, most recent first.
Each entry is (LABEL START-TIME . DURATION-MINUTES)."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t) (point)))
          entries)
      (while (re-search-forward
              "CLOCK: \\[\\([^]]+\\)\\]--\\[\\([^]]+\\)\\]\\s-*=>\\s-*\\([0-9]+\\):\\([0-9]+\\)"
              end t)
        (let* ((start-str (match-string 1))
               (end-str (match-string 2))
               (hours (string-to-number (match-string 3)))
               (mins (string-to-number (match-string 4)))
               (start-time (org-time-string-to-time (concat "[" start-str "]"))))
          (push (cons (format "%s — %s  (%dh %02dm)" start-str end-str hours mins)
                      (cons start-time (+ (* hours 60) mins)))
                entries)))
      (unless entries
        (user-error "No clock entries found under current heading"))
      ;; Org puts the newest CLOCK line first, so file order is newest first.
      (let ((i 0))
        (mapcar (lambda (e)
                  (setq i (1+ i))
                  (cons (format "%02d. %s" i (car e)) (cdr e)))
                (nreverse entries))))))

(defun gitlab-org--select-clock-entry ()
  "Read a clock entry of the current heading; return (START-TIME . MINUTES)."
  (let ((entries (gitlab-org--get-all-clock-entries)))
    (if (= (length entries) 1)
        (cdar entries)
      (gitlab-org--completing-read "Clock entry: " entries))))

(defun gitlab-org--my-timelogs-on (timelogs day)
  "Return those of TIMELOGS by me spent on DAY (\"YYYY-MM-DD\", local time)."
  (let ((me (gitlab-org--username)))
    (seq-filter (lambda (log)
                  (and (equal (alist-get 'username (alist-get 'user log)) me)
                       (equal (format-time-string
                               "%F" (parse-iso8601-time-string (alist-get 'spentAt log)))
                              day)))
                timelogs)))

(defun gitlab-org-submit-clock-entry ()
  "Submit a clock entry of the current org heading to its GitLab issue.
The issue comes from the GITLAB_ISSUE property (inherited)."
  (interactive)
  (let* ((ref (or (org-entry-get nil gitlab-org--property t)
                  (user-error "No %s property on this heading" gitlab-org--property)))
         (entry (gitlab-org--select-clock-entry))
         (start-time (car entry))
         (minutes (cdr entry))
         (day (format-time-string "%F" start-time))
         (issue-logs (gitlab-org-get-issue-timelogs ref))
         (existing (gitlab-org--my-timelogs-on (cdr issue-logs) day)))
    (if (and existing
             (not (yes-or-no-p
                   (format "You already logged %d min on %s on %s:\n%s\nSubmit %d min more? "
                           (/ (apply #'+ (mapcar (lambda (log) (alist-get 'timeSpent log))
                                                 existing))
                              60)
                           ref day
                           (mapconcat (lambda (log)
                                        (format "  %d min — %s"
                                                (/ (alist-get 'timeSpent log) 60)
                                                (or (alist-get 'summary log) "(no summary)")))
                                      existing "\n")
                           minutes))))
        (message "Submission cancelled.")
      (let ((summary (read-string "Summary: " (org-get-heading t t t t))))
        (gitlab-org-add-timelog (car issue-logs) start-time minutes summary)
        (message "Logged %d min on %s (%s)" minutes ref day)))))

(defun gitlab-org--clock-out-hook ()
  "After clocking out, offer to submit the time if the heading has GITLAB_ISSUE."
  (when-let* ((ref (org-entry-get nil gitlab-org--property t)))
    (when (y-or-n-p (format "Submit clocked time to GitLab %s? " ref))
      (gitlab-org-submit-clock-entry))))

(defun gitlab-org-enable-clock-out-hook ()
  "Offer to submit time to GitLab on `org-clock-out'."
  (interactive)
  (add-hook 'org-clock-out-hook #'gitlab-org--clock-out-hook)
  (message "GitLab clock-out hook enabled."))

(defun gitlab-org-disable-clock-out-hook ()
  "Stop offering to submit time to GitLab on `org-clock-out'."
  (interactive)
  (remove-hook 'org-clock-out-hook #'gitlab-org--clock-out-hook)
  (message "GitLab clock-out hook disabled."))

(defun gitlab-org--yank-issues (issues as-list)
  "Put ISSUES on the kill ring as org TODO subtrees.
With AS-LIST non-nil, as a `- ' list, one issue per line."
  (if (null issues)
      (message "No matching issues.")
    (kill-new (if as-list
                  (mapconcat (lambda (issue) (concat "- " (gitlab-org--issue-label issue t)))
                             issues "\n")
                (mapconcat #'gitlab-org--org-task issues "")))
    (message "Copied %d issues%s." (length issues)
             (if as-list "" " — use C-c C-x C-y (org-paste-subtree) to insert"))))

(defun gitlab-org-yank-my-issues (&optional as-list)
  "Copy my open issues (any project) to the kill ring as org tasks.
With prefix argument AS-LIST, copy them as a plain list."
  (interactive "P")
  (gitlab-org--yank-issues (gitlab-org-get-my-issues) as-list))

(defun gitlab-org-yank-project-issues (&optional as-list)
  "Select a project and a filter, then copy the matching issues as org tasks.
With prefix argument AS-LIST, copy them as a plain list."
  (interactive "P")
  (let* ((project (gitlab-org--select-project))
         (project-id (alist-get 'id project)))
    (gitlab-org--yank-issues
     (gitlab-org-get-project-issues project-id (gitlab-org--select-filter project-id))
     as-list)))

(defun gitlab-org-glab-token ()
  "Return the token `glab' stores for `gitlab-org-host', or nil."
  (let ((token (ignore-errors
                 (car (process-lines gitlab-org-glab-program "config" "get" "token"
                                     "--host" (gitlab-org--host))))))
    (unless (string-blank-p (or token ""))
      (string-trim token))))

(provide 'gitlab-org)
;;; gitlab-org.el ends here
