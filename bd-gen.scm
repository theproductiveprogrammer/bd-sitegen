;; Brown-Dragon Site Generator
;; Main Features:
;;      * Parses and generates site from simple text markup (no html needed)
;;      * Sets up hidden posts and backdoors
;;      * Uses Scheme itself as the language for templates! This gives the
;;        full expressive power of Scheme transparently into any template.
;;        Because of this power, templates are used to generate all posts,
;;        landing pages, and RSS feeds without needing any special support
;;        from the generator.
;; Author: http://the-brown-dragon.com

;; Standard Posix functions
(require 'posix)
;; Regular Expressions
(require 'regex)
;; SRFI-1 (List Library)
(require 'srfi-1)
;; SRFI-13 (String Library)
(require 'srfi-13)

;; Helper functions for display
(define (bd.out _msg) (display _msg) (newline))
(define (bd.err-out _msg)
  (display _msg (current-error-port))(newline (current-error-port)))

;; Continuation Function for Error Return
(define (bd.error-return _x) (bd.err-out "Error handler not set!"))
(define (bd.exit _msg) (bd.err-out _msg) (bd.error-return #t))
;; Use this to set continuation function
(define (bd.error?)
  (call/cc
   (lambda (s)
     (set! bd.error-return s)
     (s #f))))

;; Helper function for executing external programs
(define (bd.exec _cmd . _name)
  (if (< 0 (length _name)) (bd.out (format "~A..." (car _name))))
  (call-with-values (lambda () (process _cmd))
    (lambda (ip op pid)
      (call-with-values (lambda () (process-wait pid))
        (lambda (pid_ norm status)
          (when (or (not norm) (not (= 0 status)))
                (bd.exit (format "~A failed!" _name))))))))

;; Helper functions for Templates
(define (bd.2-html _markup)
  (call-with-values (lambda ()(process "sed -f markup2html.sed"))
    (lambda (ip op pid)
      (if (string? _markup)
          (display _markup op)
          (for-each (lambda (l) (display l op)(newline op)) _markup))
      (close-output-port op)
      (let ((r (read-all ip)))
        (close-input-port ip)
        r))))

(define (bd.esc-html _str)
  (string-substitute "<" "&lt;"
   (string-substitute ">" "&gt;"
    (string-substitute "\"" "&quot;"
     (string-substitute "\\&" "&amp;" _str 'g) 'g) 'g) 'g))

(define (bd.post-teaser _line)
  (set! _line (string-substitute (regexp* "<.*>" '(ungreedy)) "" _line 'g))
  (if (> (string-length _line) 64)
      (string-append (substring _line 0 64) "...")
      _line))

(define (bd.display-date dt)
  (set! dt (seconds->local-time dt))
  (let ((y (+ 1900 (vector-ref dt 5)))
        (m (+ 1 (vector-ref dt 4)))
        (d (vector-ref dt 3)))
    (if (> 10 m) (set! m (format "0~A" m)))
    (if (> 10 d) (set! d (format "0~A" d)))
    (format "~S-~A-~A" y m d)))

(define (bd.display-yymm dt)
  (set! dt (seconds->local-time dt))
  (let ((y (+ 1900 (vector-ref dt 5)))
        (m (+ 1 (vector-ref dt 4))))
    (if (> 10 m) (set! m (format "0~A" m)))
    (format "~S-~A" y m)))

(define (bd.rss-date dt)
  (set! dt (seconds->local-time dt))
  (let ((y (+ 1900 (vector-ref dt 5)))
        (m (vector-ref (vector "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                               "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                       (vector-ref dt 4)))
        (d (vector-ref dt 3))
        (wd (vector-ref (vector "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
                        (vector-ref dt 6))))
    (format "~A, ~A ~A ~A 00:00:00 GMT" wd d m y)))

(define (bd.tag-fname _tag)
  (string-append "tag-" (string-downcase (string-delete #\/ _tag)) ".htm"))

(define (bd.posts-by-similarity _post _posts)
  (let ((bd.pbs-1
         (lambda (p)
           (apply + (map (lambda (tg) (if (member tg (post-tags _post)) 1 0))
                         (post-tags p))))))
    (filter (lambda (p) (not (eq? p _post)))
            (sort _posts
                  (lambda (p1 p2)
                    (let ((i1 (bd.pbs-1 p1))(i2 (bd.pbs-1 p2)))
                      (if (= i1 i2) (> (post-date p1) (post-date p2))
                          (> i1 i2))))))))
;; Parser
(define (bd.parse _fname _fn)
  (call-with-input-file _fname
    (lambda (f) (for-each (lambda (l) (set! _fn (_fn l))) (read-lines f)))))

(define (bd.parse-tags _fname)
  (letrec ((tags '()) (bd.pt (lambda (l) (set! tags (cons l tags)) bd.pt)))
    (bd.parse _fname bd.pt)
    (reverse tags)))

(define (bd.parse-date _line)
  (let ((m (string-match " *(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d) *" _line)))
    (if (not m) (bd.exit "Expected date: yyyy-mm-dd"))
    (let ((yr (- (string->number (cadr m)) 1900))
          (mn (- (string->number (caddr m)) 1))
          (dy (string->number (cadddr m))))
      (local-time->seconds (vector 0 0 0 dy mn yr #f #f #f 0)))))

(define (bd.parse-post-tags _line _tags)
  (let ((tags '())(add-tag #f))
    (set! add-tag
          (lambda (curtg)
            (let ((ct (find (cut string-ci=? <> curtg) _tags)))
              (if (not ct) (bd.exit (format "Failed to find tag: ~S" curtg)))
              (set! tags (cons ct tags)))))
    (if (not (string-match "Tags[ \t]*:[ \t]*.*" _line))
        (bd.exit "Expected tags line to start with 'Tags:'"))
    (let loop ((t (cadr (string-match "Tags[ \t]*:[ \t]*(.*)" _line))))
      (let ((m (string-match "([^, \t]+)[, \t]+(.*)" t)))
        (if (not m)
            (if (not (string-match "[, \t]*" t)) (add-tag t))
            (begin (add-tag (cadr m)) (loop (caddr m))))))
    (if (eq? tags '()) (bd.exit "No tags found!"))
    tags))

;; Record to hold a post
(define-record post fname ofname link title date tags quot content teaser)

(define (bd.parse-post _fname _tags)
  (letrec ((r (make-post _fname #f #f #f #f #f '() '() #f))
           (bd.pp-1 (lambda (l) (post-title-set! r l) bd.pp-2))
           (bd.pp-2 (lambda (l) (post-date-set! r (bd.parse-date l)) bd.pp-3))
           (bd.pp-3 (lambda (l) (post-tags-set! r (bd.parse-post-tags l _tags))
                            (lambda (l) (bd.pp-4 bd.pp-5 l))))
           (bd.pp-4                     ; Ignore blank lines before input
            (lambda (f l) (if (string-match "[ \t]*" l)
                              (cut bd.pp-4 f <>)
                              (f l))))
           (bd.pp-5                     ; Quote is terminated by a single '.'
            (lambda (l)
              (if (string-match "[ \t]*.[ \t]*" l)
                  (lambda (l) (bd.pp-4 bd.pp-6 l))
                  (begin (post-quot-set! r (cons l (post-quot r))) bd.pp-5))))
           (bd.pp-6 (lambda (l) (post-teaser-set! r l) (bd.pp-7 l)))
           (bd.pp-7 (lambda (l)
                      (post-content-set! r (cons l (post-content r))) bd.pp-7)))
    (bd.parse _fname bd.pp-1)
    (cond ((not (post-title r)) (bd.exit "No title found!"))
          ((not (post-date r)) (bd.exit "No date found!"))
          ((not (post-tags r)) (bd.exit "No tags found!"))
          ((eq? (post-quot r) '()) (bd.exit "No quote found!"))
          ((eq? (post-content r) '()) (bd.exit "No content found!"))
          (#t (post-quot-set! r (reverse (post-quot r)))
              (post-content-set! r (reverse (post-content r)))
              r))))

(define (bd.parse-codeblock _post)
  (let ((r '()))
    (for-each
     (lambda (l)
       (let ((m (string-match "^\\[code (.*)\\]$" l)))
         (set! r (append r (list l)))
         (when m
               (call-with-input-file
                   (make-pathname (pathname-directory (post-fname _post))
                                  (cadr m))
                 (lambda (f)
                   (for-each (lambda (l) (set! r (append r (list l))))
                             (read-lines f))))
               (set! r (append r (list "[/codeend]"))))))
     (post-content _post))
    r))

;; Record to hold overall site
(define-record site tags posts)

;; Template processing
(define (bd.apply-template _template _inp _ofile _curr _site)
  (letrec ((disp #f) (ml #f)
           (bd.at-1
            (lambda (l)
              (let ((m (string-match "(.*)<%(\\(.*\\))%>(.*)" l)))
                (if m                   ; Single line embedded code
                    (let ((f (string-append "(lambda (_curr _site)"
                                            (caddr m)
                                            ")")))
                      (set! f (eval (with-input-from-string f read)))
                      (disp (string-append (cadr m) (f _curr _site) (cadddr m)))
                      bd.at-1)
                    (let ((m (string-match "(.*)<%(\\(.*)" l)))
                      (if m             ; Multi-line embedded code
                          (begin
                            (set! ml (caddr m))
                            (disp (cadr m))
                            bd.at-2)
                          (begin        ; No embedded code
                            (disp l)
                            bd.at-1)))))))
           (bd.at-2
            (lambda (l)
              (let ((m (string-match "(.*\\))%>(.*)" l)))
                (if m                ; End of multi-line embedded code
                    (let ((f (string-append "(lambda (_curr _site)"
                                            (string-append ml (cadr m))
                                            ")")))
                      (set! f (eval (with-input-from-string f read)))
                      (disp (string-append (f _curr _site) (caddr m)))
                      bd.at-1)
                    (begin
                      (set! ml (string-append ml l))
                      bd.at-2))))))
    (call-with-output-file _ofile
      (lambda (port)
        (bd.out (format "Processing ~S to ~S..." _inp _ofile))
        (set! disp (lambda (s) (display s port)))
        (bd.parse _template bd.at-1)))))

;; Main function - site generator
(define (bd.main _contentdir
                 _templatedir
                 _backdoordir
                 _tagfile
                 _ndx-template
                 _tag-template
                 _post-template
                 _rdf-templates
                 _outdir)
  (let ((ndx-template (make-pathname _templatedir _ndx-template))
        (tag-template (make-pathname _templatedir _tag-template))
        (post-template (make-pathname _templatedir _post-template))
        (rdf-templates (map (lambda (t)
                              (make-pathname _templatedir t)) _rdf-templates))

        (ndx (make-pathname _outdir "index.htm"))

        (posts #f)
        (site (make-site _outdir #f))

        (hidden-posts (lambda (t) (string=? "hidden" t))))

    ;; Let's clean up and get ready for new output
    (if (directory? _outdir)
        (bd.exec (format "rm -rf ~S" _outdir) "Clean existing site"))
    ;; Start parsing
    (bd.out "Parsing tags...")
    (site-tags-set! site (bd.parse-tags _tagfile))
    (bd.out "Parsing blogs...")
    (set! posts
          (map (lambda (f)
                 (bd.out (format "Parsing blog ~S" f))
                 (bd.parse-post f (site-tags site)))
               (find-files _contentdir ".*\\.blog")))
    (for-each
     (lambda (p)
       (post-link-set! p (string-append (pathname-file (post-fname p)) ".htm"))
       (post-ofname-set! p (make-pathname _outdir (post-link p))))
     posts)
    ;; Resolve [code ...] links
    (map (lambda (p) (post-content-set! p (bd.parse-codeblock p))) posts)
    ;; Right - parsing done! Set up the site information
    (bd.out "Setting up site information...")
    (site-tags-set! site (remove hidden-posts (site-tags site)))
    (site-posts-set!
     site
     (sort (remove (lambda (p) (find hidden-posts (post-tags p))) posts)
           (lambda (p1 p2) (> (post-date p1) (post-date p2)))))
    ;; Site info done - now output posts
    (bd.out "Creating posts...")
    (create-directory _outdir)
    (for-each
     (lambda (p)
       (bd.apply-template post-template (post-fname p) (post-ofname p) p site))
     posts)
    ;; Create tag landing pages
    (bd.out "Creating tag landing pages...")
    (for-each
     (lambda (tg)
       (bd.apply-template tag-template `(tag ,tg)
                          (make-pathname _outdir (bd.tag-fname tg)) tg site))
     (site-tags site))
    ;; Create index.htm(l), default.htm(l) [4 files]
    (bd.out "Creating index pages...")
    (bd.apply-template ndx-template '(all posts) ndx #f site)
    (bd.exec (string-append "cp " ndx " " (make-pathname _outdir "index.html")))
    (bd.exec
     (string-append "cp " ndx " " (make-pathname _outdir "default.htm")))
    (bd.exec
     (string-append "cp " ndx " " (make-pathname _outdir "default.html")))
    ;; Full or Quick Run?
    (when (and (not (member "/quickrun" (argv)))
               _backdoordir)
          ;; Create RDF Feeds
          (bd.out "Creating RDF feeds...")
          (for-each
           (lambda (ft)
             (bd.apply-template ft '(rdf feed)
                                (make-pathname _outdir (pathname-file ft) "xml")
                                #f site))
           rdf-templates)
          ;; Copy backdoor files
          (bd.exec (string-append "rsync -ai " _backdoordir "/ " _outdir)
                   "Copying backdoor files..."))
    ;; Copy additional supporting files
    (bd.out "Copying supporting files...")
    (bd.exec (string-append "rsync -ai " _templatedir "/ " _outdir))
    (bd.exec (string-append "rsync -ai " _contentdir "/ " _outdir))
    (bd.exec (string-append "chmod -R 777 " _outdir))))

(define (bd.go!)
  (if (bd.error?)(exit 1))
  (bd.exec "./cpylatest.sh" "Copy latest stuff")
  (bd.main "content" "template" "backdoor"
           "content/tags.list"
           "ndx.htm" "tag.htm" "post.htm"
           '("rss.rdf" "rss-egg.rdf")
           "site/www"))

(bd.go!)
