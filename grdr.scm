(use-modules (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-9 gnu)
             (sxml simple)
             (sxml xpath)
             (sxml match))

(define namespace-prefixes
  '((rss . "http://purl.org/rss/1.0/modules/content/")
    (atom . "http://www.w3.org/2005/Atom")))

(define (open-xml path)
  (xml->sxml (open-input-file path)
             #:namespaces namespace-prefixes
             #:trim-whitespace? #t))

(define atom-feed
  (open-xml "./tests/atom.xml"))

(define rss-feed
  (open-xml "./tests/rss2.xml"))

(define-immutable-record-type <feed>
  (make-feed title author entries)
  feed?
  (title feed-title set-feed-title)
  (author feed-author set-feed-author)
  (entries feed-entries set-feed-entries))
(define default-feed
   (make-feed "" "" '()))

(define-immutable-record-type <entry>
  (make-entry title link id date description content)
  entry? x
  (title entry-title set-entry-title)
  (link entry-link set-entry-link)
  (id entry-id set-entry-id)
  (date entry-date set-entry-date)
  (description entry-description set-entry-description)
  (content entry-content set-entry-content))
(define default-entry
  (make-entry "" "" "" "" "" ""))

(define (set-entries feed new-entry)
  (set-feed-entries
   feed
   (cons new-entry (feed-entries feed))))

(define (find-feed top)
  (filter (compose not null?)
          ((node-or
            (node-closure (node-typeof? 'atom:feed))
            (node-closure (node-typeof? 'cfashannel)))
           top)))

(define (match-feed-child child feed)
  (sxml-match
   child
   ;; Title
   ((atom:title ,t)
    (set-feed-title feed t))
   ((title ,t)
    (set-feed-title feed t))
   ;; Author
   ((atom:author (atom:name ,a))
    (set-feed-author feed a))
   ;; Entry
   ((atom:entry . ,e)
    (set-entries feed (parse-entry e)))
   ((item . ,e)
    (set-entries feed (parse-entry e)))
   (,otherwise
    feed)))

(define (match-entry-child child entry)
  (sxml-match
   child
   ;; Title
   ((atom:title ,t)
    (set-entry-title entry t))
   ((title ,t)
    (set-entry-title entry t))
    ;; Link
   ((atom:link (@ (href ,url)))
    (set-entry-link entry url))
   ((link ,url)
    (set-fields entry
                ((entry-link) url)
                ((entry-id) url)))
   ;; ID
   ((atom:id ,id)
    (set-entry-id entry id))
   ;; Description
   ((atom:summary ,content)
    (set-entry-description entry content))
   ((description ,content)
    (set-entry-description entry content))
   ;; Content
   ((atom:content ,content)
    (set-entry-content entry content))
   ((content ,content)
    (set-entry-content entry content))
   ;; Date
   ((atom:updated ,date)
    (set-entry-date entry date))
   ((pubDate ,date)
    (set-entry-date entry date))
   (,otherwise
    entry)))

(define (parse-feed x)
  (fold match-feed-child default-feed x))

(define (parse-entry e)
  (fold match-entry-child default-entry e))
