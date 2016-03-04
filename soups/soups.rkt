#lang scheme/base
(require setup/collects
         racket/contract/base
         scribble/core
         scribble/base
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         (for-syntax scheme/base))

(provide/contract
 [abstract 
  (->* () () #:rest (listof pre-content?)
       block?)]
 [subtitle
  (->* () () #:rest (listof pre-content?)
       content?)]
 #;[authorinfo
  (-> pre-content? pre-content? pre-content?
      block?)]
 [conferenceinfo
  (-> pre-content? pre-content?
      block?)]
 [copyrightyear
  (->* () () #:rest (listof pre-content?)
       block?)]
 [copyrightdata
  (->* () () #:rest (listof pre-content?)
       block?)]
 [exclusive-license
  (->* () ()
       block?)]
 #;[doi
  (->* () () #:rest (listof pre-content?)
       block?)]
 [to-appear
  (->* () () #:rest pre-content?
       block?)]
 [category
  (->* (pre-content? pre-content? pre-content?)
       ((or/c false/c pre-content?))
       content?)]
 [terms
  (->* () () #:rest (listof pre-content?)
       content?)]
 [keywords
  (->* () () #:rest (listof pre-content?)
       content?)])

(provide preprint 10pt nocopyright onecolumn noqcourier notimes
         include-abstract)

(define-syntax-rule (defopts name ...)
  (begin (define-syntax (name stx)
           (raise-syntax-error #f
                               "option must appear on the same line as `#lang scribble/soups'"
                               stx))
         ...
         (provide name ...)))
(defopts preprint 10pt nocopyright onecolumn noqcourier notimes)

(define soups-extras
  (let ([abs (lambda (s)
               (path->collects-relative
                (collection-file-path s "soups" "soups")))])
    (list
     (make-css-addition (abs "scrofulous-toad.css"))
     (make-tex-addition (abs "soups.tex")))))

;; ----------------------------------------
;; Abstracts:

(define abstract-style (make-style "abstract" soups-extras))

(define (abstract . strs)
  (make-nested-flow
   abstract-style
   (decode-flow strs)))

(define (extract-abstract p)
  (unless (part? p)
    (error 'include-abstract "doc binding is not a part: ~e" p))
  (unless (null? (part-parts p))
    (error 'include-abstract "abstract part has sub-parts: ~e" (part-parts p)))
  (when (part-title-content p)
    (error 'include-abstract "abstract part has title content: ~e" (part-title-content p)))
  (part-blocks p))

(define-syntax-rule (include-abstract mp)
  (begin
    (require (only-in mp [doc abstract-doc]))
    (make-nested-flow abstract-style (extract-abstract abstract-doc))))

;; ----------------------------------------
;; Authors and conference info:

(define (authorinfo name affiliation e-mail)
  ;; The \SAuthor macro in "style.tex" looks specifically
  ;; for an \SAuthorinfo as its argument, and handles it
  ;; specially in that case:
  (author
   (make-multiarg-element
    (make-style "SAuthorinfo" soups-extras)
    (list
     (make-element #f (decode-content (list name)))
     (make-element (make-style "SAuthorPlace" soups-extras)
                   (decode-content (list affiliation)))
     (make-element (make-style "SAuthorEmail" soups-extras)
                   (decode-content (list e-mail)))))))

(define (subtitle . str)
  (make-element (make-style "SSubtitle" (append '(aux) soups-extras))
                (decode-content str)))

(define (conferenceinfo what where)
  (make-paragraph
   (make-style 'pretitle null)
   (make-multiarg-element
    (make-style "SConferenceInfo" soups-extras)
    (list
     (make-element #f (decode-content (list what)))
     (make-element #f (decode-content (list where)))))))

(define (copyrightyear . when)
  (make-paragraph
   (make-style 'pretitle null)
   (make-element
    (make-style "SCopyrightYear" soups-extras)
    (decode-content when))))

(define (copyrightdata . what)
  (make-paragraph
   (make-style 'pretitle null)
   (make-element
    (make-style "SCopyrightData" soups-extras)
    (decode-content what))))

#;(define (doi . what)
  (make-paragraph
   (make-style 'pretitle null)
   (make-element
    (make-style "Sdoi" soups-extras)
    (decode-content what))))

(define (exclusive-license . what)
  (make-paragraph
   (make-style 'pretitle null)
   (make-element
    (make-style "SPexclusivelicense" soups-extras)
    (decode-content what))))

(define (to-appear . what)
  (make-paragraph
   (make-style 'pretitle null)
   (make-element
    (make-style "toappear" soups-extras)
    (decode-content what))))

;; ----------------------------------------
;; Categories, terms, and keywords:

(define (category sec title sub [more #f])
  (make-multiarg-element
   (make-style (format "SCategory~a" (if more "Plus" "")) soups-extras)
   (list*
    (make-element #f (decode-content (list sec)))
    (make-element #f (decode-content (list title)))
    (make-element #f (decode-content (list sub)))
    (if more
        (list (make-element #f (decode-content (list more))))
        null))))

(define (terms . str)
  (make-element (make-style "STerms" soups-extras) (decode-content str)))

(define (keywords . str)
  (make-element (make-style "SKeywords" soups-extras) (decode-content str)))
