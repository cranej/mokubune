;;;; sample configuration file
(config site-title "cranej's personal site")
(config site-base-url "https://your-site-url")

;;; uncomment if you want to apply templates to markdown files other then copying
;; (add-rule "*.md" :apply-template)

;;; you can even apply templates to certain text files
;; (add-rule "*post*.txt" :apply-template)

;; tell mokubune to print more messages
(be-verbose)
