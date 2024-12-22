;;;; sample configuration file
(config site-title "title-of-your-site")
(config site-base-url "https://your-site-url")

;; setup pipe line
(set-stages
 (list (mk-stage ("public/gemini/" :index-config '("index.gmi" "index.gmi"))
                 (is-type "gmi") apply-template
                 (is-type "org") (copy-as-type "txt" t)
                 (is-type "rst") (copy-as-type "txt" t)
                 t copy)
       ;;; for example you can uncomment the following stage to generate www
       ;;; site from gemtext site
       ;; (mk-stage ("public/www/" :name "gemtxt-to-html")
       ;;           (is-type "gmi") gemtext->html
       ;;           (glob-match "**/*.rst.txt") rst->html
       ;;           t copy)
       ))

;; if you enabled stage "gemtxt-to-html" like above, uncomment the following
;;  so gemtxt-to-html can rewrite links of '.rst.txt' file correctly
;; (rewrite-url-of ".rst.txt") 

;; tell mokubune to print more messages
(be-verbose)
