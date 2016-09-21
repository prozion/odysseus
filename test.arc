;; load module to work with barchart
;; (load "lib/barchart.arc")

;; read data from izbirkom.ru
(= page_url (string "izbirkom.ru" ""))
(= page_contents (get-url page_url))

;; get data into list, manipulations with page_contents using regexps
(= votes_data '((1 10) (2 35) (3 70) (4 12)))

;; draw chart
(= svg_contents "<svg></svg>") ;;(barchart/draw votes_data))

;; write to file
(= output_file (argv 1))
(writefile svg_contents output_file)
