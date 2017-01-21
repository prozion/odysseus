#lang racket

(require "../../../odysseus/lib/all.rkt")
(require "../../../odysseus/pmf/all.rkt")

(provide (all-defined-out))

(define keywords (list
  "Personal Health Informatics"
  "Quantified Self"
    "lifelogging" "self-tracking" "auto-analytics" "body hacking" "self-quantifying" "personal informatics" "self-surveillance"
))

(define
  organizations
  (list
    (@ 'url "http://quantifiedself.com" 'place "CA/?" 'email "labs@quantifiedself.com")
    (@ 'url "https://www.amia.org" 'place "MD/Bethesda")
    (@ 'url "http://www.himss.org" 'place "IL/Chicago")
))

(define
  people
  (list
    (@ 'surname "Korhonen" 'name "Ilkka" 'tags "") ; https://www.youtube.com/watch?v=fLnmiobvsyo&feature=youtu.be&t=11m
    (@ 'surname "Ziegler" 'name "Leslie" 'email "leslieziegler@gmail.com" 'twitter "lesliejz" 'tags "") ; Chicago Ideas Week, tracking body parameters throughout a year: https://www.youtube.com/watch?v=pjmNmvl7cdI
    (@ 'surname "Roth" 'name "Chris" 'email "chris@cjroth.com" 'tags "") ; google spreadsheets and logging through sending chat emojis
))

(define
  devices
  (list
    (@ 'name "FitBit" 'producer "FitBit" 'type "pulse-measurement accelerometer")
    (@ 'name "Zeo Personal Sleep Manager" 'producer "Zeo" 'type "EEG")
    (@ 'name "iBrain" 'producer "Neurovigil" 'type "EEG")
    (@ 'name "Neuroon Sleep Mask" 'producer "Neuroon" 'url "https://neuroon.com" 'type "EEG")
    (@ 'name "Telomere tester" 'producer "Titanovo" 'type "dna-measurement" 'measurement-target "cell/nucleus/chromosome")
    (@ 'name "Bio Stamp RC" 'producer "mc10" 'device-type "portable-sensor" 'type "EMG ECG")
    (@ 'name "Microbiome Explorer" 'producer "UBiome" 'url "https://ubiome.com" 'type "dna-measurement" 'measurement-target "prokaryotes/dna")
))

(define
  personalwares
  (list
    (@ 'repo "https://github.com/familysmarts/ostlog" 'author "Sergio Bogazzi" 'outlet "https://familysmarts.net") ; family
    (@ 'repo "https://gitlab.com/ddenniss/odysseus" 'author "Denis Shirshov" 'email "denis.shirshov@gmail.com" 'outlet "https://familysmarts.net") ; ask for access to the repo
))


;(define
;  device-producers
;  (list
;    (@ 'name "Blooming" 'url "http://blooming.io" 'place "Finland/?" 'email "kristian@meruhealth.com") ; portable EEG?
;    (@ 'name "FitBit" 'url "https://www.fitbit.com" 'place "CA/San-Francisco ...")
;    (@ 'name "mc10" 'url "https://www.mc10inc.com" 'place "MA/Lexington")
;))
