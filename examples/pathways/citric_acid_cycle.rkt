#lang racket

(require "../../omics/atoms.rkt")
(require "../../omics/paths.rkt")

; reference article: https://en.wikipedia.org/wiki/Citric_acid_cycle

(define H (atom 'H))
(define O (atom 'O))

(define HOH (bond O (list H H)))

(chemical-path
  (@
    'substrate (list oxaloacetate acetyl_CoA HOH)
    'enzyme citrate_synthase
    'product (list citrate CoA-SH))
  (@
    'substrate citrate
    'enzyme aconitase
    'product (list cis-aconitate HOH))
  (@
    'substrate (list cis-aconitate HOH)
    'enzyme aconitase
    'product isocitrate)
  (@
    'substrate (list isocitrate NAD+)
    'enzyme isocitrate_dehydrogenase
    'product (list oxalosuccinate NADH H+))
  (@
    'substrate oxalosuccinate
    'enzyme isocitrate_dehydrogenase
    'product (list α-ketoglutarate CO2))
  (@
    'substrate (list α-ketoglutarate NAD+ CoA-SH)
    'enzyme α-ketoglutarate_dehydrogenase
    'product (list succinyl-CoA NADH H+ CO2))
  (@
    'substrate (list succinyl-CoA GDP Pi)
    'enzyme succinyl-CoA_synthetase
    'product (list succinate CoA-SH GTP))
  (@
    'substrate (list succinate ubiquinone)
    'enzyme succinate_dehydrogenase
    'product (list fumarate ubiquinol))
  (@
    'substrate (list fumarate HOH)
    'enzyme fumarase
    'product (list l-malate))
  (@
    'substrate (list l-malate NAD+)
    'enzyme malate_dehydrogenase
    'product (list oxaloacetate NADH H+)))
