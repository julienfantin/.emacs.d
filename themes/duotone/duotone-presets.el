;;; Commentary:
;; https://github.com/smlombardi/duotone-blue-dark
;;; Code:
(require 'cl-lib)
(require 'duotone-palette)

(defcustom duotone-palette-sea-light
  (make-instance
   'duotone-palette
   :uno    212
   :duo    160
   :uno-1  '(.76 .10)
   :uno-2  '(.62 .24)
   :uno-3  '(.55 .44)
   :uno-4  '(.15 .70)
   :duo-1  '(1.00 .28)
   :duo-2  '(.66 .52)
   :duo-3  '(.06 .80)
   :bg     '(.33 1.00)
   :accent '(.90 .40))
  "https://github.com/5310/duotone-bright-sea-syntax/blob/master/styles/colors.less")

(defcustom duotone-palette-candid-light
  (make-instance
   'duotone-palette
   :uno      200
   :duo      40
   :uno-1    '(.76 .10)
   :uno-2    '(.58 .34)
   :uno-3    '(.34 .54)
   :uno-4    '(.16 .66)
   :duo-1    '(.92 .28)
   :duo-2    '(.46 .62)
   :duo-3    '(.06 .70)
   :bg       '(.12 .92)
   :renamed  '(208 1.00 .60)
   :added    '(150  .60 .54)
   :modified '(40   .60 .70)
   :removed  '(0    .70 .60)
   :accent   '(.80 .60))
  "https://github.com/tsangkenneth/duotone-light-blue-syntax/blob/master/styles/colors.less")

(defcustom duotone-palette-arch-light
  (make-instance
   'duotone-palette
   :bg       (chroma-hex :hex "#f4f5f6")
   :uno-1    (chroma-hex :hex "#14151a")
   :uno-2    (chroma-hex :hex "#272a34")
   :uno-3    (chroma-hex :hex "#343a46")
   :uno-4    (chroma-hex :hex "#828996")
   :duo-1    (chroma-hex :hex "#437fdc")
   :duo-2    (chroma-hex :hex "#5a92e3")
   :duo-3    (chroma-hex :hex "#757c8e")
   :added    (chroma-hex :hex "#2acb34")
   :renamed  (chroma-hex :hex "#437fdc")
   :removed  (chroma-hex :hex "#fb4847")
   :modified (chroma-hex :hex "#fbb825")
   :accent   (chroma-hex :hex "#437fdc"))
  "Mono dark palette.")

(defcustom duotone-palette-arch-dark
  (make-instance
   'duotone-palette
   :bg       (chroma-hex :hex "#272a34")
   :uno-1    (chroma-hex :hex "#ffffff")
   :uno-2    (chroma-hex :hex "#f4f5f6")
   :uno-3    (chroma-hex :hex "#ebeced")
   :uno-4    (chroma-hex :hex "#757c8e")
   :duo-1    (chroma-hex :hex "#437fdc")
   :duo-2    (chroma-hex :hex "#5a92e3")
   :duo-3    (chroma-hex :hex "#343a46")
   :added    (chroma-hex :hex "#2acb34")
   :renamed  (chroma-hex :hex "#437fdc")
   :removed  (chroma-hex :hex "#fb4847")
   :modified (chroma-hex :hex "#fbb825")
   :accent   (chroma-hex :hex "#437fdc"))
  "Mono dark palette.")

(defcustom duotone-palette-mono-light
  (make-instance
   'duotone-palette
   :bg       (chroma-hex :hex "#F7F6F1")
   :uno-1    (chroma-hex :hex "#000")
   :uno-2    (chroma-hex :hex "#222")
   :uno-3    (chroma-hex :hex "#444")
   :uno-4    (chroma-hex :hex "#666")
   :duo-1    (chroma-hex :hex "#111")
   :duo-2    (chroma-hex :hex "#333")
   :duo-3    (chroma-hex :hex "#555")
   :added    (chroma-hex :hex "#5F985F")
   :renamed  (chroma-hex :hex "#678DCB")
   :removed  (chroma-hex :hex "#98655F")
   :modified (chroma-hex :hex "#CBBC6B")
   :accent   (chroma-hex :hex "#6A447E"))
  "Mono dark palette.")

(defcustom duotone-palette-mono-dark
  (make-instance
   'duotone-palette
   :bg       (chroma-hex :hex "#292A2A")
   :uno-1    (chroma-hex :hex "#eee")
   :uno-2    (chroma-hex :hex "#ccc")
   :uno-3    (chroma-hex :hex "#999")
   :uno-4    (chroma-hex :hex "#777")
   :duo-1    (chroma-hex :hex "#eee")
   :duo-2    (chroma-hex :hex "#bbb")
   :duo-3    (chroma-hex :hex "#888")
   :added    (chroma-hex :hex "#23CC1C")
   :renamed  (chroma-hex :hex "#2377FF")
   :removed  (chroma-hex :hex "#CC331C")
   :modified (chroma-hex :hex "#FFDF03")
   :accent   (chroma-hex :hex "#fff"))
  "Mono light palette.")

(defcustom duotone-palette-earth-dark
  (make-instance
   'duotone-palette
   :uno    24
   :duo    42
   :uno-1  '(.99 .96)
   :uno-2  '(.24 .62)
   :uno-3  '(.12 .48)
   :uno-4  '(.08 .38)
   :duo-1  '(.99 .66)
   :duo-2  '(.44 .55)
   :duo-3  '(.24 .44)
   :bg     '(.10 .18)
   :accent '(1.00 .66))
  "Dark earth https://github.com/simurai/duotone-dark-earth-syntax")

(defcustom duotone-palette-sea-dark
  (make-instance
   'duotone-palette
   :uno      212
   :duo      160
   :uno-1    '(.99 .92)
   :uno-2    '(.88 .77)
   :uno-3    '(.24 .54)
   :uno-4    '(.12 .38)
   :duo-1    '(.99 .70)
   :duo-2    '(.72 .48)
   :duo-3    '(.12 .38)
   :bg       '(.24 .15)
   :accent   '(1.00 .66)
   :renamed  '(208 1.00 .60)
   :added    '(150  .60 .54)
   :modified '(40   .60 .70)
   :removed  '(0    .70 .60))
  "Duotone light palette.
https://github.com/simurai/duotone-dark-sea-syntax."
  :group 'duotone
  :type 'duotone-palette)


(defcustom duotone-palette-space-dark
  (make-instance
   'duotone-palette
   :uno    240
   :duo    20
   :uno-1  '(.99 .96)
   :uno-2  '(.28 .70)
   :uno-3  '(.12 .52)
   :uno-4  '(.08 .38)
   :duo-1  '(.99 .66)
   :duo-2  '(.72 .52)
   :duo-3  '(.16 .36)
   :bg     '(.12 .16)
   :accent '(1.00 .66))
  "Duotone light palette.
https://github.com/simurai/duotone-dark-sea-syntax."
  :group 'duotone
  :type 'duotone-palette)


(defcustom duotone-palette-duotone-light
  (make-instance
   'duotone-palette
   :uno      40
   :uno-1    '(0.76 0.13)
   :uno-2    '(0.65 0.35)
   :uno-3    '(0.34 0.54)
   :uno-4    '(0.16 0.66)
   :duo      220
   :duo-1    '(0.92 0.3)
   :duo-2    '(0.46 0.52)
   :duo-3    '(0.06 0.7)
   :bg       '(.33 .97)
   :accent   '(.80 .60)
   :renamed  '(208 1.00 .60)
   :modified '(40 .60 .70)
   :removed  '(0 .70 .60))
  "Duotone light palette."
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-duotone-dark
  (make-instance
   'duotone-palette
   :uno    250
   :uno-1  '(1.0 .96)
   :uno-2  '(.98 .86)
   :uno-3  '(.96 .78)
   :uno-4  '(.12 .46)
   :duo    30
   :duo-1  '(1.0 .80)
   :duo-2  '(.72 .62)
   :duo-3  '(.06 .46)
   :bg     '(.14 .18)
   :accent '(1.0 .66))
  "Duotone dark palette."
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-blessed-light
  (make-instance
   'duotone-palette
   :uno      195
   :uno-1    '(.76 .1)
   :uno-2    '(.58 .34)
   :uno-3    '(.34 .54)
   :uno-4    '(.16 .66)
   :duo      310
   :duo-1    '(.92 .28)
   :duo-2    '(.46 .62)
   :duo-3    '(.6 .7)
   :bg       '(.33 .97)
   :accent   '(.80 .60)
   :renamed  '(208 1.00 .60)
   :added    '(150 .60 .54)
   :modified '(40 .60 .70)
   :removed  '(0 .70 .60))
  "Duotone palette blessed-light."
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-blessed-dark
  (make-instance
   'duotone-palette
   :uno      195
   :uno-1    '(1.00 .96)
   :uno-2    '( .98 .86)
   :uno-3    '( .96 .78)
   :uno-4    '( .12 .46)
   :duo      310
   :duo-1    '(1.00 .80)
   :duo-2    '( .72 .62)
   :duo-3    '( .06 .46)
   :accent   '(1.00 .66)
   :bg       '(195 .14 .18)
   :accent   '(.80 .60)
   :renamed  '(208 1.00 .60)
   :added    '(150 .60 .54)
   :modified '(40 .60 .70)
   :removed  '(0 .70 .60))
  "Duotone palette blessed-light."
  :group 'duotone
  :type 'duotone-palette)


(defcustom duotone-palette-newspaper-light
  (make-instance
   'duotone-palette
   :uno-1  (chroma-hex :hex "#202936")
   :uno-2  (chroma-hex :hex "#3F4D63")
   :uno-3  (chroma-hex :hex "#607494")
   :uno-4  (chroma-hex :hex "#849EC7")
   :duo-1  (chroma-hex :hex "#524159")
   :duo-2  (chroma-hex :hex "#875067")
   :duo-3  (chroma-hex :hex "#B86266")
   :bg     (chroma-hex :hex "#fff")
   :accent (chroma-hex :hex "#307769"))
  "Duotone palette newspaper light."
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-emacs-light
  (make-instance
   'duotone-palette
   :bg       (chroma-hex :hex "#EFECDE")
   :fg       (chroma-hex :hex "#302D2D")
   :uno-1    (chroma-hex :hex "#262626")
   :uno-2    (chroma-hex :hex "#3E2660")
   :uno-3    (chroma-hex :hex "#3F403F")
   :uno-4    (chroma-hex :hex "#585858")
   :duo-1    (chroma-hex :hex "#4F2D83")
   :duo-2    (chroma-hex :hex "#6B42A7")
   :duo-3    (chroma-hex :hex "#4E1273")
   :renamed  (chroma-hex :hex "#5A5AD1")
   :removed  (chroma-hex :hex "#CA005A")
   :modified (chroma-hex :hex "#F6CD04")
   :accent   (chroma-hex :hex "#E54F50"))
  "Duotone palette based on Nicolas Petton Emacs website."
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-default-palettes
  '(duotone-palette-blessed-dark
    duotone-palette-arch-light
    duotone-palette-arch-dark
    duotone-palette-blessed-light
    duotone-palette-candid-light
    duotone-palette-duotone-dark
    duotone-palette-duotone-light
    duotone-palette-earth-dark
    duotone-palette-emacs-light
    duotone-palette-mono-dark
    duotone-palette-mono-light
    duotone-palette-newspaper-light
    duotone-palette-sea-dark
    duotone-palette-sea-light
    duotone-palette-space-dark)
  "Duotone default palettes."
  :group 'duotone
  :type 'list)

(provide 'duotone-presets)
;;; duotone-presets.el ends here
