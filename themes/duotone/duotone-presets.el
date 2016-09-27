;;; Commentary:
;; https://github.com/smlombardi/duotone-blue-dark(require 'duotone-palette)

(defcustom duotone-palette-sea-light
  (make-instance
   'duotone-palette
   :uno    212
   :duo    160
   :uno-1  '(.76 .10)
   :uno-2  '(.62 .14)
   :uno-3  '(.55 .44)
   :uno-4  '(.15 .70)
   :duo-1  '(1.00 .28)
   :duo-2  '(.36 .42)
   :duo-3  '(.06 .80)
   :bg     '(.33 1.00)
   :accent '(.90 .40))
  "https://github.com/5310/duotone-bright-sea-syntax/blob/master/styles/colors.less"
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-stark-sea
  (make-instance
   'duotone-palette
   :uno    212
   :duo    160
   :fg  '(.88 .89)
   :uno-1  '(.99 .92)
   :uno-2  '(.88 .77)
   :uno-3  '(.24 .54)
   :uno-4  '(.12 .38)
   :duo-1  '(.79 .70)
   :duo-2  '(.52 .48)
   :duo-3  '(.06 .32)
   :bg     '(212 0 .15)
   :accent '(160 .95 .66))
  "https://github.com/5310/duotone-bright-sea-syntax/blob/master/styles/colors.less"
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-candid-light
  (make-instance
   'duotone-palette
   :uno      180
   :uno-2    '(.58 .17)
   :uno-1    '(.76 .10)
   :uno-2    '(.58 .34)
   :uno-3    '(.34 .50)
   :uno-4    '(.16 .66)
   :duo      70
   :duo-1    '(.92 .28)
   :duo-2    '(.46 .32)
   :duo-3    '(.06 .70)
   :bg       '(.12 .92)
   :accent   '(.80 .60))
  "https://github.com/tsangkenneth/duotone-light-blue-syntax/blob/master/styles/colors.less"
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-arch-light
  (make-instance
   'duotone-palette
   :fg       (chroma-hex :hex "#272a34")
   :bg       (chroma-hex :hex "#f4f5f6")
   :uno-1    (chroma-hex :hex "#14151a")
   :uno-2    (chroma-hex :hex "#272a34")
   :uno-3    (chroma-hex :hex "#343a46")
   :uno-4    (chroma-hex :hex "#828996")
   :duo-1    (chroma-hex :hex "#437fdc")
   :duo-2    (chroma-hex :hex "#5a92e3")
   :duo-3    (chroma-hex :hex "#757c8e")
   :accent   (chroma-hex :hex "#437fdc"))
  "Mono dark palette."
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-arch-dark
  (make-instance
   'duotone-palette
   :fg       (chroma-hex :hex "#ffffff")
   :bg       (chroma-hex :hex "#13141A")
   :uno-1    (chroma-hex :hex "#ffffff")
   :uno-2    (chroma-hex :hex "#C0C1C2")
   :uno-3    (chroma-hex :hex "#AAABAC")
   :uno-4    (chroma-hex :hex "#848381")
   :duo-1    (chroma-hex :hex "#6297FF")
   :duo-2    (chroma-hex :hex "#6496E5")
   :duo-3    (chroma-hex :hex "#4D71B1")
   :accent   (chroma-hex :hex "#FFDC00"))
  "Mono dark palette."
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-mono-light
  (make-instance
   'duotone-palette
   :bg       (chroma-hex :hex "#f2f2f2")
   :uno-1    (chroma-hex :hex "#000")
   :uno-2    (chroma-hex :hex "#333")
   :uno-3    (chroma-hex :hex "#666")
   :uno-4    (chroma-hex :hex "#999")
   :duo-1    (chroma-hex :hex "#111")
   :duo-2    (chroma-hex :hex "#444")
   :duo-3    (chroma-hex :hex "#777")
   :accent   (chroma-hex :hex "#DEDA5C"))
  "Mono dark palette."
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-mono-dark
  (make-instance
   'duotone-palette
   :bg       (chroma-hex :hex "#292A2A")
   :uno-1    (chroma-hex :hex "#eee")
   :uno-2    (chroma-hex :hex "#ccc")
   :uno-3    (chroma-hex :hex "#999")
   :uno-4    (chroma-hex :hex "#777")
   :duo-1    (chroma-hex :hex "#aaa")
   :duo-2    (chroma-hex :hex "#999")
   :duo-3    (chroma-hex :hex "#777")
   :accent   (chroma-hex :hex "#DEDA5C"))
  "Mono light palette."
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-earth-dark
  (make-instance
   'duotone-palette
   :uno    24
   :duo    42
   :fg     '(.24 .82)
   :uno-1  '(.99 .96)
   :uno-2  '(.24 .72)
   :uno-3  '(.12 .54)
   :uno-4  '(.08 .38)
   :duo-1  '(.99 .66)
   :duo-2  '(.44 .55)
   :duo-3  '(.24 .44)
   :bg     '(.10 .18)
   :accent '(1.00 .66))
  "Dark earth https://github.com/simurai/duotone-dark-earth-syntax"
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-sea-dark
  (make-instance
   'duotone-palette
   :uno      212
   :duo      160
   :fg       '(.88 .88)
   :uno-1    '(.99 .92)
   :uno-2    '(.88 .77)
   :uno-3    '(.24 .54)
   :uno-4    '(.12 .38)
   :duo-1    '(.99 .70)
   :duo-2    '(.72 .48)
   :duo-3    '(.12 .32)
   :bg       '(212 .24 .15)
   :accent   '(160 1.00 .66))
  "Duotone light palette.
https://github.com/simurai/duotone-dark-sea-syntax."
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-amethyst-dark
  (make-instance
   'duotone-palette
   :uno      252
   :duo      270
   :fg      '(.98 .96)
   :uno-1    '(1.0 .96)
   :uno-2    '(.98 .86)
   :uno-3    '(.96 .78)
   :uno-4    '(.12 .46)
   :duo-1    '(1.0 .80)
   :duo-2    '(.72 .62)
   :duo-3    '(.06 .46)
   :bg       '(270 .14 .18)
   :accent   '(270 1.0 .66))
  "Duotone amethyst dark.
https://github.com/bynines/duotone-dark-amethyst-syntax"
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-blue-dark
  (make-instance
   'duotone-palette
   :uno    205
   :duo    40
   :fg     '(.98 1.0)
   :uno-1  '(.95 .92)
   :uno-2  '(.95 .86)
   :uno-3  '(.75 .75)
   :uno-4  '(.20 .50)
   :duo-1  '(.70 .75)
   :duo-2  '(.72 .62)
   :duo-3  '(.15 .55)
   :bg     '(205 .12 .18)
   :accent '(40 1.00 .66))
  "Duotone blue dark palette.
https://github.com/smlombardi/duotone-blue-dark"
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-space-dark
  (make-instance
   'duotone-palette
   :uno    240
   :duo    20
   :fg     '(.28 .90)
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
   :fg       '(0.65 0.15)
   :uno-1    '(0.76 0.13)
   :uno-2    '(0.65 0.35)
   :uno-3    '(0.34 0.54)
   :uno-4    '(0.16 0.66)
   :duo      220
   :duo-1    '(0.92 0.3)
   :duo-2    '(0.46 0.52)
   :duo-3    '(0.23 0.52)
   :bg       (chroma-hex :hex "#faf8f3"))
  "Duotone light palette."
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-duotone-dark
  (make-instance
   'duotone-palette
   :uno    250
   :fg     '(.98 .92)
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
   :fg       '(.58 .1)
   :uno-1    '(.76 .1)
   :uno-2    '(.58 .24)
   :uno-3    '(.34 .54)
   :uno-4    '(.16 .66)
   :duo      310
   :duo-1    '(.92 .28)
   :duo-2    '(.46 .52)
   :duo-3    '(.6 .7)
   :bg       '(.33 .97)
   :accent   '(.80 .60))
  "Duotone palette blessed-light."
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-watch-light
  (make-instance
   'duotone-palette
   :uno-1    (chroma-hex :hex "#2d2006")
   :uno-2    (chroma-hex :hex "#896724")
   :uno-3    (chroma-hex :hex "#B29762")
   :uno-4    (chroma-hex :hex "#B6ad9a")
   :duo-1    (chroma-hex :hex "#065289")
   :duo-2    (chroma-hex :hex "#718ecd")
   :duo-3    (chroma-hex :hex "#aeb3b7")
   :bg       (chroma-hex :hex "#FAF8F5")
   :accent   (chroma-hex :hex "#447EBB"))
  "Duotone palette two-watch dark."
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-two-watch-dark
  (make-instance
   'duotone-palette
   :uno-1    (chroma-hex :hex "#d6e9ff")
   :uno-2    (chroma-hex :hex "#abb2bf")
   :uno-3    (chroma-hex :hex "#6e88a6")
   :uno-4    (chroma-hex :hex "#55606d")
   :duo-1    (chroma-hex :hex "#c8ae9d")
   :duo-2    (chroma-hex :hex "#e06c75")
   :duo-3    (chroma-hex :hex "#dd672c")
   :bg       (chroma-hex :hex "#282c34")
   :accent   (chroma-hex :hex "#56b6c2"))
  "Duotone palette two-watch dark."
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
   :accent   (chroma-hex :hex "#E54F50"))
  "Duotone palette based on Nicolas Petton Emacs website."
  :group 'duotone
  :type 'duotone-palette)

(defcustom duotone-palette-light-rose
  (make-instance
   'duotone-palette
   :bg       (chroma-hex :hex "#E8E1DD")
   :fg       (chroma-hex :hex "#2F0028")
   :uno-1    (chroma-hex :hex "#07004F")
   :uno-2    (chroma-hex :hex "#190038")
   :uno-3    (chroma-hex :hex "#25003A")
   :uno-4    (chroma-hex :hex "#2F0028")
   :duo-1    (chroma-hex :hex "#7A004A")
   :duo-2    (chroma-hex :hex "#610044")
   :duo-3    (chroma-hex :hex "#470038")
   :accent   (chroma-hex :hex "#981C1C"))
  "Duotone palette based on Nicolas Petton Emacs website."
  :group 'duotone
  :type 'duotone-palette)


(defcustom duotone-palette-default-palettes
  '(duotone-palette-arch-dark
    duotone-palette-arch-light
    duotone-palette-amethyst-dark
    duotone-palette-blessed-light
    duotone-palette-blue-dark
    duotone-palette-candid-light
    duotone-palette-light-rose
    duotone-palette-duotone-dark
    duotone-palette-duotone-light
    duotone-palette-earth-dark
    duotone-palette-emacs-light
    duotone-palette-mono-dark
    duotone-palette-mono-light
    duotone-palette-newspaper-light
    duotone-palette-sea-dark
    duotone-palette-sea-light
    duotone-palette-space-dark
    duotone-palette-stark-sea
    duotone-palette-two-watch-dark
    duotone-palette-watch-light)
  "Duotone default palettes."
  :group 'duotone
  :type 'list)

(provide 'duotone-presets)
;;; duotone-presets.el ends here
