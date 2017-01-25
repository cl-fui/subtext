(in-package #:stext)

;;; This is a bit of a mess.  Keys come in as gtkkey codes representing
;;; keyboard codes.  These are mapped across keymaps; at some point
;;; they will need to be converted to lisp codes.  Gtkkey codes seem
;;; to not match the lisp character set, and Lisp implementations are
;;; not very portable about printing non-ascii characters (CCL does not
;;; seem to recognize (code-char #x2399) as PRINT_SCREEN_SYMBOL; while
;;; gtk reports it as #xFF61.
;;;
;;; Since at this stage we have to handle keystrokes across all platforms
;;; we shall rely on GTK codes, and translate them to Lisp chars only;
;;; as necessary.

;;;   Note that #'equalp is used, making name lookup case-insensitive.
(defvar *gtkcode-gtkname-map* (make-hash-table))
(defvar *gtkname-gtkcode-map* (make-hash-table :test 'equal))

(defun define-gtkcode (gtkcode gtkname)
  "Define a mapping from a gtkname to a gtkcode."
  (setf (gethash gtkcode *gtkcode-gtkname-map*) gtkname
        (gethash gtkname *gtkname-gtkcode-map*) gtkcode))

(defun gtkname->gtkcode (gtkname)
  "Return the gtkcode corresponding to NAME or nil"
  (gethash gtkname *gtkname-gtkcode-map*))

(defun gtkcode->gtkname (gtkcode)
  "Return the name corresponding to GTKCODE or nil"
  (gethash gtkcode *gtkcode-gtkname-map*))

(define-gtkcode #xffffff "VoidSymbol")   ;Void symbol

(defun modifier-p (gtkcode)
  "check if the gtkcode is a modifier (shift, etc)"
  (and (>= gtkcode #xffe1)
       (<= gtkcode #xffee)))

;;; ASCII
;;;
(define-gtkcode #x0020 "SPC")     ;U+0020 SPACE
(define-gtkcode #x0021 "!")       ;U+0021 EXCLAMATION MAR
(define-gtkcode #x0022 "\"")      ;U+0022 QUOTATION MARK
(define-gtkcode #x0023 "#")       ;U+0023 NUMBER SIGN
(define-gtkcode #x0024 "$")       ;U+0024 DOLLAR SIGN
(define-gtkcode #x0025 "%")       ;U+0025 PERCENT SIGN
(define-gtkcode #x0026 "&")       ;U+0026 AMPERSAND
(define-gtkcode #x0027 "'")       ;U+0027 APOSTROPHE
(define-gtkcode #x0028 "(")      ;U+0028 LEFT PARENTHESIS
(define-gtkcode #x0029 ")")     ;U+0029 RIGHT PARENTHESIS
(define-gtkcode #x002a "*")       ;U+002A ASTERISK
(define-gtkcode #x002b "+")           ;U+002B PLUS SIGN
(define-gtkcode #x002c ",")          ;U+002C COMMA
(define-gtkcode #x002d "-")          ;U+002D HYPHEN-MINUS
(define-gtkcode #x002e ".")         ;U+002E FULL STOP
(define-gtkcode #x002f "/")          ;U+002F SOLIDUS
(define-gtkcode #x0030 "0")              ;U+0030 DIGIT ZERO
(define-gtkcode #x0031 "1")              ;U+0031 DIGIT ONE
(define-gtkcode #x0032 "2")              ;U+0032 DIGIT TWO
(define-gtkcode #x0033 "3")              ;U+0033 DIGIT THREE
(define-gtkcode #x0034 "4")              ;U+0034 DIGIT FOUR
(define-gtkcode #x0035 "5")              ;U+0035 DIGIT FIVE
(define-gtkcode #x0036 "6")              ;U+0036 DIGIT SIX
(define-gtkcode #x0037 "7")              ;U+0037 DIGIT SEVEN
(define-gtkcode #x0038 "8")              ;U+0038 DIGIT EIGHT
(define-gtkcode #x0039 "9")              ;U+0039 DIGIT NINE
(define-gtkcode #x003a ":")          ;U+003A COLON
(define-gtkcode #x003b ";")      ;U+003B SEMICOLON
(define-gtkcode #x003c "<")           ;U+003C LESS-THAN SIGN
(define-gtkcode #x003d "=")          ;U+003D EQUALS SIGN
(define-gtkcode #x003e ">")        ;U+003E GREATER-THAN SIGN
(define-gtkcode #x003f "?")       ;U+003F QUESTION MARK
(define-gtkcode #x0040 "@")             ;U+0040 COMMERCIAL AT
(define-gtkcode #x0041 "A")              ;U+0041 LATIN CAPITAL LETTER A
(define-gtkcode #x0042 "B")              ;U+0042 LATIN CAPITAL LETTER B
(define-gtkcode #x0043 "C")              ;U+0043 LATIN CAPITAL LETTER C
(define-gtkcode #x0044 "D")              ;U+0044 LATIN CAPITAL LETTER D
(define-gtkcode #x0045 "E")              ;U+0045 LATIN CAPITAL LETTER E
(define-gtkcode #x0046 "F")              ;U+0046 LATIN CAPITAL LETTER F
(define-gtkcode #x0047 "G")              ;U+0047 LATIN CAPITAL LETTER G
(define-gtkcode #x0048 "H")              ;U+0048 LATIN CAPITAL LETTER H
(define-gtkcode #x0049 "I")              ;U+0049 LATIN CAPITAL LETTER I
(define-gtkcode #x004a "J")              ;U+004A LATIN CAPITAL LETTER J
(define-gtkcode #x004b "K")              ;U+004B LATIN CAPITAL LETTER K
(define-gtkcode #x004c "L")              ;U+004C LATIN CAPITAL LETTER L
(define-gtkcode #x004d "M")              ;U+004D LATIN CAPITAL LETTER M
(define-gtkcode #x004e "N")              ;U+004E LATIN CAPITAL LETTER N
(define-gtkcode #x004f "O")              ;U+004F LATIN CAPITAL LETTER O
(define-gtkcode #x0050 "P")              ;U+0050 LATIN CAPITAL LETTER P
(define-gtkcode #x0051 "Q")              ;U+0051 LATIN CAPITAL LETTER Q
(define-gtkcode #x0052 "R")              ;U+0052 LATIN CAPITAL LETTER R
(define-gtkcode #x0053 "S")              ;U+0053 LATIN CAPITAL LETTER S
(define-gtkcode #x0054 "T")              ;U+0054 LATIN CAPITAL LETTER T
(define-gtkcode #x0055 "U")              ;U+0055 LATIN CAPITAL LETTER U
(define-gtkcode #x0056 "V")              ;U+0056 LATIN CAPITAL LETTER V
(define-gtkcode #x0057 "W")              ;U+0057 LATIN CAPITAL LETTER W
(define-gtkcode #x0058 "X")              ;U+0058 LATIN CAPITAL LETTER X
(define-gtkcode #x0059 "Y")              ;U+0059 LATIN CAPITAL LETTER Y
(define-gtkcode #x005a "Z")              ;U+005A LATIN CAPITAL LETTER Z
(define-gtkcode #x005b "[")    ;U+005B LEFT SQUARE BRACKET
(define-gtkcode #x005c "\\")      ;U+005C REVERSE SOLIDUS
(define-gtkcode #x005d "]")   ;U+005D RIGHT SQUARE BRACKET
(define-gtkcode #x005e "^")    ;U+005E CIRCUMFLEX ACCENT
(define-gtkcode #x005f "_")     ;U+005F LOW LINE
(define-gtkcode #x0060 "`")          ;U+0060 GRAVE ACCENT
(define-gtkcode #x0061 "a")              ;U+0061 LATIN SMALL LETTER A
(define-gtkcode #x0062 "b")              ;U+0062 LATIN SMALL LETTER B
(define-gtkcode #x0063 "c")              ;U+0063 LATIN SMALL LETTER C
(define-gtkcode #x0064 "d")              ;U+0064 LATIN SMALL LETTER D
(define-gtkcode #x0065 "e")              ;U+0065 LATIN SMALL LETTER E
(define-gtkcode #x0066 "f")              ;U+0066 LATIN SMALL LETTER F
(define-gtkcode #x0067 "g")              ;U+0067 LATIN SMALL LETTER G
(define-gtkcode #x0068 "h")              ;U+0068 LATIN SMALL LETTER H
(define-gtkcode #x0069 "i")              ;U+0069 LATIN SMALL LETTER I
(define-gtkcode #x006a "j")              ;U+006A LATIN SMALL LETTER J
(define-gtkcode #x006b "k")              ;U+006B LATIN SMALL LETTER K
(define-gtkcode #x006c "l")              ;U+006C LATIN SMALL LETTER L
(define-gtkcode #x006d "m")              ;U+006D LATIN SMALL LETTER M
(define-gtkcode #x006e "n")              ;U+006E LATIN SMALL LETTER N
(define-gtkcode #x006f "o")              ;U+006F LATIN SMALL LETTER O
(define-gtkcode #x0070 "p")              ;U+0070 LATIN SMALL LETTER P
(define-gtkcode #x0071 "q")              ;U+0071 LATIN SMALL LETTER Q
(define-gtkcode #x0072 "r")              ;U+0072 LATIN SMALL LETTER R
(define-gtkcode #x0073 "s")              ;U+0073 LATIN SMALL LETTER S
(define-gtkcode #x0074 "t")              ;U+0074 LATIN SMALL LETTER T
(define-gtkcode #x0075 "u")              ;U+0075 LATIN SMALL LETTER U
(define-gtkcode #x0076 "v")              ;U+0076 LATIN SMALL LETTER V
(define-gtkcode #x0077 "w")              ;U+0077 LATIN SMALL LETTER W
(define-gtkcode #x0078 "x")              ;U+0078 LATIN SMALL LETTER X
(define-gtkcode #x0079 "y")              ;U+0079 LATIN SMALL LETTER Y
(define-gtkcode #x007a "z")              ;U+007A LATIN SMALL LETTER Z
(define-gtkcode #x007b "{")      ;U+007B LEFT CURLY BRACKET
(define-gtkcode #x007c "|")            ;U+007C VERTICAL LINE
(define-gtkcode #x007d "}")     ;U+007D RIGHT CURLY BRACKET
(define-gtkcode #x007e "~")     ;U+007E TILDE

(define-gtkcode #xff08 "BS")      ;Back space, back char
(define-gtkcode #xff09 "TAB")
(define-gtkcode #xff0a "LINEFEED")       ;Linefeed, LF
(define-gtkcode #xff0b "CLEAR")
(define-gtkcode #xff0d "RET")  ;Return, enter
(define-gtkcode #xff13 "PAUSE")          ;Pause, BRK
(define-gtkcode #xff14 "SCROLL-LOCK")
(define-gtkcode #xff15 "SYS-REQ")
(define-gtkcode #xff1b "ESC")
(define-gtkcode #xffff "DEL")     ;Delete, rubout

(define-gtkcode #xff50 "HOME")
(define-gtkcode #xff51 "LEFT")           ;Move left, left arrow
(define-gtkcode #xff52 "UP")             ;Move up, up arrow
(define-gtkcode #xff53 "RIGHT")          ;Move right, right arrow
(define-gtkcode #xff54 "DOWN")           ;Move down, down arrow
(define-gtkcode #xff55 "PAGE-UP")
(define-gtkcode #xff56 "PAGE-DOWN")
(define-gtkcode #xff57 "END")            ;
;(define-gtkcode #xff58 "Begin")          ;BOL
;(define-gtkcode #xff60 "Select")         ;Select, mark
;(define-gtkcode #xff61 "Print")          ;TAKEN
;(define-gtkcode #xff62 "Execute")        ;Execute, run, do
(define-gtkcode #xff63 "INSERT")         ;Insert, insert here
;(define-gtkcode #xff65 "Undo")
;(define-gtkcode #xff66 "Redo")           ;Redo, again
(define-gtkcode #xff67 "MENU")
;(define-gtkcode #xff68 "Find")           ;Find, search
;(define-gtkcode #xff69 "Cancel")         ;Cancel, stop, abort, exit
;(define-gtkcode #xff6a "Help")           ;Help
;(define-gtkcode #xff6b "BREAK")          ;same as PAUSE on my kbd
(define-gtkcode #xff7f "NUM-LOCK")
(define-gtkcode #xff80 "KP_SPACE")       ;Space
(define-gtkcode #xff89 "KP_TAB")
(define-gtkcode #xff8d "KP_ENTER")       ;Enter
(define-gtkcode #xff91 "KP_F1")          ;PF1, KP_A, ...
(define-gtkcode #xff92 "KP_F2")
(define-gtkcode #xff93 "KP_F3")
(define-gtkcode #xff94 "KP_F4")
(define-gtkcode #xff95 "KP_HOME")
(define-gtkcode #xff96 "KP_LEFT")
(define-gtkcode #xff97 "KP_UP")
(define-gtkcode #xff98 "KP_RIGHT")
(define-gtkcode #xff99 "KP_DOWN")
(define-gtkcode #xff9a "KP_PRIOR")
(define-gtkcode #xff9a "KP_PAGE_UP")
(define-gtkcode #xff9b "KP_NEXT")
(define-gtkcode #xff9b "KP_PAGE_DOWN")
(define-gtkcode #xff9c "KP_END")
(define-gtkcode #xff9d "KP_BEGIN")
(define-gtkcode #xff9e "KP_INSERT")
(define-gtkcode #xff9f "KP_DELETE")
(define-gtkcode #xffbd "KP_EQUAL")       ;Equals
(define-gtkcode #xffaa "KP_MULTIPLY")
(define-gtkcode #xffab "KP_ADD")
(define-gtkcode #xffac "KP_SEPARATOR")   ;Separator, often comma
(define-gtkcode #xffad "KP_SUBTRACT")
(define-gtkcode #xffae "KP_DECIMAL")
(define-gtkcode #xffaf "KP_DIVIDE")
(define-gtkcode #xffb0 "KP_0")
(define-gtkcode #xffb1 "KP_1")
(define-gtkcode #xffb2 "KP_2")
(define-gtkcode #xffb3 "KP_3")
(define-gtkcode #xffb4 "KP_4")
(define-gtkcode #xffb5 "KP_5")
(define-gtkcode #xffb6 "KP_6")
(define-gtkcode #xffb7 "KP_7")
(define-gtkcode #xffb8 "KP_8")
(define-gtkcode #xffb9 "KP_9")

(define-gtkcode #xffe1 "SHIFT-L")        ;Left shift
(define-gtkcode #xffe2 "SHIFT-R")        ;Right shift
(define-gtkcode #xffe3 "CONTROL-L")      ;Left control
(define-gtkcode #xffe4 "CONTROL-R")      ;Right control
(define-gtkcode #xffe5 "CAPS-LOCK")      ;Caps lock
;(define-gtkcode #xffe6 "SHIFT-LOCK")     ;Shift lock not on my kbd
;(define-gtkcode #xffe7 "Meta_L")         ;Left meta
;(define-gtkcode #xffe8 "Meta_R")         ;Right meta
(define-gtkcode #xffe9 "ALT-L")          ;Left alt
(define-gtkcode #xffea "ALT-R")          ;Right alt
(define-gtkcode #xffeb "SUPER_L")        ;Left super (WINDOWS KEY)
(define-gtkcode #xffec "SUPER_R")        ;Right super (WINDOWS KEY)
;(define-gtkcode #xffed "Hyper_L")        ;Left hyper
;(define-gtkcode #xffee "Hyper_R")        ;Right hyper
(define-gtkcode #xffbe "F1")
(define-gtkcode #xffbf "F2")
(define-gtkcode #xffc0 "F3")
(define-gtkcode #xffc1 "F4")
(define-gtkcode #xffc2 "F5")
(define-gtkcode #xffc3 "F6")
(define-gtkcode #xffc4 "F7")
(define-gtkcode #xffc5 "F8")
(define-gtkcode #xffc6 "F9")
(define-gtkcode #xffc7 "F10")
(define-gtkcode #xffc8 "F11")
(define-gtkcode #xffc9 "F12")
(define-gtkcode #xffca "F13")
(define-gtkcode #xffcb "F14")
(define-gtkcode #xffcc "F15")
(define-gtkcode #xffcd "F16")
(define-gtkcode #xffce "F17")
(define-gtkcode #xffcf "F18")
(define-gtkcode #xffd0 "F19")
(define-gtkcode #xffd1 "F20")
(define-gtkcode #xffd2 "F21")
(define-gtkcode #xffd3 "F22")
(define-gtkcode #xffd4 "F23")
(define-gtkcode #xffd5 "F24")
(define-gtkcode #xffd6 "F25")
(define-gtkcode #xffd7 "F26")
(define-gtkcode #xffd8 "F27")
(define-gtkcode #xffd9 "F28")
(define-gtkcode #xffda "F29")
(define-gtkcode #xffdb "F30")
(define-gtkcode #xffdc "F31")
(define-gtkcode #xffdd "F32")
(define-gtkcode #xffde "F33")
(define-gtkcode #xffdf "F34")
(define-gtkcode #xffe0 "F35")



;;; WEIRD AND UNTESTED
#|
(define-gtkcode #x00a0 "nobreakspace")   ;U+00A0 NO-BREAK SPACE
(define-gtkcode #x00a1 "exclamdown")  ;U+00A1 INVERTED EXCLAMATION MARK
(define-gtkcode #x00a2 "cent")           ;U+00A2 CENT SIGN
(define-gtkcode #x00a3 "sterling")       ;U+00A3 POUND SIGN
(define-gtkcode #x00a4 "currency")       ;U+00A4 CURRENCY SIGN
(define-gtkcode #x00a5 "yen")            ;U+00A5 YEN SIGN
(define-gtkcode #x00a6 "brokenbar")      ;U+00A6 BROKEN BAR
(define-gtkcode #x00a7 "section")        ;U+00A7 SECTION SIGN
(define-gtkcode #x00a8 "diaeresis")      ;U+00A8 DIAERESIS
(define-gtkcode #x00a9 "copyright")      ;U+00A9 COPYRIGHT SIGN
(define-gtkcode #x00aa "ordfeminine") ;U+00AA FEMININE ORDINAL INDICATOR
(define-gtkcode #x00ab "guillemotleft") ;U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
(define-gtkcode #x00ac "notsign")        ;U+00AC NOT SIGN
(define-gtkcode #x00ad "hyphen")         ;U+00AD SOFT HYPHEN
(define-gtkcode #x00ae "registered")     ;U+00AE REGISTERED SIGN
(define-gtkcode #x00af "macron")         ;U+00AF MACRON
(define-gtkcode #x00b0 "degree")         ;U+00B0 DEGREE SIGN
(define-gtkcode #x00b1 "plusminus")      ;U+00B1 PLUS-MINUS SIGN
(define-gtkcode #x00b2 "twosuperior")    ;U+00B2 SUPERSCRIPT TWO
(define-gtkcode #x00b3 "threesuperior")  ;U+00B3 SUPERSCRIPT THREE
(define-gtkcode #x00b4 "acute")          ;U+00B4 ACUTE ACCENT
(define-gtkcode #x00b5 "mu")             ;U+00B5 MICRO SIGN
(define-gtkcode #x00b6 "paragraph")      ;U+00B6 PILCROW SIGN
(define-gtkcode #x00b7 "periodcentered") ;U+00B7 MIDDLE DOT
(define-gtkcode #x00b8 "cedilla")        ;U+00B8 CEDILLA
(define-gtkcode #x00b9 "onesuperior")    ;U+00B9 SUPERSCRIPT ONE
(define-gtkcode #x00ba "masculine") ;U+00BA MASCULINE ORDINAL INDICATOR
(define-gtkcode #x00bb "guillemotright") ;U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
(define-gtkcode #x00bc "onequarter") ;U+00BC VULGAR FRACTION ONE QUARTER
(define-gtkcode #x00bd "onehalf")      ;U+00BD VULGAR FRACTION ONE HALF
(define-gtkcode #x00be "threequarters") ;U+00BE VULGAR FRACTION THREE QUARTERS
(define-gtkcode #x00bf "questiondown")   ;U+00BF INVERTED QUESTION MARK
(define-gtkcode #x00c0 "Agrave") ;U+00C0 LATIN CAPITAL LETTER A WITH GRAVE
(define-gtkcode #x00c1 "Aacute") ;U+00C1 LATIN CAPITAL LETTER A WITH ACUTE
(define-gtkcode #x00c2 "Acircumflex") ;U+00C2 LATIN CAPITAL LETTER A WITH CIRCUMFLEX
(define-gtkcode #x00c3 "Atilde") ;U+00C3 LATIN CAPITAL LETTER A WITH TILDE
(define-gtkcode #x00c4 "Adiaeresis") ;U+00C4 LATIN CAPITAL LETTER A WITH DIAERESIS
(define-gtkcode #x00c5 "Aring") ;U+00C5 LATIN CAPITAL LETTER A WITH RING ABOVE
(define-gtkcode #x00c6 "AE")            ;U+00C6 LATIN CAPITAL LETTER AE
(define-gtkcode #x00c7 "Ccedilla") ;U+00C7 LATIN CAPITAL LETTER C WITH CEDILLA
(define-gtkcode #x00c8 "Egrave") ;U+00C8 LATIN CAPITAL LETTER E WITH GRAVE
(define-gtkcode #x00c9 "Eacute") ;U+00C9 LATIN CAPITAL LETTER E WITH ACUTE
(define-gtkcode #x00ca "Ecircumflex") ;U+00CA LATIN CAPITAL LETTER E WITH CIRCUMFLEX
(define-gtkcode #x00cb "Ediaeresis") ;U+00CB LATIN CAPITAL LETTER E WITH DIAERESIS
(define-gtkcode #x00cc "Igrave") ;U+00CC LATIN CAPITAL LETTER I WITH GRAVE
(define-gtkcode #x00cd "Iacute") ;U+00CD LATIN CAPITAL LETTER I WITH ACUTE
(define-gtkcode #x00ce "Icircumflex") ;U+00CE LATIN CAPITAL LETTER I WITH CIRCUMFLEX
(define-gtkcode #x00cf "Idiaeresis") ;U+00CF LATIN CAPITAL LETTER I WITH DIAERESIS
(define-gtkcode #x00d0 "ETH")          ;U+00D0 LATIN CAPITAL LETTER ETH
(define-gtkcode #x00d0 "Eth")            ;deprecated
(define-gtkcode #x00d1 "Ntilde") ;U+00D1 LATIN CAPITAL LETTER N WITH TILDE
(define-gtkcode #x00d2 "Ograve") ;U+00D2 LATIN CAPITAL LETTER O WITH GRAVE
(define-gtkcode #x00d3 "Oacute") ;U+00D3 LATIN CAPITAL LETTER O WITH ACUTE
(define-gtkcode #x00d4 "Ocircumflex") ;U+00D4 LATIN CAPITAL LETTER O WITH CIRCUMFLEX
(define-gtkcode #x00d5 "Otilde") ;U+00D5 LATIN CAPITAL LETTER O WITH TILDE
(define-gtkcode #x00d6 "Odiaeresis") ;U+00D6 LATIN CAPITAL LETTER O WITH DIAERESIS
(define-gtkcode #x00d7 "multiply")       ;U+00D7 MULTIPLICATION SIGN
(define-gtkcode #x00d8 "Oslash") ;U+00D8 LATIN CAPITAL LETTER O WITH STROKE
(define-gtkcode #x00d8 "Ooblique") ;U+00D8 LATIN CAPITAL LETTER O WITH STROKE
(define-gtkcode #x00d9 "Ugrave") ;U+00D9 LATIN CAPITAL LETTER U WITH GRAVE
(define-gtkcode #x00da "Uacute") ;U+00DA LATIN CAPITAL LETTER U WITH ACUTE
(define-gtkcode #x00db "Ucircumflex") ;U+00DB LATIN CAPITAL LETTER U WITH CIRCUMFLEX
(define-gtkcode #x00dc "Udiaeresis") ;U+00DC LATIN CAPITAL LETTER U WITH DIAERESIS
(define-gtkcode #x00dd "Yacute") ;U+00DD LATIN CAPITAL LETTER Y WITH ACUTE
(define-gtkcode #x00de "THORN")      ;U+00DE LATIN CAPITAL LETTER THORN
(define-gtkcode #x00de "Thorn")          ;deprecated
(define-gtkcode #x00df "ssharp")     ;U+00DF LATIN SMALL LETTER SHARP S
(define-gtkcode #x00e0 "agrave") ;U+00E0 LATIN SMALL LETTER A WITH GRAVE
(define-gtkcode #x00e1 "aacute") ;U+00E1 LATIN SMALL LETTER A WITH ACUTE
(define-gtkcode #x00e2 "acircumflex") ;U+00E2 LATIN SMALL LETTER A WITH CIRCUMFLEX
(define-gtkcode #x00e3 "atilde") ;U+00E3 LATIN SMALL LETTER A WITH TILDE
(define-gtkcode #x00e4 "adiaeresis") ;U+00E4 LATIN SMALL LETTER A WITH DIAERESIS
(define-gtkcode #x00e5 "aring") ;U+00E5 LATIN SMALL LETTER A WITH RING ABOVE
(define-gtkcode #x00e6 "ae")             ;U+00E6 LATIN SMALL LETTER AE
(define-gtkcode #x00e7 "ccedilla") ;U+00E7 LATIN SMALL LETTER C WITH CEDILLA
(define-gtkcode #x00e8 "egrave") ;U+00E8 LATIN SMALL LETTER E WITH GRAVE
(define-gtkcode #x00e9 "eacute") ;U+00E9 LATIN SMALL LETTER E WITH ACUTE
(define-gtkcode #x00ea "ecircumflex") ;U+00EA LATIN SMALL LETTER E WITH CIRCUMFLEX
(define-gtkcode #x00eb "ediaeresis") ;U+00EB LATIN SMALL LETTER E WITH DIAERESIS
(define-gtkcode #x00ec "igrave") ;U+00EC LATIN SMALL LETTER I WITH GRAVE
(define-gtkcode #x00ed "iacute") ;U+00ED LATIN SMALL LETTER I WITH ACUTE
(define-gtkcode #x00ee "icircumflex") ;U+00EE LATIN SMALL LETTER I WITH CIRCUMFLEX
(define-gtkcode #x00ef "idiaeresis") ;U+00EF LATIN SMALL LETTER I WITH DIAERESIS
(define-gtkcode #x00f0 "eth")            ;U+00F0 LATIN SMALL LETTER ETH
(define-gtkcode #x00f1 "ntilde") ;U+00F1 LATIN SMALL LETTER N WITH TILDE
(define-gtkcode #x00f2 "ograve") ;U+00F2 LATIN SMALL LETTER O WITH GRAVE
(define-gtkcode #x00f3 "oacute") ;U+00F3 LATIN SMALL LETTER O WITH ACUTE
(define-gtkcode #x00f4 "ocircumflex") ;U+00F4 LATIN SMALL LETTER O WITH CIRCUMFLEX
(define-gtkcode #x00f5 "otilde") ;U+00F5 LATIN SMALL LETTER O WITH TILDE
(define-gtkcode #x00f6 "odiaeresis") ;U+00F6 LATIN SMALL LETTER O WITH DIAERESIS
(define-gtkcode #x00f7 "division")       ;U+00F7 DIVISION SIGN
(define-gtkcode #x00f8 "oslash") ;U+00F8 LATIN SMALL LETTER O WITH STROKE
(define-gtkcode #x00f8 "ooblique") ;U+00F8 LATIN SMALL LETTER O WITH STROKE
(define-gtkcode #x00f9 "ugrave") ;U+00F9 LATIN SMALL LETTER U WITH GRAVE
(define-gtkcode #x00fa "uacute") ;U+00FA LATIN SMALL LETTER U WITH ACUTE
(define-gtkcode #x00fb "ucircumflex") ;U+00FB LATIN SMALL LETTER U WITH CIRCUMFLEX
(define-gtkcode #x00fc "udiaeresis") ;U+00FC LATIN SMALL LETTER U WITH DIAERESIS
(define-gtkcode #x00fd "yacute") ;U+00FD LATIN SMALL LETTER Y WITH ACUTE
(define-gtkcode #x00fe "thorn")        ;U+00FE LATIN SMALL LETTER THORN
(define-gtkcode #x00ff "ydiaeresis") ;U+00FF LATIN SMALL LETTER Y WITH DIAERESIS
(define-gtkcode #x01a1 "Aogonek") ;U+0104 LATIN CAPITAL LETTER A WITH OGONEK
(define-gtkcode #x01a2 "breve")          ;U+02D8 BREVE
(define-gtkcode #x01a3 "Lstroke") ;U+0141 LATIN CAPITAL LETTER L WITH STROKE
(define-gtkcode #x01a5 "Lcaron") ;U+013D LATIN CAPITAL LETTER L WITH CARON
(define-gtkcode #x01a6 "Sacute") ;U+015A LATIN CAPITAL LETTER S WITH ACUTE
(define-gtkcode #x01a9 "Scaron") ;U+0160 LATIN CAPITAL LETTER S WITH CARON
(define-gtkcode #x01aa "Scedilla") ;U+015E LATIN CAPITAL LETTER S WITH CEDILLA
(define-gtkcode #x01ab "Tcaron") ;U+0164 LATIN CAPITAL LETTER T WITH CARON
(define-gtkcode #x01ac "Zacute") ;U+0179 LATIN CAPITAL LETTER Z WITH ACUTE
(define-gtkcode #x01ae "Zcaron") ;U+017D LATIN CAPITAL LETTER Z WITH CARON
(define-gtkcode #x01af "Zabovedot") ;U+017B LATIN CAPITAL LETTER Z WITH DOT ABOVE
(define-gtkcode #x01b1 "aogonek") ;U+0105 LATIN SMALL LETTER A WITH OGONEK
(define-gtkcode #x01b2 "ogonek")         ;U+02DB OGONEK
(define-gtkcode #x01b3 "lstroke") ;U+0142 LATIN SMALL LETTER L WITH STROKE
(define-gtkcode #x01b5 "lcaron") ;U+013E LATIN SMALL LETTER L WITH CARON
(define-gtkcode #x01b6 "sacute") ;U+015B LATIN SMALL LETTER S WITH ACUTE
(define-gtkcode #x01b7 "caron")          ;U+02C7 CARON
(define-gtkcode #x01b9 "scaron") ;U+0161 LATIN SMALL LETTER S WITH CARON
(define-gtkcode #x01ba "scedilla") ;U+015F LATIN SMALL LETTER S WITH CEDILLA
(define-gtkcode #x01bb "tcaron") ;U+0165 LATIN SMALL LETTER T WITH CARON
(define-gtkcode #x01bc "zacute") ;U+017A LATIN SMALL LETTER Z WITH ACUTE
(define-gtkcode #x01bd "doubleacute")    ;U+02DD DOUBLE ACUTE ACCENT
(define-gtkcode #x01be "zcaron") ;U+017E LATIN SMALL LETTER Z WITH CARON
(define-gtkcode #x01bf "zabovedot") ;U+017C LATIN SMALL LETTER Z WITH DOT ABOVE
(define-gtkcode #x01c0 "Racute") ;U+0154 LATIN CAPITAL LETTER R WITH ACUTE
(define-gtkcode #x01c3 "Abreve") ;U+0102 LATIN CAPITAL LETTER A WITH BREVE
(define-gtkcode #x01c5 "Lacute") ;U+0139 LATIN CAPITAL LETTER L WITH ACUTE
(define-gtkcode #x01c6 "Cacute") ;U+0106 LATIN CAPITAL LETTER C WITH ACUTE
(define-gtkcode #x01c8 "Ccaron") ;U+010C LATIN CAPITAL LETTER C WITH CARON
(define-gtkcode #x01ca "Eogonek") ;U+0118 LATIN CAPITAL LETTER E WITH OGONEK
(define-gtkcode #x01cc "Ecaron") ;U+011A LATIN CAPITAL LETTER E WITH CARON
(define-gtkcode #x01cf "Dcaron") ;U+010E LATIN CAPITAL LETTER D WITH CARON
(define-gtkcode #x01d0 "Dstroke") ;U+0110 LATIN CAPITAL LETTER D WITH STROKE
(define-gtkcode #x01d1 "Nacute") ;U+0143 LATIN CAPITAL LETTER N WITH ACUTE
(define-gtkcode #x01d2 "Ncaron") ;U+0147 LATIN CAPITAL LETTER N WITH CARON
(define-gtkcode #x01d5 "Odoubleacute") ;U+0150 LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
(define-gtkcode #x01d8 "Rcaron") ;U+0158 LATIN CAPITAL LETTER R WITH CARON
(define-gtkcode #x01d9 "Uring") ;U+016E LATIN CAPITAL LETTER U WITH RING ABOVE
(define-gtkcode #x01db "Udoubleacute") ;U+0170 LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
(define-gtkcode #x01de "Tcedilla") ;U+0162 LATIN CAPITAL LETTER T WITH CEDILLA
(define-gtkcode #x01e0 "racute") ;U+0155 LATIN SMALL LETTER R WITH ACUTE
(define-gtkcode #x01e3 "abreve") ;U+0103 LATIN SMALL LETTER A WITH BREVE
(define-gtkcode #x01e5 "lacute") ;U+013A LATIN SMALL LETTER L WITH ACUTE
(define-gtkcode #x01e6 "cacute") ;U+0107 LATIN SMALL LETTER C WITH ACUTE
(define-gtkcode #x01e8 "ccaron") ;U+010D LATIN SMALL LETTER C WITH CARON
(define-gtkcode #x01ea "eogonek") ;U+0119 LATIN SMALL LETTER E WITH OGONEK
(define-gtkcode #x01ec "ecaron") ;U+011B LATIN SMALL LETTER E WITH CARON
(define-gtkcode #x01ef "dcaron") ;U+010F LATIN SMALL LETTER D WITH CARON
(define-gtkcode #x01f0 "dstroke") ;U+0111 LATIN SMALL LETTER D WITH STROKE
(define-gtkcode #x01f1 "nacute") ;U+0144 LATIN SMALL LETTER N WITH ACUTE
(define-gtkcode #x01f2 "ncaron") ;U+0148 LATIN SMALL LETTER N WITH CARON
(define-gtkcode #x01f5 "odoubleacute") ;U+0151 LATIN SMALL LETTER O WITH DOUBLE ACUTE
(define-gtkcode #x01fb "udoubleacute") ;U+0171 LATIN SMALL LETTER U WITH DOUBLE ACUTE
(define-gtkcode #x01f8 "rcaron") ;U+0159 LATIN SMALL LETTER R WITH CARON
(define-gtkcode #x01f9 "uring") ;U+016F LATIN SMALL LETTER U WITH RING ABOVE
(define-gtkcode #x01fe "tcedilla") ;U+0163 LATIN SMALL LETTER T WITH CEDILLA
(define-gtkcode #x01ff "abovedot")       ;U+02D9 DOT ABOVE
(define-gtkcode #x02a1 "Hstroke") ;U+0126 LATIN CAPITAL LETTER H WITH STROKE
(define-gtkcode #x02a6 "Hcircumflex") ;U+0124 LATIN CAPITAL LETTER H WITH CIRCUMFLEX
(define-gtkcode #x02a9 "Iabovedot") ;U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
(define-gtkcode #x02ab "Gbreve") ;U+011E LATIN CAPITAL LETTER G WITH BREVE
(define-gtkcode #x02ac "Jcircumflex") ;U+0134 LATIN CAPITAL LETTER J WITH CIRCUMFLEX
(define-gtkcode #x02b1 "hstroke") ;U+0127 LATIN SMALL LETTER H WITH STROKE
(define-gtkcode #x02b6 "hcircumflex") ;U+0125 LATIN SMALL LETTER H WITH CIRCUMFLEX
(define-gtkcode #x02b9 "idotless") ;U+0131 LATIN SMALL LETTER DOTLESS I
(define-gtkcode #x02bb "gbreve") ;U+011F LATIN SMALL LETTER G WITH BREVE
(define-gtkcode #x02bc "jcircumflex") ;U+0135 LATIN SMALL LETTER J WITH CIRCUMFLEX
(define-gtkcode #x02c5 "Cabovedot") ;U+010A LATIN CAPITAL LETTER C WITH DOT ABOVE
(define-gtkcode #x02c6 "Ccircumflex") ;U+0108 LATIN CAPITAL LETTER C WITH CIRCUMFLEX
(define-gtkcode #x02d5 "Gabovedot") ;U+0120 LATIN CAPITAL LETTER G WITH DOT ABOVE
(define-gtkcode #x02d8 "Gcircumflex") ;U+011C LATIN CAPITAL LETTER G WITH CIRCUMFLEX
(define-gtkcode #x02dd "Ubreve") ;U+016C LATIN CAPITAL LETTER U WITH BREVE
(define-gtkcode #x02de "Scircumflex") ;U+015C LATIN CAPITAL LETTER S WITH CIRCUMFLEX
(define-gtkcode #x02e5 "cabovedot") ;U+010B LATIN SMALL LETTER C WITH DOT ABOVE
(define-gtkcode #x02e6 "ccircumflex") ;U+0109 LATIN SMALL LETTER C WITH CIRCUMFLEX
(define-gtkcode #x02f5 "gabovedot") ;U+0121 LATIN SMALL LETTER G WITH DOT ABOVE
(define-gtkcode #x02f8 "gcircumflex") ;U+011D LATIN SMALL LETTER G WITH CIRCUMFLEX
(define-gtkcode #x02fd "ubreve") ;U+016D LATIN SMALL LETTER U WITH BREVE
(define-gtkcode #x02fe "scircumflex") ;U+015D LATIN SMALL LETTER S WITH CIRCUMFLEX
(define-gtkcode #x03a2 "kra")            ;U+0138 LATIN SMALL LETTER KRA
(define-gtkcode #x03a2 "kappa")          ;deprecated
(define-gtkcode #x03a3 "Rcedilla") ;U+0156 LATIN CAPITAL LETTER R WITH CEDILLA
(define-gtkcode #x03a5 "Itilde") ;U+0128 LATIN CAPITAL LETTER I WITH TILDE
(define-gtkcode #x03a6 "Lcedilla") ;U+013B LATIN CAPITAL LETTER L WITH CEDILLA
(define-gtkcode #x03aa "Emacron") ;U+0112 LATIN CAPITAL LETTER E WITH MACRON
(define-gtkcode #x03ab "Gcedilla") ;U+0122 LATIN CAPITAL LETTER G WITH CEDILLA
(define-gtkcode #x03ac "Tslash") ;U+0166 LATIN CAPITAL LETTER T WITH STROKE
(define-gtkcode #x03b3 "rcedilla") ;U+0157 LATIN SMALL LETTER R WITH CEDILLA
(define-gtkcode #x03b5 "itilde") ;U+0129 LATIN SMALL LETTER I WITH TILDE
(define-gtkcode #x03b6 "lcedilla") ;U+013C LATIN SMALL LETTER L WITH CEDILLA
(define-gtkcode #x03ba "emacron") ;U+0113 LATIN SMALL LETTER E WITH MACRON
(define-gtkcode #x03bb "gcedilla") ;U+0123 LATIN SMALL LETTER G WITH CEDILLA
(define-gtkcode #x03bc "tslash") ;U+0167 LATIN SMALL LETTER T WITH STROKE
(define-gtkcode #x03bd "ENG")          ;U+014A LATIN CAPITAL LETTER ENG
(define-gtkcode #x03bf "eng")            ;U+014B LATIN SMALL LETTER ENG
(define-gtkcode #x03c0 "Amacron") ;U+0100 LATIN CAPITAL LETTER A WITH MACRON
(define-gtkcode #x03c7 "Iogonek") ;U+012E LATIN CAPITAL LETTER I WITH OGONEK
(define-gtkcode #x03cc "Eabovedot") ;U+0116 LATIN CAPITAL LETTER E WITH DOT ABOVE
(define-gtkcode #x03cf "Imacron") ;U+012A LATIN CAPITAL LETTER I WITH MACRON
(define-gtkcode #x03d1 "Ncedilla") ;U+0145 LATIN CAPITAL LETTER N WITH CEDILLA
(define-gtkcode #x03d2 "Omacron") ;U+014C LATIN CAPITAL LETTER O WITH MACRON
(define-gtkcode #x03d3 "Kcedilla") ;U+0136 LATIN CAPITAL LETTER K WITH CEDILLA
(define-gtkcode #x03d9 "Uogonek") ;U+0172 LATIN CAPITAL LETTER U WITH OGONEK
(define-gtkcode #x03dd "Utilde") ;U+0168 LATIN CAPITAL LETTER U WITH TILDE
(define-gtkcode #x03de "Umacron") ;U+016A LATIN CAPITAL LETTER U WITH MACRON
(define-gtkcode #x03e0 "amacron") ;U+0101 LATIN SMALL LETTER A WITH MACRON
(define-gtkcode #x03e7 "iogonek") ;U+012F LATIN SMALL LETTER I WITH OGONEK
(define-gtkcode #x03ec "eabovedot") ;U+0117 LATIN SMALL LETTER E WITH DOT ABOVE
(define-gtkcode #x03ef "imacron") ;U+012B LATIN SMALL LETTER I WITH MACRON
(define-gtkcode #x03f1 "ncedilla") ;U+0146 LATIN SMALL LETTER N WITH CEDILLA
(define-gtkcode #x03f2 "omacron") ;U+014D LATIN SMALL LETTER O WITH MACRON
(define-gtkcode #x03f3 "kcedilla") ;U+0137 LATIN SMALL LETTER K WITH CEDILLA
(define-gtkcode #x03f9 "uogonek") ;U+0173 LATIN SMALL LETTER U WITH OGONEK
(define-gtkcode #x03fd "utilde") ;U+0169 LATIN SMALL LETTER U WITH TILDE
(define-gtkcode #x03fe "umacron") ;U+016B LATIN SMALL LETTER U WITH MACRON

(define-gtkcode #xff7e "Mode_switch")    ;Character set switch
(define-gtkcode #xff7e "script_switch")  ;Alias for mode_switch


(define-gtkcode #xfe01 "ISO_Lock")
(define-gtkcode #xfe02 "ISO_Level2_Latch")
(define-gtkcode #xfe03 "ISO_Level3_Shift")
(define-gtkcode #xfe04 "ISO_Level3_Latch")
(define-gtkcode #xfe05 "ISO_Level3_Lock")
(define-gtkcode #xff7e "ISO_Group_Shift") ;Alias for mode_switch
(define-gtkcode #xfe06 "ISO_Group_Latch")
(define-gtkcode #xfe07 "ISO_Group_Lock")
(define-gtkcode #xfe08 "ISO_Next_Group")
(define-gtkcode #xfe09 "ISO_Next_Group_Lock")
(define-gtkcode #xfe0a "ISO_Prev_Group")
(define-gtkcode #xfe0b "ISO_Prev_Group_Lock")
(define-gtkcode #xfe0c "ISO_First_Group")
(define-gtkcode #xfe0d "ISO_First_Group_Lock")
(define-gtkcode #xfe0e "ISO_Last_Group")
(define-gtkcode #xfe0f "ISO_Last_Group_Lock")
(define-gtkcode #xfe20 "ISO_Left_Tab")
(define-gtkcode #xfe21 "ISO_Move_Line_Up")
(define-gtkcode #xfe22 "ISO_Move_Line_Down")
(define-gtkcode #xfe23 "ISO_Partial_Line_Up")
(define-gtkcode #xfe24 "ISO_Partial_Line_Down")
(define-gtkcode #xfe25 "ISO_Partial_Space_Left")
(define-gtkcode #xfe26 "ISO_Partial_Space_Right")
(define-gtkcode #xfe27 "ISO_Set_Margin_Left")
(define-gtkcode #xfe28 "ISO_Set_Margin_Right")
(define-gtkcode #xfe29 "ISO_Release_Margin_Left")
(define-gtkcode #xfe2a "ISO_Release_Margin_Right")
(define-gtkcode #xfe2b "ISO_Release_Both_Margins")
(define-gtkcode #xfe2c "ISO_Fast_Cursor_Left")
(define-gtkcode #xfe2d "ISO_Fast_Cursor_Right")
(define-gtkcode #xfe2e "ISO_Fast_Cursor_Up")
(define-gtkcode #xfe2f "ISO_Fast_Cursor_Down")
(define-gtkcode #xfe30 "ISO_Continuous_Underline")
(define-gtkcode #xfe31 "ISO_Discontinuous_Underline")
(define-gtkcode #xfe32 "ISO_Emphasize")
(define-gtkcode #xfe33 "ISO_Center_Object")
(define-gtkcode #xfe34 "ISO_Enter")
(define-gtkcode #xfe50 "dead_grave")
(define-gtkcode #xfe51 "dead_acute")
(define-gtkcode #xfe52 "dead_circumflex")
(define-gtkcode #xfe53 "dead_tilde")
(define-gtkcode #xfe54 "dead_macron")
(define-gtkcode #xfe55 "dead_breve")
(define-gtkcode #xfe56 "dead_abovedot")
(define-gtkcode #xfe57 "dead_diaeresis")
(define-gtkcode #xfe58 "dead_abovering")
(define-gtkcode #xfe59 "dead_doubleacute")
(define-gtkcode #xfe5a "dead_caron")
(define-gtkcode #xfe5b "dead_cedilla")
(define-gtkcode #xfe5c "dead_ogonek")
(define-gtkcode #xfe5d "dead_iota")
(define-gtkcode #xfe5e "dead_voiced_sound")
(define-gtkcode #xfe5f "dead_semivoiced_sound")
(define-gtkcode #xfe60 "dead_belowdot")
(define-gtkcode #xfe61 "dead_hook")
(define-gtkcode #xfe62 "dead_horn")
(define-gtkcode #xfed0 "First_Virtual_Screen")
(define-gtkcode #xfed1 "Prev_Virtual_Screen")
(define-gtkcode #xfed2 "Next_Virtual_Screen")
(define-gtkcode #xfed4 "Last_Virtual_Screen")
(define-gtkcode #xfed5 "Terminate_Server")
(define-gtkcode #xfe70 "AccessX_Enable")
(define-gtkcode #xfe71 "AccessX_Feedback_Enable")
(define-gtkcode #xfe72 "RepeatKeys_Enable")
(define-gtkcode #xfe73 "SlowKeys_Enable")
(define-gtkcode #xfe74 "BounceKeys_Enable")
(define-gtkcode #xfe75 "StickyKeys_Enable")
(define-gtkcode #xfe76 "MouseKeys_Enable")
(define-gtkcode #xfe77 "MouseKeys_Accel_Enable")
(define-gtkcode #xfe78 "Overlay1_Enable")
(define-gtkcode #xfe79 "Overlay2_Enable")
(define-gtkcode #xfe7a "AudibleBell_Enable")
(define-gtkcode #xfee0 "Pointer_Left")
(define-gtkcode #xfee1 "Pointer_Right")
(define-gtkcode #xfee2 "Pointer_Up")
(define-gtkcode #xfee3 "Pointer_Down")
(define-gtkcode #xfee4 "Pointer_UpLeft")
(define-gtkcode #xfee5 "Pointer_UpRight")
(define-gtkcode #xfee6 "Pointer_DownLeft")
(define-gtkcode #xfee7 "Pointer_DownRight")
(define-gtkcode #xfee8 "Pointer_Button_Dflt")
(define-gtkcode #xfee9 "Pointer_Button1")
(define-gtkcode #xfeea "Pointer_Button2")
(define-gtkcode #xfeeb "Pointer_Button3")
(define-gtkcode #xfeec "Pointer_Button4")
(define-gtkcode #xfeed "Pointer_Button5")
(define-gtkcode #xfeee "Pointer_DblClick_Dflt")
(define-gtkcode #xfeef "Pointer_DblClick1")
(define-gtkcode #xfef0 "Pointer_DblClick2")
(define-gtkcode #xfef1 "Pointer_DblClick3")
(define-gtkcode #xfef2 "Pointer_DblClick4")
(define-gtkcode #xfef3 "Pointer_DblClick5")
(define-gtkcode #xfef4 "Pointer_Drag_Dflt")
(define-gtkcode #xfef5 "Pointer_Drag1")
(define-gtkcode #xfef6 "Pointer_Drag2")
(define-gtkcode #xfef7 "Pointer_Drag3")
(define-gtkcode #xfef8 "Pointer_Drag4")
(define-gtkcode #xfefd "Pointer_Drag5")
(define-gtkcode #xfef9 "Pointer_EnableKeys")
(define-gtkcode #xfefa "Pointer_Accelerate")
(define-gtkcode #xfefb "Pointer_DfltBtnNext")
(define-gtkcode #xfefc "Pointer_DfltBtnPrev")
(define-gtkcode #xfd01 "3270_Duplicate")
(define-gtkcode #xfd02 "3270_FieldMark")
(define-gtkcode #xfd03 "3270_Right2")
(define-gtkcode #xfd04 "3270_Left2")
(define-gtkcode #xfd05 "3270_BackTab")
(define-gtkcode #xfd06 "3270_EraseEOF")
(define-gtkcode #xfd07 "3270_EraseInput")
(define-gtkcode #xfd08 "3270_Reset")
(define-gtkcode #xfd09 "3270_Quit")
(define-gtkcode #xfd0a "3270_PA1")
(define-gtkcode #xfd0b "3270_PA2")
(define-gtkcode #xfd0c "3270_PA3")
(define-gtkcode #xfd0d "3270_Test")
(define-gtkcode #xfd0e "3270_Attn")
(define-gtkcode #xfd0f "3270_CursorBlink")
(define-gtkcode #xfd10 "3270_AltCursor")
(define-gtkcode #xfd11 "3270_KeyClick")
(define-gtkcode #xfd12 "3270_Jump")
(define-gtkcode #xfd13 "3270_Ident")
(define-gtkcode #xfd14 "3270_Rule")
(define-gtkcode #xfd15 "3270_Copy")
(define-gtkcode #xfd16 "3270_Play")
(define-gtkcode #xfd17 "3270_Setup")
(define-gtkcode #xfd18 "3270_Record")
(define-gtkcode #xfd19 "3270_ChangeScreen")
(define-gtkcode #xfd1a "3270_DeleteWord")
(define-gtkcode #xfd1b "3270_ExSelect")
(define-gtkcode #xfd1c "3270_CursorSelect")
(define-gtkcode #xfd1d "3270_PrintScreen")
(define-gtkcode #xfd1e "3270_Enter")



(define-gtkcode #x13bc "OE")          ;U+0152 LATIN CAPITAL LIGATURE OE
(define-gtkcode #x13bd "oe")            ;U+0153 LATIN SMALL LIGATURE OE
(define-gtkcode #x13be "Ydiaeresis") ;U+0178 LATIN CAPITAL LETTER Y WITH DIAERESIS
(define-gtkcode #x047e "overline")       ;U+203E OVERLINE
(define-gtkcode #x04a1 "kana_fullstop")  ;U+3002 IDEOGRAPHIC FULL STOP
(define-gtkcode #x04a2 "kana_openingbracket") ;U+300C LEFT CORNER BRACKET
(define-gtkcode #x04a3 "kana_closingbracket") ;U+300D RIGHT CORNER BRACKET
(define-gtkcode #x04a4 "kana_comma")     ;U+3001 IDEOGRAPHIC COMMA
(define-gtkcode #x04a5 "kana_conjunctive") ;U+30FB KATAKANA MIDDLE DOT
(define-gtkcode #x04a5 "kana_middledot") ;deprecated
(define-gtkcode #x04a6 "kana_WO")        ;U+30F2 KATAKANA LETTER WO
(define-gtkcode #x04a7 "kana_a")        ;U+30A1 KATAKANA LETTER SMALL A
(define-gtkcode #x04a8 "kana_i")        ;U+30A3 KATAKANA LETTER SMALL I
(define-gtkcode #x04a9 "kana_u")        ;U+30A5 KATAKANA LETTER SMALL U
(define-gtkcode #x04aa "kana_e")        ;U+30A7 KATAKANA LETTER SMALL E
(define-gtkcode #x04ab "kana_o")        ;U+30A9 KATAKANA LETTER SMALL O
(define-gtkcode #x04ac "kana_ya")      ;U+30E3 KATAKANA LETTER SMALL YA
(define-gtkcode #x04ad "kana_yu")      ;U+30E5 KATAKANA LETTER SMALL YU
(define-gtkcode #x04ae "kana_yo")      ;U+30E7 KATAKANA LETTER SMALL YO
(define-gtkcode #x04af "kana_tsu")     ;U+30C3 KATAKANA LETTER SMALL TU
(define-gtkcode #x04af "kana_tu")        ;deprecated
(define-gtkcode #x04b0 "prolongedsound") ;U+30FC KATAKANA-HIRAGANA PROLONGED SOUND MARK
(define-gtkcode #x04b1 "kana_A")         ;U+30A2 KATAKANA LETTER A
(define-gtkcode #x04b2 "kana_I")         ;U+30A4 KATAKANA LETTER I
(define-gtkcode #x04b3 "kana_U")         ;U+30A6 KATAKANA LETTER U
(define-gtkcode #x04b4 "kana_E")         ;U+30A8 KATAKANA LETTER E
(define-gtkcode #x04b5 "kana_O")         ;U+30AA KATAKANA LETTER O
(define-gtkcode #x04b6 "kana_KA")        ;U+30AB KATAKANA LETTER KA
(define-gtkcode #x04b7 "kana_KI")        ;U+30AD KATAKANA LETTER KI
(define-gtkcode #x04b8 "kana_KU")        ;U+30AF KATAKANA LETTER KU
(define-gtkcode #x04b9 "kana_KE")        ;U+30B1 KATAKANA LETTER KE
(define-gtkcode #x04ba "kana_KO")        ;U+30B3 KATAKANA LETTER KO
(define-gtkcode #x04bb "kana_SA")        ;U+30B5 KATAKANA LETTER SA
(define-gtkcode #x04bc "kana_SHI")       ;U+30B7 KATAKANA LETTER SI
(define-gtkcode #x04bd "kana_SU")        ;U+30B9 KATAKANA LETTER SU
(define-gtkcode #x04be "kana_SE")        ;U+30BB KATAKANA LETTER SE
(define-gtkcode #x04bf "kana_SO")        ;U+30BD KATAKANA LETTER SO
(define-gtkcode #x04c0 "kana_TA")        ;U+30BF KATAKANA LETTER TA
(define-gtkcode #x04c1 "kana_CHI")       ;U+30C1 KATAKANA LETTER TI
(define-gtkcode #x04c1 "kana_TI")        ;deprecated
(define-gtkcode #x04c2 "kana_TSU")       ;U+30C4 KATAKANA LETTER TU
(define-gtkcode #x04c2 "kana_TU")        ;deprecated
(define-gtkcode #x04c3 "kana_TE")        ;U+30C6 KATAKANA LETTER TE
(define-gtkcode #x04c4 "kana_TO")        ;U+30C8 KATAKANA LETTER TO
(define-gtkcode #x04c5 "kana_NA")        ;U+30CA KATAKANA LETTER NA
(define-gtkcode #x04c6 "kana_NI")        ;U+30CB KATAKANA LETTER NI
(define-gtkcode #x04c7 "kana_NU")        ;U+30CC KATAKANA LETTER NU
(define-gtkcode #x04c8 "kana_NE")        ;U+30CD KATAKANA LETTER NE
(define-gtkcode #x04c9 "kana_NO")        ;U+30CE KATAKANA LETTER NO
(define-gtkcode #x04ca "kana_HA")        ;U+30CF KATAKANA LETTER HA
(define-gtkcode #x04cb "kana_HI")        ;U+30D2 KATAKANA LETTER HI
(define-gtkcode #x04cc "kana_FU")        ;U+30D5 KATAKANA LETTER HU
(define-gtkcode #x04cc "kana_HU")        ;deprecated
(define-gtkcode #x04cd "kana_HE")        ;U+30D8 KATAKANA LETTER HE
(define-gtkcode #x04ce "kana_HO")        ;U+30DB KATAKANA LETTER HO
(define-gtkcode #x04cf "kana_MA")        ;U+30DE KATAKANA LETTER MA
(define-gtkcode #x04d0 "kana_MI")        ;U+30DF KATAKANA LETTER MI
(define-gtkcode #x04d1 "kana_MU")        ;U+30E0 KATAKANA LETTER MU
(define-gtkcode #x04d2 "kana_ME")        ;U+30E1 KATAKANA LETTER ME
(define-gtkcode #x04d3 "kana_MO")        ;U+30E2 KATAKANA LETTER MO
(define-gtkcode #x04d4 "kana_YA")        ;U+30E4 KATAKANA LETTER YA
(define-gtkcode #x04d5 "kana_YU")        ;U+30E6 KATAKANA LETTER YU
(define-gtkcode #x04d6 "kana_YO")        ;U+30E8 KATAKANA LETTER YO
(define-gtkcode #x04d7 "kana_RA")        ;U+30E9 KATAKANA LETTER RA
(define-gtkcode #x04d8 "kana_RI")        ;U+30EA KATAKANA LETTER RI
(define-gtkcode #x04d9 "kana_RU")        ;U+30EB KATAKANA LETTER RU
(define-gtkcode #x04da "kana_RE")        ;U+30EC KATAKANA LETTER RE
(define-gtkcode #x04db "kana_RO")        ;U+30ED KATAKANA LETTER RO
(define-gtkcode #x04dc "kana_WA")        ;U+30EF KATAKANA LETTER WA
(define-gtkcode #x04dd "kana_N")         ;U+30F3 KATAKANA LETTER N
(define-gtkcode #x04de "voicedsound") ;U+309B KATAKANA-HIRAGANA VOICED SOUND MARK
(define-gtkcode #x04df "semivoicedsound") ;U+309C KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK

(define-gtkcode #x08bc "lessthanequal")  ;U+2264 LESS-THAN OR EQUAL TO
(define-gtkcode #x08bd "notequal")       ;U+2260 NOT EQUAL TO
(define-gtkcode #x08be "greaterthanequal") ;U+2265 GREATER-THAN OR EQUAL TO
(define-gtkcode #x08bf "integral")       ;U+222B INTEGRAL
(define-gtkcode #x08c0 "therefore")      ;U+2234 THEREFORE
(define-gtkcode #x08c1 "variation")      ;U+221D PROPORTIONAL TO
(define-gtkcode #x08c2 "infinity")       ;U+221E INFINITY
(define-gtkcode #x08c5 "nabla")          ;U+2207 NABLA
(define-gtkcode #x08c8 "approximate")    ;U+223C TILDE OPERATOR
(define-gtkcode #x08c9 "similarequal")  ;U+2243 ASYMPTOTICALLY EQUAL TO
(define-gtkcode #x08cd "ifonlyif")      ;U+21D4 LEFT RIGHT DOUBLE ARROW
(define-gtkcode #x08ce "implies")       ;U+21D2 RIGHTWARDS DOUBLE ARROW
(define-gtkcode #x08cf "identical")      ;U+2261 IDENTICAL TO
(define-gtkcode #x08d6 "radical")        ;U+221A SQUARE ROOT
(define-gtkcode #x08da "includedin")     ;U+2282 SUBSET OF
(define-gtkcode #x08db "includes")       ;U+2283 SUPERSET OF
(define-gtkcode #x08dc "intersection")   ;U+2229 INTERSECTION
(define-gtkcode #x08dd "union")          ;U+222A UNION
(define-gtkcode #x08de "logicaland")     ;U+2227 LOGICAL AND
(define-gtkcode #x08df "logicalor")      ;U+2228 LOGICAL OR
(define-gtkcode #x08ef "partialderivative") ;U+2202 PARTIAL DIFFERENTIAL
(define-gtkcode #x08f6 "function") ;U+0192 LATIN SMALL LETTER F WITH HOOK
(define-gtkcode #x08fb "leftarrow")      ;U+2190 LEFTWARDS ARROW
(define-gtkcode #x08fc "uparrow")        ;U+2191 UPWARDS ARROW
(define-gtkcode #x08fd "rightarrow")     ;U+2192 RIGHTWARDS ARROW
(define-gtkcode #x08fe "downarrow")      ;U+2193 DOWNWARDS ARROW
(define-gtkcode #x09df "blank")
(define-gtkcode #x09e0 "soliddiamond")   ;U+25C6 BLACK DIAMOND
(define-gtkcode #x09e1 "checkerboard")   ;U+2592 MEDIUM SHADE
(define-gtkcode #x09e2 "ht")   ;U+2409 SYMBOL FOR HORIZONTAL TABULATION
(define-gtkcode #x09e3 "ff")             ;U+240C SYMBOL FOR FORM FEED
(define-gtkcode #x09e4 "cr")         ;U+240D SYMBOL FOR CARRIAGE RETURN
(define-gtkcode #x09e5 "lf")             ;U+240A SYMBOL FOR LINE FEED
(define-gtkcode #x09e8 "nl")             ;U+2424 SYMBOL FOR NEWLINE
(define-gtkcode #x09e9 "vt")     ;U+240B SYMBOL FOR VERTICAL TABULATION
(define-gtkcode #x09ea "lowrightcorner") ;U+2518 BOX DRAWINGS LIGHT UP AND LEFT
(define-gtkcode #x09eb "uprightcorner") ;U+2510 BOX DRAWINGS LIGHT DOWN AND LEFT
(define-gtkcode #x09ec "upleftcorner") ;U+250C BOX DRAWINGS LIGHT DOWN AND RIGHT
(define-gtkcode #x09ed "lowleftcorner") ;U+2514 BOX DRAWINGS LIGHT UP AND RIGHT
(define-gtkcode #x09ee "crossinglines") ;U+253C BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
(define-gtkcode #x09ef "horizlinescan1") ;U+23BA HORIZONTAL SCAN LINE-1
(define-gtkcode #x09f0 "horizlinescan3") ;U+23BB HORIZONTAL SCAN LINE-3
(define-gtkcode #x09f1 "horizlinescan5") ;U+2500 BOX DRAWINGS LIGHT HORIZONTAL
(define-gtkcode #x09f2 "horizlinescan7") ;U+23BC HORIZONTAL SCAN LINE-7
(define-gtkcode #x09f3 "horizlinescan9") ;U+23BD HORIZONTAL SCAN LINE-9
(define-gtkcode #x09f4 "leftt") ;U+251C BOX DRAWINGS LIGHT VERTICAL AND RIGHT
(define-gtkcode #x09f5 "rightt") ;U+2524 BOX DRAWINGS LIGHT VERTICAL AND LEFT
(define-gtkcode #x09f6 "bott") ;U+2534 BOX DRAWINGS LIGHT UP AND HORIZONTAL
(define-gtkcode #x09f7 "topt") ;U+252C BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
(define-gtkcode #x09f8 "vertbar")   ;U+2502 BOX DRAWINGS LIGHT VERTICAL
(define-gtkcode #x0aa1 "emspace")        ;U+2003 EM SPACE
(define-gtkcode #x0aa2 "enspace")        ;U+2002 EN SPACE
(define-gtkcode #x0aa3 "em3space")       ;U+2004 THREE-PER-EM SPACE
(define-gtkcode #x0aa4 "em4space")       ;U+2005 FOUR-PER-EM SPACE
(define-gtkcode #x0aa5 "digitspace")     ;U+2007 FIGURE SPACE
(define-gtkcode #x0aa6 "punctspace")     ;U+2008 PUNCTUATION SPACE
(define-gtkcode #x0aa7 "thinspace")      ;U+2009 THIN SPACE
(define-gtkcode #x0aa8 "hairspace")      ;U+200A HAIR SPACE
(define-gtkcode #x0aa9 "emdash")         ;U+2014 EM DASH
(define-gtkcode #x0aaa "endash")         ;U+2013 EN DASH
(define-gtkcode #x0aac "signifblank")    ;(U+2423 OPEN BOX)
(define-gtkcode #x0aae "ellipsis")       ;U+2026 HORIZONTAL ELLIPSIS
(define-gtkcode #x0aaf "doubbaselinedot") ;U+2025 TWO DOT LEADER
(define-gtkcode #x0ab0 "onethird")    ;U+2153 VULGAR FRACTION ONE THIRD
(define-gtkcode #x0ab1 "twothirds")  ;U+2154 VULGAR FRACTION TWO THIRDS
(define-gtkcode #x0ab2 "onefifth")    ;U+2155 VULGAR FRACTION ONE FIFTH
(define-gtkcode #x0ab3 "twofifths")  ;U+2156 VULGAR FRACTION TWO FIFTHS
(define-gtkcode #x0ab4 "threefifths") ;U+2157 VULGAR FRACTION THREE FIFTHS
(define-gtkcode #x0ab5 "fourfifths") ;U+2158 VULGAR FRACTION FOUR FIFTHS
(define-gtkcode #x0ab6 "onesixth")    ;U+2159 VULGAR FRACTION ONE SIXTH
(define-gtkcode #x0ab7 "fivesixths") ;U+215A VULGAR FRACTION FIVE SIXTHS
(define-gtkcode #x0ab8 "careof")         ;U+2105 CARE OF
(define-gtkcode #x0abb "figdash")        ;U+2012 FIGURE DASH
(define-gtkcode #x0abc "leftanglebracket") ;(U+27E8 MATHEMATICAL LEFT ANGLE BRACKET)
(define-gtkcode #x0abd "decimalpoint")   ;(U+002E FULL STOP)
(define-gtkcode #x0abe "rightanglebracket") ;(U+27E9 MATHEMATICAL RIGHT ANGLE BRACKET)
(define-gtkcode #x0abf "marker")
(define-gtkcode #x0ac3 "oneeighth")  ;U+215B VULGAR FRACTION ONE EIGHTH
(define-gtkcode #x0ac4 "threeeighths") ;U+215C VULGAR FRACTION THREE EIGHTHS
(define-gtkcode #x0ac5 "fiveeighths") ;U+215D VULGAR FRACTION FIVE EIGHTHS
(define-gtkcode #x0ac6 "seveneighths") ;U+215E VULGAR FRACTION SEVEN EIGHTHS
(define-gtkcode #x0ac9 "trademark")      ;U+2122 TRADE MARK SIGN
(define-gtkcode #x0aca "signaturemark")  ;(U+2613 SALTIRE)
(define-gtkcode #x0acb "trademarkincircle")
(define-gtkcode #x0acc "leftopentriangle") ;(U+25C1 WHITE LEFT-POINTING TRIANGLE)
(define-gtkcode #x0acd "rightopentriangle") ;(U+25B7 WHITE RIGHT-POINTING TRIANGLE)
(define-gtkcode #x0ace "emopencircle")   ;(U+25CB WHITE CIRCLE)
(define-gtkcode #x0acf "emopenrectangle") ;(U+25AF WHITE VERTICAL RECTANGLE)
(define-gtkcode #x0ad0 "leftsinglequotemark") ;U+2018 LEFT SINGLE QUOTATION MARK
(define-gtkcode #x0ad1 "rightsinglequotemark") ;U+2019 RIGHT SINGLE QUOTATION MARK
(define-gtkcode #x0ad2 "leftdoublequotemark") ;U+201C LEFT DOUBLE QUOTATION MARK
(define-gtkcode #x0ad3 "rightdoublequotemark") ;U+201D RIGHT DOUBLE QUOTATION MARK
(define-gtkcode #x0ad4 "prescription")   ;U+211E PRESCRIPTION TAKE
(define-gtkcode #x0ad6 "minutes")        ;U+2032 PRIME
(define-gtkcode #x0ad7 "seconds")        ;U+2033 DOUBLE PRIME
(define-gtkcode #x0ad9 "latincross")     ;U+271D LATIN CROSS
(define-gtkcode #x0ada "hexagram")
(define-gtkcode #x0adb "filledrectbullet") ;(U+25AC BLACK RECTANGLE)
(define-gtkcode #x0adc "filledlefttribullet") ;(U+25C0 BLACK LEFT-POINTING TRIANGLE)
(define-gtkcode #x0add "filledrighttribullet") ;(U+25B6 BLACK RIGHT-POINTING TRIANGLE)
(define-gtkcode #x0ade "emfilledcircle") ;(U+25CF BLACK CIRCLE)
(define-gtkcode #x0adf "emfilledrect") ;(U+25AE BLACK VERTICAL RECTANGLE)
(define-gtkcode #x0ae0 "enopencircbullet") ;(U+25E6 WHITE BULLET)
(define-gtkcode #x0ae1 "enopensquarebullet") ;(U+25AB WHITE SMALL SQUARE)
(define-gtkcode #x0ae2 "openrectbullet") ;(U+25AD WHITE RECTANGLE)
(define-gtkcode #x0ae3 "opentribulletup") ;(U+25B3 WHITE UP-POINTING TRIANGLE)
(define-gtkcode #x0ae4 "opentribulletdown") ;(U+25BD WHITE DOWN-POINTING TRIANGLE)
(define-gtkcode #x0ae5 "openstar")       ;(U+2606 WHITE STAR)
(define-gtkcode #x0ae6 "enfilledcircbullet") ;(U+2022 BULLET)
(define-gtkcode #x0ae7 "enfilledsqbullet") ;(U+25AA BLACK SMALL SQUARE)
(define-gtkcode #x0ae8 "filledtribulletup") ;(U+25B2 BLACK UP-POINTING TRIANGLE)
(define-gtkcode #x0ae9 "filledtribulletdown") ;(U+25BC BLACK DOWN-POINTING TRIANGLE)
(define-gtkcode #x0aea "leftpointer") ;(U+261C WHITE LEFT POINTING INDEX)
(define-gtkcode #x0aeb "rightpointer") ;(U+261E WHITE RIGHT POINTING INDEX)
(define-gtkcode #x0aec "club")           ;U+2663 BLACK CLUB SUIT
(define-gtkcode #x0aed "diamond")        ;U+2666 BLACK DIAMOND SUIT
(define-gtkcode #x0aee "heart")          ;U+2665 BLACK HEART SUIT
(define-gtkcode #x0af0 "maltesecross")   ;U+2720 MALTESE CROSS
(define-gtkcode #x0af1 "dagger")         ;U+2020 DAGGER
(define-gtkcode #x0af2 "doubledagger")   ;U+2021 DOUBLE DAGGER
(define-gtkcode #x0af3 "checkmark")      ;U+2713 CHECK MARK
(define-gtkcode #x0af4 "ballotcross")    ;U+2717 BALLOT X
(define-gtkcode #x0af5 "musicalsharp")   ;U+266F MUSIC SHARP SIGN
(define-gtkcode #x0af6 "musicalflat")    ;U+266D MUSIC FLAT SIGN
(define-gtkcode #x0af7 "malesymbol")     ;U+2642 MALE SIGN
(define-gtkcode #x0af8 "femalesymbol")   ;U+2640 FEMALE SIGN
(define-gtkcode #x0af9 "telephone")      ;U+260E BLACK TELEPHONE
(define-gtkcode #x0afa "telephonerecorder") ;U+2315 TELEPHONE RECORDER
(define-gtkcode #x0afb "phonographcopyright") ;U+2117 SOUND RECORDING COPYRIGHT
(define-gtkcode #x0afc "caret")          ;U+2038 CARET
(define-gtkcode #x0afd "singlelowquotemark") ;U+201A SINGLE LOW-9 QUOTATION MARK
(define-gtkcode #x0afe "doublelowquotemark") ;U+201E DOUBLE LOW-9 QUOTATION MARK
(define-gtkcode #x0aff "cursor")
(define-gtkcode #x0ba3 "leftcaret")      ;(U+003C LESS-THAN SIGN)
(define-gtkcode #x0ba6 "rightcaret")     ;(U+003E GREATER-THAN SIGN)
(define-gtkcode #x0ba8 "downcaret")      ;(U+2228 LOGICAL OR)
(define-gtkcode #x0ba9 "upcaret")        ;(U+2227 LOGICAL AND)
(define-gtkcode #x0bc0 "overbar")        ;(U+00AF MACRON)
(define-gtkcode #x0bc2 "downtack")       ;U+22A5 UP TACK
(define-gtkcode #x0bc3 "upshoe")         ;(U+2229 INTERSECTION)
(define-gtkcode #x0bc4 "downstile")      ;U+230A LEFT FLOOR
(define-gtkcode #x0bc6 "underbar")       ;(U+005F LOW LINE)
(define-gtkcode #x0bca "jot")            ;U+2218 RING OPERATOR
(define-gtkcode #x0bcc "quad")       ;U+2395 APL FUNCTIONAL SYMBOL QUAD
(define-gtkcode #x0bce "uptack")         ;U+22A4 DOWN TACK
(define-gtkcode #x0bcf "circle")         ;U+25CB WHITE CIRCLE
(define-gtkcode #x0bd3 "upstile")        ;U+2308 LEFT CEILING
(define-gtkcode #x0bd6 "downshoe")       ;(U+222A UNION)
(define-gtkcode #x0bd8 "rightshoe")      ;(U+2283 SUPERSET OF)
(define-gtkcode #x0bda "leftshoe")       ;(U+2282 SUBSET OF)
(define-gtkcode #x0bdc "lefttack")       ;U+22A2 RIGHT TACK
(define-gtkcode #x0bfc "righttack")      ;U+22A3 LEFT TAC
|#



