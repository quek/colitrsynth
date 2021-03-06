(in-package :colitrsynth)

(defun midino-to-freq (midino)
  (* 440.0
     (expt 2.0
           (/ (the fixnum (- midino 69))
              12.0))))

(define-compiler-macro midino-to-freq (&whole form midino)
  (if (constantp midino)
      (funcall 'midino-to-freq midino)
      form))

(defun midino-to-note (midino)
  (declare (optimize (speed 3) (safety 0))
           (fixnum midino))
  (case midino
    (128 'none)
    (129 'off)

    (0 'c0 )
    (1 'c#0)
    (2 'd0 )
    (3 'd#0)
    (4 'e0 )
    (5 'f0 )
    (6 'f#0)
    (7 'g0 )
    (8 'g#0)
    (9 'a0 )
    (10 'a#0)
    (11 'b0 )

    (12 'c1 )
    (13 'c#1)
    (14 'd1 )
    (15 'd#1)
    (16 'e1 )
    (17 'f1 )
    (18 'f#1)
    (19 'g1 )
    (20 'g#1)
    (21 'a1 )
    (22 'a#1)
    (23 'b1 )
    
    (24 'c2 )
    (25 'c#2)
    (26 'd2 )
    (27 'd#2)
    (28 'e2 )
    (29 'f2 )
    (30 'f#2)
    (31 'g2 )
    (32 'g#2)
    (33 'a2 )
    (34 'a#2)
    (35 'b2 )
    
    (36 'c3 )
    (37 'c#3)
    (38 'd3 )
    (39 'd#3)
    (40 'e3 )
    (41 'f3 )
    (42 'f#3)
    (43 'g3 )
    (44 'g#3)
    (45 'a3 )
    (46 'a#3)
    (47 'b3 )
    
    (48 'c4 )
    (49 'c#4)
    (50 'd4 )
    (51 'd#4)
    (52 'e4 )
    (53 'f4 )
    (54 'f#4)
    (55 'g4 )
    (56 'g#4)
    (57 'a4 )
    (58 'a#4)
    (59 'b4 )
    
    (60 'c5 )
    (61 'c#5)
    (62 'd5 )
    (63 'd#5)
    (64 'e5 )
    (65 'f5 )
    (66 'f#5)
    (67 'g5 )
    (68 'g#5)
    (69 'a5 )
    (70 'a#5)
    (71 'b5 )
    
    (72 'c6 )
    (73 'c#6)
    (74 'd6 )
    (75 'd#6)
    (76 'e6 )
    (77 'f6 )
    (78 'f#6)
    (79 'g6 )
    (80 'g#6)
    (81 'a6 )
    (82 'a#6)
    (83 'b6 )
    
    (84 'c7 )
    (85 'c#7)
    (86 'd7 )
    (87 'd#7)
    (88 'e7 )
    (89 'f7 )
    (90 'f#7)
    (91 'g7 )
    (92 'g#7)
    (93 'a7 )
    (94 'a#7)
    (95 'b7 )
    
    (96 'c8 )
    (97 'c#8)
    (98 'd8 )
    (99 'd#8)
    (100 'e8 )
    (101 'f8 )
    (102 'f#8)
    (103 'g8 )
    (104 'g#8)
    (105 'a8 )
    (106 'a#8)
    (107 'b8 )

    (108 'c9 )
    (109 'c#9)
    (110 'd9 )
    (111 'd#9)
    (112 'e9 )
    (113 'f9 )
    (114 'f#9)
    (115 'g9 )
    (116 'g#9)
    (117 'a9 )
    (118 'a#9)
    (119 'b9 )

    (120 'ca )
    (121 'c#a)
    (122 'da )
    (123 'd#a)
    (124 'ea )
    (125 'fa )
    (126 'f#a)
    (127 'ga )))

(defconstant none 128)
(defconstant off 129)

(defconstant c0  0)
(defconstant c#0 1)
(defconstant d0  2)
(defconstant d#0 3)
(defconstant e0  4)
(defconstant f0  5)
(defconstant f#0 6)
(defconstant g0  7)
(defconstant g#0 8)
(defconstant a0  9)
(defconstant a#0 10)
(defconstant b0  11)

(defconstant c1  12)
(defconstant c#1 13)
(defconstant d1  14)
(defconstant d#1 15)
(defconstant e1  16)
(defconstant f1  17)
(defconstant f#1 18)
(defconstant g1  19)
(defconstant g#1 20)
(defconstant a1  21)
(defconstant a#1 22)
(defconstant b1  23)
                   
(defconstant c2  24)
(defconstant c#2 25)
(defconstant d2  26)
(defconstant d#2 27)
(defconstant e2  28)
(defconstant f2  29)
(defconstant f#2 30)
(defconstant g2  31)
(defconstant g#2 32)
(defconstant a2  33)
(defconstant a#2 34)
(defconstant b2  35)
                   
(defconstant c3  36)
(defconstant c#3 37)
(defconstant d3  38)
(defconstant d#3 39)
(defconstant e3  40)
(defconstant f3  41)
(defconstant f#3 42)
(defconstant g3  43)
(defconstant g#3 44)
(defconstant a3  45)
(defconstant a#3 46)
(defconstant b3  47)
                   
(defconstant c4  48)
(defconstant c#4 49)
(defconstant d4  50)
(defconstant d#4 51)
(defconstant e4  52)
(defconstant f4  53)
(defconstant f#4 54)
(defconstant g4  55)
(defconstant g#4 56)
(defconstant a4  57)
(defconstant a#4 58)
(defconstant b4  59)
                   
(defconstant c5  60)
(defconstant c#5 61)
(defconstant d5  62)
(defconstant d#5 63)
(defconstant e5  64)
(defconstant f5  65)
(defconstant f#5 66)
(defconstant g5  67)
(defconstant g#5 68)
(defconstant a5  69)
(defconstant a#5 70)
(defconstant b5  71)
                   
(defconstant c6  72)
(defconstant c#6 73)
(defconstant d6  74)
(defconstant d#6 75)
(defconstant e6  76)
(defconstant f6  77)
(defconstant f#6 78)
(defconstant g6  79)
(defconstant g#6 80)
(defconstant a6  81)
(defconstant a#6 82)
(defconstant b6  83)
                   
(defconstant c7  84)
(defconstant c#7 85)
(defconstant d7  86)
(defconstant d#7 87)
(defconstant e7  88)
(defconstant f7  89)
(defconstant f#7 90)
(defconstant g7  91)
(defconstant g#7 92)
(defconstant a7  93)
(defconstant a#7 94)
(defconstant b7  95)
                   
(defconstant c8  96)
(defconstant c#8 97)
(defconstant d8  98)
(defconstant d#8 99)
(defconstant e8  100)
(defconstant f8  101)
(defconstant f#8 102)
(defconstant g8  103)
(defconstant g#8 104)
(defconstant a8  105)
(defconstant a#8 106)
(defconstant b8  107)
                   
(defconstant c9  108)
(defconstant c#9 109)
(defconstant d9  110)
(defconstant d#9 111)
(defconstant e9  112)
(defconstant f9  113)
(defconstant f#9 114)
(defconstant g9  115)
(defconstant g#9 116)
(defconstant a9  117)
(defconstant a#9 118)
(defconstant b9  119)

(defconstant ca  120)
(defconstant c#a 121)
(defconstant da  122)
(defconstant d#a 123)
(defconstant ea  124)
(defconstant fa  125)
(defconstant f#a 126)
(defconstant ga  127)

(defun valid-note-p (note)
  (<= c0 note b9))

(deftype midi-event-type () 'unsigned-byte)
(deftype midi-channel-type () 'unsigned-byte)
(deftype midi-note-type () 'unsigned-byte)
(deftype midi-velocity-type () 'unsigned-byte)

(defconstant +midi-event-on+ #x90)
(defconstant +midi-event-off+ #x80)
(defconstant +midi-cc+ #xB0)
(defconstant +midi-cc-all-notes-off+ #x7B)

(defclass midi-event ()
  ((event :initarg :event :initform +midi-event-on+ :accessor .event
          :type midi-event-type)
   (channel :initarg :channel :initform 1 :accessor .channel
            :type midi-channel-type)
   (note :initarg :note :initform c4 :accessor .note
         :type midi-note-type)
   (velocity :initarg :velocity :initform 100 :accessor .velocity
             :type midi-velocity-type)
   (frame :initarg :frame :initform 0 :accessor .frame
          :type fixnum)))

(defmethod print-object ((self midi-event) stream)
    (print-unreadable-object (self stream :type t)
      (format stream "~x ~a ~d ~d"
              (.event self)
              (midino-to-note (.note self))
              (.velocity self)
              (.frame self))))

(defun midi-event-all-notes-off ()
  (make-instance 'midi-event
                 :event +midi-cc+
                 :note +midi-cc-all-notes-off+))

#+nil
(flex:with-output-to-sequence (out)
 (write-midi-event (make-instance 'midi-event) out))

