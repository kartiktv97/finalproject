;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname finalproject) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;Final project animation

;;an animation-window is a list of commands
;;  -(make-animation-window list[cmd])

;;a velocity is
;;  -(make-velocity number number)
(define-struct velocity (x y))

;;a posn is
;;  -(make-posn number number)

;;a figure is either
;;  -(make-circle (posn velocity string number string))
;;  -(make-rectangle (posn velocity number number string string))
(define circle (posn velocity radius type color))
(define rectangle (posn velocity width height type color))

;;a command is either:
;;  -(make-displayfigure (figure))
(define displayfigure (figure))



