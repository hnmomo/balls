#lang racket

(require racket/gui)

(define-struct rotate (x y))

(define-struct ball (x y sx sy ax ay))

(define-struct wallh (x1 x2 y))

(define-struct wallv (y1 y2 x))

(define acc 0.01)
(define vis-fac 0.6)
(define ball-size 0.5)

(define wh (list (make-wallh -4 4 4) (make-wallh -3 0 3) (make-wallh 0 3 2)
                 (make-wallh -4 -2 1) (make-wallh -1 2 1) (make-wallh -3 0 0)
                 (make-wallh 2 3 0) (make-wallh -2 -1 -1) (make-wallh -4 -2 -2)
                 (make-wallh 0 1 -2) (make-wallh 2 3 -2) (make-wallh -3 -1 -3)
                 (make-wallh 1 2 -3) (make-wallh -4 4 -4)))
(define wv (list (make-wallv 4 -4 -4) (make-wallv 3 2 -3)
                 (make-wallv 1 -1 -3) (make-wallv 2 1 -2) (make-wallv  -1 -2 -2)
                 (make-wallv 3 1 -1) (make-wallv -1 -3 -1) (make-wallv 0 -2 0)
                 (make-wallv 3 2 1) (make-wallv 1 -3 1) (make-wallv 4 3 2)
                 (make-wallv 0 -2 2) (make-wallv 3 -1 3) (make-wallv -2 -4 3)
                 (make-wallv 4 -3 4)))

(define b (make-ball -3.5 0.5 0 0 0 0))

(define r (make-rotate 0 0))

(define restart (lambda()
  (set! r (make-rotate 0 0))
  (set! b (make-ball -3.5 0.5 0 0 0 0))
                  ))

(define (key frame) (class canvas%
                      (define/override (on-char key-event)
                        (cond 
                          [(and (eq? (send key-event get-key-code) 'down)
                                (> (rotate-x r) -90)) (set! r (make-rotate (- (rotate-x r) 1) (rotate-y r)))]
                          [(and (eq? (send key-event get-key-code) 'up)
                                (< (rotate-x r) 90)) (set! r (make-rotate (+ (rotate-x r) 1) (rotate-y r)))]
                          [(and (eq? (send key-event get-key-code) 'right)
                                (< (rotate-y r) 90)) (set! r (make-rotate (rotate-x r) (+ (rotate-y r) 1)))]
                          [(and (eq? (send key-event get-key-code) 'left)
                                (> (rotate-y r) -90))(set! r (make-rotate (rotate-x r) (- (rotate-y r) 1)))]
                          [(eq? (send key-event get-key-code) '#\r) (restart)]))
                      (super-new [parent frame])))

(define (wall-hit w h)
  (if h
      (cond [(empty? w) false]
            [(and (<= (abs (- (ball-y b) (wallh-y (first w)))) 0.25)
                  (< (ball-x b) (wallh-x2 (first w)))
                  (> (ball-x b) (wallh-x1 (first w))))
             (first w)]
            [else (wall-hit (rest w) h)])
      (cond [(empty? w) false]
            [(and (<= (abs (- (ball-x b) (wallv-x (first w)))) 0.25)
                  (< (ball-y b) (wallv-y1 (first w)))
                  (> (ball-y b) (wallv-y2 (first w))))
             (first w)]
            [else (wall-hit (rest w) h)])))

(define (update-sy h)
  (cond [(wallh? h)
         (local [(define sy (ball-sy b))]
           (cond [(or (and (> (ball-y b) (wallh-y h)) (< sy 0))
                      (and (< (ball-y b) (wallh-y h)) (> sy 0)))
                  (if (< (abs sy) 0.05)
                      (set! b (make-ball (ball-x b) (ball-y b) (ball-sx b) 0 (ball-ax b) (ball-ay b)))
                      (set! b (make-ball (ball-x b) (ball-y b) (ball-sx b)
                                         (* -0.5 (ball-sy b)) (ball-ax b) (ball-ay b))))]))]
        [else 0]))

(define (update-sx v)
  (cond [(wallv? v)
         (local [(define sx (ball-sx b))]
           (cond [(or (and (> (ball-x b) (wallv-x v)) (< sx 0))
                      (and (< (ball-x b) (wallv-x v)) (> sx 0)))
                  (if (< (abs sx) 0.05)
                      (set! b (make-ball (ball-x b) (ball-y b) 0 (ball-sy b) (ball-ax b) (ball-ay b)))
                      (set! b (make-ball (ball-x b) (ball-y b) (* -0.2 (ball-sx b)) (ball-sy b)
                                         (ball-ax b) (ball-ay b))))]))]
        [else 0]))

(define (update-ay rx)
  (set! b (make-ball (ball-x b) (ball-y b) (ball-sx b) (ball-sy b) (* acc (sin rx)) (ball-ay b))))

(define (update-ax ry)
  (set! b (make-ball (ball-x b) (ball-y b) (ball-sx b) (ball-sy b) (ball-ax b) (* acc (sin ry)))))

(define (update-ball)
  (set! b (make-ball (+ (ball-sx b) (ball-x b))
                     (+ (ball-sy b) (ball-y b))
                     (+ (ball-ax b) (ball-sx b))
                     (+ (ball-ay b) (ball-sy b))
                     (ball-ax b) (ball-ay b))))

(define (update)
  (update-sy (wall-hit wh true))
  (update-sx (wall-hit wv false))
  (update-ax (* (/ (rotate-x r) 180) pi))
  (update-ay (* (/ (rotate-y r) 180) pi))
  (update-ball))
    
(define (grt w rx ry h)
  (local [(define x1 0)
          (define x2 0)
          (define y1 0)
          (define y2 0)
          (define z1 0)
          (define z2 0)]
    (cond [h (set! x1 (wallh-x1 w))
             (set! x2 (wallh-x2 w))
             (set! y1 (wallh-y w))
             (set! y2 (wallh-y w))]
          [else (set! x1 (wallv-x w))
                (set! x2 (wallv-x w))
                (set! y1 (wallv-y1 w))
                (set! y2 (wallv-y2 w))])
    (set! z1 (+ (* x1 (sin ry)) (* y1 (sin rx))))
    (set! z2 (+ (* x2 (sin ry)) (* y2 (sin rx))))
    (set! x1 (* x1 (cos ry)))
    (set! x2 (* x2 (cos ry)))
    (set! y1 (* y1 (cos rx)))
    (set! y2 (* y2 (cos rx)))
    (set! z1 (- z1 (* y1 vis-fac)))
    (set! z2 (- z2 (* y2 vis-fac)))
    (set! x1 (- x1 (* y1 vis-fac)))
    (set! x2 (- x2 (* y2 vis-fac)))
    (list x1 z1 x2 z2)))

(define (grb)
    (local [(define x (ball-x b))
            (define y (ball-y b))
            (define z 0)
            (define rx (* (/ (rotate-x r) 180) pi))
            (define ry (* (/ (rotate-y r) 180) pi))]
      (set! z (+ (* x (sin ry)) (* y rx)))
      (set! x (* x (cos ry)))
      (set! y (* y (cos rx)))
      (set! z (- z (* y vis-fac)))
      (set! x (- x (* y vis-fac)))
      (list x z (- ball-size (/ y 64)))))

(define frame (new frame% [label "ball"] [width 400] [height 400]))
(define canvas (new (key frame)))
(define dc (send canvas get-dc))
(define draw-wallh
  (lambda ()
    (map (lambda (lst)
           (local [(define tra (map (lambda (x) (+ (* 20 x) 200)) lst))]
             (send dc draw-line (first tra) (second tra) (third tra) (fourth tra))))
         (map (lambda (w) (grt w (* (/ (rotate-x r) 180) pi) (* (/ (rotate-y r) 180) pi) true)) wh))))
(define draw-wallv
  (lambda ()
    (map (lambda (lst)
           (local [(define tra (map (lambda (x) (+ (* 20 x) 200)) lst))]
             (send dc draw-line (first tra) (second tra) (third tra) (fourth tra))))
         (map (lambda (w) (grt w (* (/ (rotate-x r) 180) pi) (* (/ (rotate-y r) 180) pi) false)) wv))))
(define draw-ball
  (lambda ()
    (local [(define g (grb))]
      (send dc draw-ellipse (- (+ 200 (* 20 (first g))) (* 10 (third g)))
            (- (+ 200 (* 20 (second g))) (* 10 (third g)))
            (* 20 (third g))
            (* 20 (third g))))))

(define win (lambda ()
              (send dc draw-text "you win"
                    100 20)
              (send dc draw-text "(press r to restart)"
                    100 200)))

(define timer (new timer%
                   [notify-callback (lambda()
                                      (cond [(> (ball-x b) 4) (win)]
                                            [else (update)
                                                  (send dc clear)
                                                  (draw-wallh)
                                                  (draw-wallv)
                                                  (draw-ball)]))]
                   [interval #f]))

(println (map (lambda (w) (grt w (* (/ (rotate-x r) 180) pi) (* (/ (rotate-y r) 180) pi) false)) wv))
(println (map (lambda (w) (grt w (* (/ (rotate-x r) 180) pi) (* (/ (rotate-y r) 180) pi) true)) wh))
(send frame show #t)
(send timer start 20)