#lang racket
(require "reactor-lib.rkt")
(require 2htdp/image)
(require 2htdp/universe)

;;背景(外枠)スクリーンの定義
(define SCENE-WIDTH 1920)
(define SCENE-HEIGHT 1024)
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT "white"))

;;１マスの大きさ
(define SQUARE 64)

;;ステージの大きさ
(define STAGE-WIDTH 30)
(define STAGE-HEIGHT 16)

;;環境変数周りの定義
(define WORLD-ENVIROMENT
  (list 0 0 (cons 0 0) (list null) (list null) (list null)))
(define (screen-type env) (car env))
(define (stage-selecting env) (cadr env))
(define (player-pos-in-stage env) (caddr env))
(define (stage-state-list env) (cadddr env))
(define (pause-state-list env) (car (cddddr env)))
(define (stage-result env) (cadr (cddddr env)))

;;便利関数
(define (take-element l n)
  (define (search-rec l n)
    (if (zero? n)
        (car l)
        (search-rec (cdr l) (- n 1))))
  (if (>= n (length l))
      (error "out of range")
      (search-rec l n)))

;;画像のロード
(define sample-square (bitmap/file "sample-square.bmp"))
(define sample-frame (bitmap/file "sample-frame.bmp"))
(define sample-black (bitmap/file "sample-black.bmp"))
(define frame64 (bitmap/file "frame64.bmp"))

;;ゲームスタート
(define demo "demo")
(define test "test")
(define (start . option)
  (cond ((null? option)
         (big-bang WORLD-ENVIROMENT
                   (to-draw display-contents)
                   (on-key key-action)))
        ((string=? (car option) demo)
         (big-bang WORLD-ENVIROMENT
                   (to-draw display-contents)
                   (on-key key-action)))
        ((string=? (car option) test)
         (big-bang (cons (cadr option) (cdr WORLD-ENVIROMENT))
                   (to-draw display-contents)
                   (on-key key-action)))
        (else (error "undefined option:" option))))

;;画面表示内容
(define (display-contents env)
  (cond ((= (screen-type env) 0) start-screen)
        ((= (screen-type env) 1) selection-screen)
        ((= (screen-type env) 2) stage-screen)
        ((= (screen-type env) 3) pause-screen)
        ((= (screen-type env) 4) fail-screen)
        ((= (screen-type env) 5) success-screen)
        (else (error "wrong enviroment"))))

;;キーボード入力で発火
(define (key-action env key)
  (cond ((= (screen-type env) 0) (start-key-event env key))
        ((= (screen-type env) 1) (selection-key-event env key))
        ((= (screen-type env) 2) (stage-key-event env key))
        ((= (screen-type env) 3) (pause-key-event env key))
        ((= (screen-type env) 4) env)
        ((= (screen-type env) 5) (success-key-event env key))
        (else (error "wrong enviroment"))))

;;
(define start-screen
  (place-image sample-frame
               512
               288
               (place-image frame64
                            512
                            288
                            SCENE)));木下知哉

(define (start-key-event env key)
  env)

;;
(define selection-screen
   (define (draw-scene x)
    (place-image (text "stage 1" 64 "white")
                 320
                 256
                 (place-image (text "stage 2" 64 "white")
                              960
                              256
                              (place-image (text "stage 3" 64 "white")
                                                 1600
                                                 256
                                                 (place-image (text "stage 4" 64 "white")
                                                              320
                                                              786
                                                              (place-image (text "stage 5" 64 "white")
                                                                           960
                                                                           786
                                                                           (place-image (text "stage 6" 64 "white")
                                                                                        1600
                                                                                        786
                                                                                        SCENE))))))))

<<<<<<< Updated upstream
(define (selection-key-event env key);hayato
  (define select 1)
  ;キーボード入力
  (define  (control select k)
    (cond((string=? k "right")(set! select (+ select 1)))
         ((string=? k "left")(set! select (- select 1)))
         ((string=? k "up")(set! select (- select 3)))
         ((string=? k "down")(set! select (+ select 3)))
         ((string=? k " ")(decision-stage select))
         (else select)))
  
  (define(decision-stage select)
   (cond
       ((= select 1)(1st-stage))
       ((= select 2)(2nd-stage))
       ((= select 3)(3rd-stage))
       ((= secect 4)(4th-stage))
       ((= secect 5)(5th-stage))
       ((= secect 6)(6th-stage))))
  env)
=======
(define (selection-key-event env key);hayato  
  (define selection-stage w
    (cond
      ((string=? w "right")(+ i 1))
      ((string=? w "left") (- i 1))
      ((string=? w " ") (decision-stage i))))
  (define (decision-stage i)
   (cond
       ((= i 1)(= key 1))
       ((= i 2)(= key 2))
       ((= i 3)(= key 3))
       ((= i 4)(= key 4))
       ((= i 5)(= key 5))
       ((= i 6)(= key 6))))
env)

>>>>>>> Stashed changes
;;
(define stage-screen
  SCENE)

(define stage-data-tutorial
  (let ((map-start-point (cons 0 0))
        (map-data '()))
    (list map-start-point
          map-data)))

(define (stage-key-event env key)
  (cond ((dir? key) (player-move env key))
        (else env)))

(define (dir? key)
  (or (string=? key "up")
      (string=? key "down")
      (string=? key "left")
      (string=? key "right")))

(define (player-move env dir)
  (let* ((cur-pos (player-pos-in-stage env))
         (cur-x (car cur-pos))
         (cur-y (cdr cur-pos))
         (new-pos (cond ((string=? dir "up") (cons cur-x (- cur-y 1)))
                        ((string=? dir "down") (cons cur-x (+ cur-y 1)))
                        ((string=? dir "left") (cons (- cur-x 1) cur-y))
                        ((string=? dir "right") (cons (+ cur-x 1) cur-y)))))
    (list (screen-type env)
          (stage-selecting env)
          new-pos
          (stage-state-list env)
          (pause-state-list env)
          (stage-result env))))

(define pause-screen
  SCENE)

(define (pause-key-event env key)
  env)

;;
(define fail-screen;;上野智ですよ
  SCENE)

;;yuta
(define success-screen
  SCENE)

(define (success-key-event env key)
  env)
