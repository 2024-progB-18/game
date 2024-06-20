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
  (list 0))
(define (screen-type env)
  (car env))

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
(define (start)
  (big-bang WORLD-ENVIROMENT
            (to-draw display-contents)
            (on-key key-action)))

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
                            SCENE)))

(define (start-key-event env key)
  env)

;;
(define selection-screen
  SCENE)

(define (selection-key-event env key)
  env)

;;
(define stage-screen
  SCENE)

(define (stage-key-event env key)
  env)

(define pause-screen
  SCENE)

(define (pause-key-event env key)
  env)

;;
(define fail-screen
  SCENE)

;;
(define success-screen
  SCENE)

(define (success-key-event env key)
  env)
