
#lang racket
(require "reactor-lib.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;;背景(外枠)スクリーンの定義
(define SCENE-WIDTH 1280);;大きさが合わなかったので変えました
(define SCENE-HEIGHT 768);;14インチの画面だとタスクバー抜きでの適正サイズはこれくらいになりそうです
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT "white"))

;;１マスの大きさ
(define SQUARE 64)

;;ステージの大きさ
(define STAGE-WIDTH 30)
(define STAGE-HEIGHT 16)

;;環境変数周りの定義
(define WORLD-ENVIROMENT
  (list 0 0 (cons 0 0) (list 99) (list null) (list 0)))
(define (screen-type env) (car env))
(define (stage-selecting env) (cadr env))
(define (player-pos-in-stage env) (caddr env))
(define (stage-state-list env) (cadddr env))
(define (pause-state-list env) (car (cddddr env)))
(define (stage-result env) (cadr (cddddr env)))

(define screen 0)
(define select 1)
(define pos 2)
(define stage 3)
(define pause 4)
(define result 5)
(define (edit env . content)
  (define (make-pair l result)
    (if (null? l)
        result
        (make-pair (cddr l) (cons (cons (car l) (cadr l)) result))))
  (define (add-in-order pair)
    (define (bubble p)
      (cond ((null? (cdr p)) p)
            ((> (caar p) (caadr p))
             (cons (cadr p) (bubble (cons (car p) (cddr p)))))
            (else (cons (car p) (bubble (cdr p))))))
    (define (sorting n result)
      (if (= n 1)
          result
          (sorting (- n 1) (bubble result))))
    (sorting (length pair) pair))
  (define (edit-env env pair n)
    (cond ((null? env) null)
          ((and (not (null? pair)) (= n (caar pair)))
           (cons (cdar pair) (edit-env (cdr env) (cdr pair) (+ n 1))))
          (else (cons (car env) (edit-env (cdr env) pair (+ n 1))))))
  (edit-env env (add-in-order (make-pair content null)) 0))

;;便利関数
(define (take-element l n)
  (define (search-rec l n)
    (if (zero? n)
        (car l)
        (search-rec (cdr l) (- n 1))))
  (if (>= n (length l))
      (error "out of range")
      (search-rec l n)))
(define (take-element2 l2 pos)
  (take-element (take-element l2 (cdr pos)) (car pos)))

(define (change-element l n content)
  (if (zero? n)
      (cons content (cdr l))
      (cons (car l) (change-element (cdr l) (- n 1) content))))
(define (change-element2 l2 pos content)
  (if (zero? (cdr pos))
      (cons (change-element (car l2) (car pos) content)
            (cdr l2))
      (cons (car l2)
            (change-element2 (cdr l2)
                             (cons (car pos) (- (cdr pos) 1))
                             content))))

;;test用関数
(define (select-print env s)
  (place-image (text (number->string (screen-type env)) 25 "black")
               (- SCENE-WIDTH 25) (- SCENE-HEIGHT 25) s))

;;画像のロード
(define sample-square (bitmap/file "sample-square.bmp"))
(define sample-frame (bitmap/file "sample-frame.bmp"))
(define frame64 (bitmap/file "frame64.bmp"))
(define smile (bitmap/file "smile.bmp"))
(define wall (bitmap/file "iron-barred-block.bmp"))
(define ground (bitmap/file "green-grass.bmp"))
(define blank (bitmap/file "blank-white.bmp"))
(define minus1 (bitmap/file "minus-1.bmp"))
(define pushobject (bitmap/file "wooden-box.bmp"))
(define upper-lim (bitmap/file "upper-lim.bmp"))
(define lower-lim (bitmap/file "lower-lim.bmp"))
(define right-lim (bitmap/file "right-lim.bmp"))
(define left-lim (bitmap/file "left-lim.bmp"))

;;ステージデータ
(define (init-step-remain map-data) (car map-data))
(define (init-player-pos map-data) (cadr map-data))
(define (map-size map-data) (caddr map-data))
(define (field-data map-data) (cadddr map-data))

(define map-data-tutorial
  '(10
    (0 . 0)
    (12 . 12)
    ((0 0 0 0 0 0 0 - 0 0 0 0)
     (0 w 0 w 0 0 o 0 0 0 0 0)
     (0 w 0 w 0 0 0 0 0 0 0 0)
     (0 w 0 w w w 0 0 0 0 0 0)
     (0 w 0 l 0 w 0 0 0 0 0 0)
     (0 w 0 w 0 w 0 0 0 0 0 0)
     (w w d w 0 w w 0 0 0 0 0)
     (0 0 0 r u 0 0 0 0 0 0 0)
     (0 0 0 w w w 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0 0 0)
     (0 0 0 0 0 0 0 0 0 0 0 0))))
(define map-data-1
  '(99
    (0 . 0)
    (2 . 2)
    ((0 0)
     (0 0))))
(define map-data-2
  '(99
    (0 . 0)
    (2 . 2)
    ((0 0)
     (0 0))))
(define map-data-3
  '(99
    (0 . 0)
    (2 . 2)
    ((0 0)
     (0 0))))
(define map-data-4
  '(99
    (0 . 0)
    (2 . 2)
    ((0 0)
     (0 0))))
(define map-data-5
  '(99
    (0 . 0)
    (2 . 2)
    ((0 0)
     (0 0))))
(define map-data-6
  '(99
    (0 . 0)
    (2 . 2)
    ((0 0)
     (0 0))))

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
         (big-bang (cons (cadr option) (cdr (init-stage WORLD-ENVIROMENT)))
                   (to-draw display-contents)
                   (on-key key-action)))
        (else (error "undefined option:" option))))

;;画面表示内容
(define (display-contents env)
  (cond ((= (screen-type env) 0) start-screen)
        ((= (screen-type env) 1) (selection-screen (stage-selecting env)))
        ((= (screen-type env) 2) (stage-screen env))
        ((= (screen-type env) 3) pause-screen)
        ((= (screen-type env) 4) fail-screen)
        ((= (screen-type env) 5) (success-screen env)) ;;描画に環境変数が必要なため変えてます
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
(define start-screen;木下知哉
  (place-image sample-frame
               512
               288
               (place-image frame64
                            512
                            288
                            SCENE)))

(define (start-key-event env key)
  (cond ((string=? key "enter") (list 1
                                      (stage-selecting env)
                                      (player-pos-in-stage env)
                                      (stage-state-list env)
                                      (pause-state-list env)
                                      (stage-result env)))
          (else env)))

;;
(define  (selection-screen env)
  (let ((x1 213)
  (x2 640)
  (x3 1067)
  (y1 256)
  (y2 384)
  (y3 512))
    ;選択地点の描画座標
    (define xn x2)
    (define yn y1)
    (cond ((= env 1) (set! xn x2) (set! yn y1))
          ((= env 2) (set! xn x1) (set! yn y2))
          ((= env 3) (set! xn x2) (set! yn y2))
          ((= env 4) (set! xn x3) (set! yn y2))
          ((= env 5) (set! xn x1) (set! yn y3))
          ((= env 6) (set! xn x2) (set! yn y3))
          ((= env 7) (set! xn x3) (set! yn y3)))
    
    (place-image (text "stage 0" 64 "black")
                 x2 y1
                 (place-image (text "stage 1" 64 "black")
                              x1 y2
                              (place-image (text "stage 2" 64 "black")
                                           x2 y2
                                           (place-image (text "stage 3" 64 "black")
                                                        x3 y2                                           
                                                        (place-image (text "stage 4" 64 "black")
                                                                     x1 y3                                                              
                                                                     (place-image (text "stage 5" 64 "black")
                                                                                  x2 y3
                                                                                  (place-image (text "stage 6" 64 "black")
                                                                                               x3 y3
                                                                                        (place-image (circle 30 "solid" "black")
                                                                                                     (- xn 150)
                                                                                                     yn
                                                                                       SCENE ))))))))))


 (define (selection-key-event env key);hayato
   (let ((new-env (cond ((string=? key "right") (+ (stage-selecting env) 1))
                         ((string=? key "left") (- (stage-selecting env) 1))
                         ((string=? key "up") (- (stage-selecting env) 3))
                         ((string=? key "down") (+ (stage-selecting env) 3))
                         (else (car (stage-selecting env))))))
  ;ステージ1を選択中左を押されたら6に移動
     (cond ((> new-env 7) (set! new-env 7))
           ((< new-env 1) (set! new-env 1))
           )
    (edit env select new-env)))

;;taisei
(define (stage-screen env)
  (define map-data (cadr (stage-state-list env)))
  (define (map-image-list)
    (define (make-row row pos)
      (if (null? row)
          '()
          (cons
           (cond
             ((and (= (car (player-pos-in-stage env)) (car pos))
                   (= (cdr (player-pos-in-stage env)) (cdr pos))) smile)
             ((eq? (car row) 'w) wall)
             ((eq? (car row) 'b) blank)
             ((eq? (car row) '-) minus1)
             ((eq? (car row) 'o) pushobject)
             ((eq? (car row) 'u) upper-lim)
             ((eq? (car row) 'd) lower-lim)
             ((eq? (car row) 'r) right-lim)
             ((eq? (car row) 'l) left-lim)
             (else ground))
           (make-row (cdr row) (cons (+ (car pos) 1) (cdr pos))))))
    (define (make-col col pos)
      (if (null? col)
          '()
          (append (make-row (car col) (cons 0 pos))
                  (make-col (cdr col) (+ pos 1)))))
    (make-col map-data 0))
  (define (map-pos-list)
    (define (make-row row x y)
      (if (null? row)
          '()
          (cons (make-posn x y)
                (make-row (cdr row) (+ x SQUARE) y))))
    (define (make-col col y)
      (if (null? col)
          '()
          (append (make-row (car col) 32 y)
                  (make-col (cdr col) (+ y SQUARE)))))
    (make-col map-data 32))
  (define map-field
    (place-images (map-image-list)
                  (map-pos-list)
                  SCENE))
  (define counter
    (let ((text (text/font (number->string (car (stage-state-list env)))
                           128 "black" #f 'modern 'italic 'bold #f)))
      text))
  (place-image counter
               (- SCENE-WIDTH 96)
               (- SCENE-HEIGHT 64)
               map-field))

(define (stage-key-event env key)
  (define (dec-remain-act env n)
    (cons (- (car (stage-state-list env)) n)
          (cdr (stage-state-list env))))
  (define field-data (cadr (stage-state-list env)))
  (define mapsize
    (let ((stage (stage-selecting env)))
      (cond ((= stage 0) (map-size map-data-tutorial))
            ((= stage 1) (map-size map-data-1))
            ((= stage 2) (map-size map-data-2))
            ((= stage 3) (map-size map-data-3))
            ((= stage 4) (map-size map-data-4))
            ((= stage 5) (map-size map-data-5))
            ((= stage 6) (map-size map-data-6)))))
  (define (player-move env dir)
    (define cur-pos (player-pos-in-stage env))
    (define (update-pos pos)
      (define x (car pos))
      (define y (cdr pos))
      (cond ((string=? dir "up")
             (if (= y 0)
                 pos
                 (cons x (- y 1))))
            ((string=? dir "down")
             (if (= y (- (cdr mapsize) 1))
                 pos
                 (cons x (+ y 1))))
            ((string=? dir "left")
             (if (= x 0)
                 pos
                 (cons (- x 1) y)))
            ((string=? dir "right")
             (if (= x (- (car mapsize) 1))
                 pos
                 (cons (+ x 1) y)))))
    (define new-pos (update-pos cur-pos))
    (define gimmick (take-element2 field-data new-pos))
    (cond ((or (eq? gimmick 'w)
               (eq? gimmick 'b)
               (and (= (car cur-pos) (car new-pos))
                    (= (cdr cur-pos) (cdr new-pos)))
               (and (eq? gimmick 'u) (string=? dir "down"))
               (and (eq? gimmick 'd) (string=? dir "up"))
               (and (eq? gimmick 'l) (string=? dir "right"))
               (and (eq? gimmick 'r) (string=? dir "left")))
           env)
          ((eq? gimmick '-)
           (edit env
                 pos new-pos
                 stage (dec-remain-act env 2)))
          ((eq? gimmick 'o)
           (define next-pos (update-pos new-pos))
           (if (eq? (take-element2 field-data next-pos) 0)
               (edit env
                     stage (list (car (dec-remain-act env 1))
                                 (change-element2
                                  (change-element2 field-data new-pos 0)
                                  next-pos 'o)))
               env))
          (else
           (edit env
                 pos new-pos
                 stage (dec-remain-act env 1)))))
  (cond ((string=? key "p") (edit env screen 3))
        ((dir? key) (player-move env key))
        (else env)))

(define (init-stage env)
  (define map-data
    (let ((stage (stage-selecting env)))
      (cond ((= stage 0) map-data-tutorial)
            ((= stage 1) map-data-1)
            ((= stage 2) map-data-2)
            ((= stage 3) map-data-3)
            ((= stage 4) map-data-4)
            ((= stage 5) map-data-5)
            ((= stage 6) map-data-6))))
  (edit env
        pos (init-player-pos map-data)
        stage (list (init-step-remain map-data)
                    (field-data map-data))))

(define (dir? key)
  (or (string=? key "up")
      (string=? key "down")
      (string=? key "left")
      (string=? key "right")))

(define pause-screen
  SCENE)

(define (pause-key-event env key)
  env)

;;
(define fail-screen;;上野智ですよ
  SCENE)

;;yuta
(define (success-screen env)
  (define cx (/ SCENE-WIDTH 2))
  (define cy (/ SCENE-HEIGHT 2))
  (define BOTTON-WIDTH (/ cx 2))
  (define BOTTON-HEIGHT (/ cy 3))
  (define BOTTON-TXTSIZE (/ SCENE-HEIGHT 16))
  (define TITLE-TXTSIZE (* 2 BOTTON-TXTSIZE))
  (define BOTTON-NEX-Y cy)
  (define BOTTON-SEL-Y (+ cy (/ cy 2)))
  (define TITLE-Y (- cy (/ cy 2)))
  (define botton (rectangle BOTTON-WIDTH BOTTON-HEIGHT "solid" "orange"))
  (define (title-success s)
    (place-image (text "SUCCESS" TITLE-TXTSIZE "red") cx TITLE-Y s))
  (define (botton-nextstage s)
    (place-image (text "next stage" BOTTON-TXTSIZE "black") cx BOTTON-NEX-Y
               (place-image botton cx BOTTON-NEX-Y s)))
  (define (botton-selectstage s)
    (place-image (text "select stage" BOTTON-TXTSIZE "black") cx BOTTON-SEL-Y
               (place-image botton cx BOTTON-SEL-Y s)))
  (define (outline-select env s)
    (define y
      (if (= (car (stage-result env)) 0)
          BOTTON-NEX-Y
          BOTTON-SEL-Y))
    (place-image (rectangle (+ BOTTON-WIDTH (/ BOTTON-WIDTH 8)) (+ BOTTON-HEIGHT (/ BOTTON-WIDTH 8)) "outline" "red") cx y s))
  (outline-select env (botton-selectstage (botton-nextstage (title-success
                                                   ;;test用 (select-print env SCENE)
                                                             SCENE
                                                             )))))

(define (success-key-event env key)
  (define prev "up")
  (define next "down")
  (define do "\r")
   (cond ((= (car (stage-result env)) 0)
         (cond ((string=? key do)
                (edit env screen 2 select (+ (stage-selecting env) 1) result (list 0)))
               ((string=? key next)
                (edit env result (list 1)))
               (else env)))
        ((= (car (stage-result env)) 1)
         (cond ((string=? key do)
                (edit env screen 1 select 0 result (list 0)))
               ((string=? key prev)
                (edit env result (list 0)))
               (else env)))
        (else env)))
