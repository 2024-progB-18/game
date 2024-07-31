#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require (only-in racket/gui play-sound))

;;背景(外枠)スクリーンの定義
(define SCENE-WIDTH 1280)
(define SCENE-HEIGHT 768) 
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT "beige"))

;;１マスの大きさ
(define SQUARE 64)

;;実装済みの最後のステージ
(define last-stage 5)

;;環境変数周りの定義
(define WORLD-ENVIROMENT
  (list 0 0 (cons 0 0) '() (list 1 380) (list 0) 5 0))
(define (screen-type env) (car env))
(define (stage-selecting env) (cadr env))
(define (player-pos-in-stage env) (caddr env))
(define (stage-state-list env) (cadddr env))
(define (pause-state-list env) (car (cddddr env)))
(define (stage-result env) (cadr (cddddr env)))
(define (fail-timer env) (caddr (cddddr env)))
(define (player-timer env) (cadddr (cddddr env)))

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

(define (pos=? p1 p2)
  (and (= (car p1) (car p2))
       (= (cdr p1) (cdr p2))))

;;test用関数
(define (select-print env s)
  (place-image (text (number->string (screen-type env)) 25 "black")
               (- SCENE-WIDTH 25) (- SCENE-HEIGHT 25) s))

;;画像と音声のロード
(define stage1 (bitmap/file "resources/stage1.png"))
(define stage2 (bitmap/file "resources/stage2.png"))
(define stage3 (bitmap/file "resources/stage3.png"))
(define stage4 (bitmap/file "resources/stage4.png"))
(define stage5 (bitmap/file "resources/stage5.png"))
(define floor1 (bitmap/file "resources/floor1.png"))
(define floor2 (bitmap/file "resources/lib-floor.png"))
(define floor3 (bitmap/file "resources/webclass-tile1.png"))
(define floor4 (bitmap/file "resources/webclass-tile2.png"))
(define mars (bitmap/file "resources/mars.png"))
(define wall2 (bitmap/file "resources/lib-book.png"))
(define wall3 (bitmap/file "resources/wall3.png"))
(define blank (bitmap/file "resources/blank.png"))
(define smile (bitmap/file "resources/smile.png"))
(define kadaikun0 (bitmap/file "resources/player0.png"))
(define kadaikun1 (bitmap/file "resources/player1.png"))
(define kadaikun2 (bitmap/file "resources/player2.png"))
(define kadaikun3 (bitmap/file "resources/player3.png"))
(define iron-block (bitmap/file "resources/iron-barred-block.png"))
(define grass (bitmap/file "resources/green-grass.png"))
(define plus1 (bitmap/file "resources/plus1.png"))
(define plus3 (bitmap/file "resources/plus3.png"))
(define plus5 (bitmap/file "resources/plus5.png"))
(define minus1 (bitmap/file "resources/minus1.png"))
(define minus2 (bitmap/file "resources/minus2.png"))
(define minus5 (bitmap/file "resources/minus5.png"))
(define pushobject (bitmap/file "resources/wooden-box.png"))
(define folder (bitmap/file "resources/folder.png"))
(define limit (bitmap/file "resources/limit.png"))
(define goal (bitmap/file "resources/goal-flag.png"))
(define key (bitmap/file "resources/key.png"))
(define lock (bitmap/file "resources/lock.png"))
(define unlock (bitmap/file "resources/unlock.png"))
(define rev-switch (bitmap/file "resources/rev-switch.png"))
(define jump (bitmap/file "resources/jump-pad.png"))
(define device (bitmap/file "resources/laser-device.png"))
(define off (bitmap/file "resources/off-device.png"))
(define laser-h (bitmap/file "resources/laser-horizontal.png"))
(define laser-v (bitmap/file "resources/laser-vertical.png"))
(define explo1 (bitmap/file "resources/explo1.png"))
(define explo2 (bitmap/file "resources/explo2.png"))
(define explo3 (bitmap/file "resources/explo3.png"))
(define alien1 (bitmap/file "resources/alien1.png"))
(define alien2 (bitmap/file "resources/alien2.png"))
(define explo-step "resources/explo-step.mp3")
(define explo-laser "resources/explo-laser.mp3")

;;ステージデータ
(define (init-step-remain map-data) (car map-data))
(define (init-key-count map-data) (cadr map-data))
(define (init-player-pos map-data) (caddr map-data))
(define (map-size map-data) (cadddr map-data))
(define (field-data map-data) (car (cddddr map-data)))
(define (move-object-list map-data) (cadr (cddddr map-data)))
(define (switch-func-list map-data) (caddr (cddddr map-data)))
(define (lazer-irradiation-list map-data) (cadddr (cddddr map-data)))
(define (enemy-list map-data) (car (cddddr (cddddr map-data))))

(define map-data-tutorial
  '(20
    0
    (0 . 0)
    (6 . 5)
    ((0 0 0 w k w)
     (w 0 0 w 0 w)
     (g w 0 0 o 0)
     (0 0 w 0 0 b)
     (0 o 0 L 0 b))
    ()
    ()
    ()
    ()))
(define map-data-1
  '(15
    0
    (0 . 3)
    (7 . 4)
    ((b b k + b b g)
     (+ 0 o 0 0 b L)
     (0 0 o 0 - 0 l)
     (0 b 0 r 0 0 0))
    ()
    ()
    ()
    ()))
(define map-data-2
  `(27
    0
    (4 . 1)
    (6 . 6)
    ((w w w w w (s 0))
     (0 k (s 0) w 0 0)
     (0 w u w w 0)
     (l 0 0 + l u)
     (l - 0 w w d)
     ((s 0) l + w g L))
    ()
    (,(lambda (s-data)
        (edit s-data
              3
              (change-element2
               (change-element2
                (change-element2
                 (change-element2
                  (change-element2
                   (change-element2
                    (change-element2
                     (change-element2
                      (change-element2
                       (change-element2
                        (take-element s-data 3)
                        '(5 . 0) '(s 1))
                       '(2 . 1) '(s 1))
                      '(0 . 5) '(s 1))
                     '(2 . 2) 'd)
                    '(0 . 3) 'r)
                   '(0 . 4) 'r)
                  '(1 . 5) 'r)
                 '(4 . 3) 'l)
                '(5 . 3) 'd)
               '(5 . 4) 'u)))
     ,(lambda (s-data)
        (edit s-data
              3
              (change-element2
               (change-element2
                (change-element2
                 (change-element2
                  (change-element2
                   (change-element2
                    (change-element2
                     (change-element2
                      (change-element2
                       (change-element2
                        (take-element s-data 3)
                        '(5 . 0) '(s 0))
                       '(2 . 1) '(s 0))
                      '(0 . 5) '(s 0))
                     '(2 . 2) 'u)
                    '(0 . 3) 'l)
                   '(0 . 4) 'l)
                  '(1 . 5) 'l)
                 '(4 . 3) 'r)
                '(5 . 3) 'u)
               '(5 . 4) 'd))))
    ()
    ()))
(define map-data-3
  `(47
    0
    (0 . 0)
    (10 . 8)
    ((0 o 0 0 0 Il b b b b)
     (Ir 0 w 0 0 0 (s 0) 0 b b)
     (w (x 3) 0 0 0 0 b 0 Il b)
     (0 (s 0) 0 r 0 0 b 0 0 0)
     (0 0 0 w 0 0 (s 0) 0 b 0)
     (0 w w w 0 0 (s 0) 0 b 0)
     (0 0 0 w 0 0 w 0 b L)
     (k w 0 (x -2) l 0 w (x 5) w g))
    ()
    (,(lambda (s-data)
        (edit s-data
              3
              (change-element2
               (change-element2
                (change-element2
                 (change-element2
                  (change-element2
                   (change-element2
                    (change-element2
                     (take-element s-data 3)
                     '(6 . 1) '(s 1))
                    '(1 . 3) '(s 1))
                   '(8 . 2) 'Id)
                  '(5 . 0) 'Id)
                 '(0 . 1) 'Id)
                '(6 . 4) '(s 1))
               '(6 . 5) '(s 1))
              6
              '((#t (8 . 2) d) (#t (5 . 0) d) (#t (0 . 1) d))))
     ,(lambda (s-data)
        (edit s-data
              3
              (change-element2
               (change-element2
               (change-element2
               (change-element2
                (change-element2
                 (change-element2
                  (change-element2
                   (take-element s-data 3)
                   '(6 . 1) '(s 0))
                  '(1 . 3) '(s 0))
                 '(8 . 2) 'Il)
                '(5 . 0) 'Il)
               '(0 . 1) 'Ir)
               '(6 . 4) '(s 0))
               '(6 . 5) '(s 0))
              6
              '((#t (8 . 2) l) (#t (5 . 0) l) (#t (0 . 1) r)))))
    ((#t (8 . 2) l) (#t (5 . 0) l) (#t (0 . 1) r))
    ()))
(define map-data-4
  `(64
    1
    (1 . 1)
    (19 . 11)
    ((0 0 0 b 0 0 0 b 0 0 0 b 0 0 0 b 0 0 0)
     (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 (s 0) 0)
     (0 0 0 b 0 0 0 b 0 o 0 b 0 0 0 b 0 (x -5) 0)
     (b 0 b b b b b b b 0 b b b b b b b L b)
     (0 0 0 b k k k b 0 0 0 b 0 0 0 b 0 (x -5) 0)
     (0 (x 5) 0 b 0 (x 5) 0 b 0 b 0 b 0 (x 5) 0 L 0 iu 0)
     (0 0 0 b 0 (s 0) 0 b 0 0 Ir b w 0 0 b 0 0 0)
     (b 0 b b b L b b b 0 b b b L b b b u b)
     (0 0 0 b 0 0 Ir b 0 o 0 b 0 0 0 b b g b)
     (0 (x 5) 0 0 0 0 0 0 0 b 0 L 0 (x 5) 0 b b b b)
     (0 0 0 b 0 0 0 b 0 0 0 b 0 0 0 b b b b))
    ()
    (,(lambda (s-data)
        (edit s-data
              3
              (change-element2
               (change-element2
                (change-element2
                 (change-element2
                  (change-element2
                   (change-element2
                    (take-element s-data 3)
                    '(17 . 1) '(s 1))
                   '(5 . 6) '(s 1))
                  '(10 . 6) 'Id)
                 '(6 . 8) 'ir)
                '(17 . 5) 'Iu)
               '(17 . 7) 'd)
              6
              '((#t (10 . 6) d) (#t (17 . 5) u))))
     ,(lambda (s-data)
        (edit s-data
              3
              (change-element2
               (change-element2
                (change-element2
                 (change-element2
                  (change-element2
                   (change-element2
                    (take-element s-data 3)
                    '(17 . 1) '(s 2))
                   '(5 . 6) '(s 2))
                  '(10 . 6) 'Il)
                 '(6 . 8) 'Ir)
                '(17 . 5) 'iu)
               '(17 . 7) 'u)
              6
              '((#t (10 . 6) l) (#t (6 . 8) r))))
     ,(lambda (s-data)
        (edit s-data
              3
              (change-element2
               (change-element2
                (change-element2
                 (change-element2
                  (change-element2
                   (change-element2
                    (take-element s-data 3)
                    '(17 . 1) '(s 3))
                   '(5 . 6) '(s 3))
                  '(10 . 6) 'Iu)
                 '(6 . 8) 'ir)
                '(17 . 5) 'Iu)
               '(17 . 7) 'd)
              6
              '((#t (10 . 6) u) (#t (17 . 5) u))))
     ,(lambda (s-data)
        (edit s-data
              3
              (change-element2
               (change-element2
                (change-element2
                 (change-element2
                  (change-element2
                   (change-element2
                    (take-element s-data 3)
                    '(17 . 1) '(s 0))
                   '(5 . 6) '(s 0))
                  '(10 . 6) 'Ir)
                 '(6 . 8) 'Ir)
                '(17 . 5) 'iu)
               '(17 . 7) 'u)
              6
              '((#t (10 . 6) r) (#t (6 . 8) r)))))
    ((#t (10 . 6) r) (#t (6 . 8) r) (#f (17 . 5) u))
    ()))
(define map-data-5
  '(46
    0
    (0 . 0)
    (12 . 7)
    ((0 0 0 w 0 0 0 k w 0 o 0)
     (w w jd Ir 0 0 0 0 w L 0 0)
     (0 0 w 0 0 0 ju 0 w 0 l 0)
     (w 0 0 0 o 0 0 0 w 0 l 0)
     (ju w w 0 0 0 0 e1 w 0 l e2)
     (k k 0 L 0 0 0 0 w L w 0)
     (b b b w 0 0 jr 0 0 0 w g))
    ()
    ()
    ((#t (3 . 1) r))
    (((7 . 4) 0 n ((7 . 4) (7 . 5) (7 . 6)))
     ((11 . 4) 2 p ((11 . 2) (11 . 3) (11 . 4) (11 . 5))))))
(define map-data-6
  `(97
    0
    (16 . 2)
    (17 . 10)
    ((g L 0 l (x -5) L 0 l (s 4) L 0 0 0 w k 0 0)
     (L L 0 w w w 0 w w 0 l 0 0 0 w w (s 0))
     (L 0 e1 l (x 5) L 0 l (s 3) L Ir 0 0 w 0 (s 1) 0)
     (w w w w w w w w w w w Id 0 0 0 w (s 1))
     (0 0 0 0 0 0 0 0 0 0 0 0 l 0 w 0 e1)
     (0 w o o o o u o w w 0 0 0 0 (s 0) 0 e2)
     (0 w 0 0 0 e2 0 0 0 w 0 0 0 o 0 w w)
     (0 0 w w 0 e1 0 0 0 w 0 0 0 0 w b b)
     (b 0 0 w o o o iu w w w w w w b b b)
     (b b 0 0 0 0 0 0 (s 0) b b b b b b b b))
    ()
    (,(lambda (s-data)
        (edit s-data
              3 (change-element2 (take-element s-data 3) '(14 . 0) 0)))
     ,(lambda (s-data)
        (edit s-data
              3
              (change-element2
               (change-element2
                (change-element2
                 (change-element2
                  (change-element2
                   (take-element s-data 3)
                   '(13 . 0) 'w)
                  '(13 . 2) 0)
                 '(14 . 5) 0)
                '(15 . 2) '(s 2))
               '(16 . 3) '(s 2))))
     ,(lambda (s-data)
        (edit s-data
              3
              (change-element2
               (change-element2
                (change-element2
                 (change-element2
                  (change-element2
                   (take-element s-data 3)
                   '(13 . 0) 0)
                  '(13 . 2) 'w)
                 '(14 . 5) 'w)
                '(15 . 2) '(s 1))
               '(16 . 3) '(s 1))))
     ,(lambda (s-data)
        (edit s-data
              3 (change-element2 (take-element s-data 3) '(6 . 1) '(x -5))))
     ,(lambda (s-data)
        (edit s-data
              3 (change-element2 (take-element s-data 3) '(6 . 1) '(x 5)))))
    ((#t (10 . 2) r) (#t (11 . 3) d))
    (((16 . 4) 0 n ((16 . 4) (16 . 5) (15 . 5) (15 . 4)))
     ((16 . 5) 0 n ((16 . 5) (15 . 5) (15 . 4) (16 . 4))))))
(define map-data-test
  `(99
    0
    (0 . 0)
    (14 . 12)
    ((0 0 0 0 0 0  0 - 0  + 0  0 0 (s 0))
     (0 w 0 w 0 0  o 0 0  0 0  0 0 0    )
     (0 w 0 w 0 0  0 0 0  0 0  0 0 0    )
     (0 w 0 w w w  0 0 Il 0 Ir 0 0 w    )
     (0 w 0 l 0 w  0 0 0  0 0  0 0 0    )
     (k w 0 w 0 w  0 0 0  0 0  0 0 (x 5))
     (w w d w 0 w  w 0 0  0 0  0 0 (x 3))
     (0 0 0 r u 0  L 0 0  0 0  0 0 (x -2))
     (0 0 0 w w w  w 0 0  0 0  0 0 0    )
     (w w 0 0 0 ju 0 0 0  0 0  0 0 0    )
     (i 0 w 0 0 0  0 0 0  0 0  0 0 0    )
     (0 0 w 0 0 0  0 0 0  0 0  0 0 g    ))
    ((i (0 . 10)))
    (,(lambda (s-data)
        (edit s-data
              3
              (change-element2
               (change-element2
                (take-element s-data 3)
                '(13 . 0) '(s 1))
               '(13 . 1) 'w)))
     ,(lambda (s-data)
        (edit s-data
              3
              (change-element2
               (change-element2
                (take-element s-data 3)
                '(13 . 0) '(s 0))
               '(13 . 1) 0))))
    ((#t (8 . 3) l) (#t (10 . 3) r))
    ()))

;;ゲームスタート
(define demo "demo")
(define test "test")
(define (start . option)
  (big-bang (cond ((null? option) WORLD-ENVIROMENT)
                  ((string=? (car option) "demo")
                   (init-stage (edit WORLD-ENVIROMENT screen 2 select -1)))
                  ((string=? (car option) "test")
                   (cons (cadr option) (cdr (init-stage WORLD-ENVIROMENT))))
                  (else WORLD-ENVIROMENT))
            (to-draw display-contents)
            (on-key key-action)
            (on-tick tick-process 0.10)))

;;画面表示内容
(define (display-contents env)
  (cond ((= (screen-type env) 0) start-screen)
        ((= (screen-type env) 1) (selection-screen (stage-selecting env)))
        ((= (screen-type env) 2) (stage-screen env))
        ((= (screen-type env) 3) (pause-screen env))
        ((= (screen-type env) 4) (stage-screen env))
        ((= (screen-type env) 5) (success-screen env))
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
    (place-image (text "Submit" 140 "red")
                 640
                 256
        (place-image (text "press Enter key" 50 "blue")
                     640
                     512
                            SCENE)))

(define (start-key-event env key)
  (cond ((string=? key "\r") (edit env screen 1))
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
    (cond ((= env 0) (set! xn x2) (set! yn y1))
          ((= env 1) (set! xn x1) (set! yn y2))
          ((= env 2) (set! xn x2) (set! yn y2))
          ((= env 3) (set! xn x3) (set! yn y2))
          ((= env 4) (set! xn x1) (set! yn y3))
          ((= env 5) (set! xn x2) (set! yn y3))
          ((= env 6) (set! xn x3) (set! yn y3)))
    
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
                                                                                  (place-image (text "stage 6" 64 "gray")
                                                                                               x3 y3
                                                                                               ;; (place-image (circle 30 "solid" "black")
                                                                                               (place-image (text ">" 100 "black")
                                                                                                            (- xn 150)
                                                                                                            yn
                                                                                                            SCENE ))))))))))


(define (selection-key-event env key);hayato
  (let ((new-env (cond ((string=? key "right") (+ (stage-selecting env) 1))
                       ((string=? key "left") (- (stage-selecting env) 1))
                       ((string=? key "up") (- (stage-selecting env) 3))
                       ((string=? key "down") (if (= (stage-selecting env) 0)
                                                  1
                                                  (+ (stage-selecting env) 3)))
                       (else (stage-selecting env)))))
    ;ステージ1を選択中左を押されたら6に移動
    (cond ((> new-env last-stage) (set! new-env last-stage))
          ((< new-env 0) (set! new-env 0))
          )
    (cond ((dir? key) (edit env select new-env))
          ((and (string=? key "\r")
                (ormap (lambda (x) (= x (stage-selecting env)))
                       (range (+ last-stage 1))))
           (init-stage (edit env screen 2 select new-env)))
          ((string=? key "t")
           (init-stage (edit env screen 2 select -1)))
          (else env))))

;;taisei
(define (stage-screen env)
  (define field-width (* (caaddr (stage-state-list env)) SQUARE))
  (define field-height (* (cdaddr (stage-state-list env)) SQUARE))
  (define field-data (cadddr (stage-state-list env)))
  (define lazer-list (take-element (stage-state-list env) 7))
  (define f-timer (fail-timer env))
  (define p-timer (player-timer env))
  (define (map-image-list)
    (define (make-row row pos)
      (define ground
        (cond ((= (stage-selecting env) 1) floor1)
              ((= (stage-selecting env) 2) floor2)
              ((= (stage-selecting env) 3) floor3)
              ((= (stage-selecting env) 4) floor4)
              ((= (stage-selecting env) 5) blank)
              (else grass)))
      (define player-image
        (if (= (stage-selecting env) -1)
            smile
            (place-image (cond ((= p-timer 0) kadaikun0)
                               ((= p-timer 1) kadaikun1)
                               ((= p-timer 2) kadaikun2)
                               ((= p-timer 3) kadaikun3)
                               (else kadaikun3))
                         32 32 ground)))
      (define switch
        (cond ((= (stage-selecting env) 2) rev-switch)
              (else rev-switch)))
      (define wall
        (cond ((= (stage-selecting env) 2) wall2)
              ((= (stage-selecting env) 3) wall3)
              ((= (stage-selecting env) 4) wall3)
              (else iron-block)))
      (define push-o
        (cond ((= (stage-selecting env) 3)
               (place-image folder 32 32 ground))
              ((= (stage-selecting env) 4)
               (place-image folder 32 32 ground))
              (else pushobject)))
      (if (null? row)
          '()
          (let* ((c (car row))
                 (image
                  (cond
                    ((and (= (screen-type env) 2)
                          (pos=? (player-pos-in-stage env) pos))
                     player-image)
                    ((list? c)
                     (cond
                       ((eq? (car c) 's)
                        (place-image switch 32 32 ground))
                       ((and (eq? (car c) 'x) (= (cadr c) 3))
                        (place-image plus3 32 32 ground))
                       ((and (eq? (car c) 'x) (= (cadr c) 5))
                        (place-image plus5 32 32 ground))
                       ((and (eq? (car c) 'x) (= (cadr c) -2))
                        (place-image minus2 32 32 ground))
                       ((and (eq? (car c) 'x) (= (cadr c) -5))
                        (place-image minus5 32 32 ground))))
                    ((eq? c 'w) wall)
                    ((eq? c 'b) blank)
                    ((eq? c '+) (place-image plus1 32 32 ground))
                    ((eq? c '-) (place-image minus1 32 32 ground))
                    ((eq? c 'o) push-o)
                    ((eq? c 'u) (place-image limit 32 32 ground))
                    ((eq? c 'd) (place-image (rotate 180 limit) 32 32 ground))
                    ((eq? c 'r) (place-image (rotate 270 limit) 32 32 ground))
                    ((eq? c 'l) (place-image (rotate 90 limit) 32 32 ground))
                    ((eq? c 'g) (place-image goal 32 32 ground))
                    ((eq? c 'k) (place-image key 32 32 ground))
                    ((eq? c 'L) lock)
                    ((eq? c 'U) (place-image unlock 32 32 ground))
                    ((eq? c 'e1) (place-image alien1 32 32 ground))
                    ((eq? c 'e2) (place-image alien2 32 32 ground))
                    ((eq? c 'ju) (place-image jump 32 32 ground))
                    ((eq? c 'jd) (place-image (rotate 180 jump) 32 32 ground))
                    ((eq? c 'jr) (place-image (rotate 270 jump) 32 32 ground))
                    ((eq? c 'jl) (place-image (rotate 90 jump) 32 32 ground))
                    ((eq? c 'Iu) (place-image device 32 32 ground))
                    ((eq? c 'iu) (place-image off 32 32 ground))
                    ((eq? c 'Id) (place-image (rotate 180 device) 32 32 ground))
                    ((eq? c 'id) (place-image (rotate 180 off) 32 32 ground))
                    ((eq? c 'Ir) (place-image (rotate 270 device) 32 32 ground))
                    ((eq? c 'ir) (place-image (rotate 270 off) 32 32 ground))
                    ((eq? c 'Il) (place-image (rotate 90 device) 32 32 ground))
                    ((eq? c 'il) (place-image (rotate 90 off) 32 32 ground))
                    (else ground)))
                 (laser-added-image
                  (cond ((ormap (lambda (l) (and (eq? (cadr l) 'h)
                                                 (pos=? (car l) pos)))
                                lazer-list)
                         (place-image laser-h 32 32 image))
                        ((ormap (lambda (l) (and (eq? (cadr l) 'v)
                                                 (pos=? (car l) pos)))
                                lazer-list)
                         (place-image laser-v 32 32 image))
                        (else image))))
            (cons
             (cond ((or (= (screen-type env) 2)
                        (not (pos=? (player-pos-in-stage env) pos)))
                    laser-added-image)
                   ((or (= f-timer 0) (= f-timer 4))
                    (place-image explo1 32 32 laser-added-image))
                   ((or (= f-timer 1) (= f-timer 3))
                    (place-image explo2 32 32 laser-added-image))
                   (else
                    (place-image explo3 32 32 laser-added-image)))
             (make-row (cdr row) (cons (+ (car pos) 1) (cdr pos)))))))
    (define (make-col col pos)
      (if (null? col)
          '()
          (append (make-row (car col) (cons 0 pos))
                  (make-col (cdr col) (+ pos 1)))))
    (make-col field-data 0))
  (define dx (cond ((or (= f-timer 2) (= f-timer 4)) 8)
                       ((or (= f-timer 1) (= f-timer 3)) -8)
                       (else 0)))
  (define dy (cond ((or (= f-timer 3) (= f-timer 4)) -8)
                     ((or (= f-timer 1) (= f-timer 2)) 8)
                     (else 0)))
  (define (map-pos-list)
    (define (make-row row x y)
      (if (null? row)
          '()
          (cons (make-posn x y)
                (make-row (cdr row) (+ x SQUARE) y))))
    (define (make-col col y)
      (if (null? col)
          '()
          (append (make-row (car col)
                            (/ (+ (- SCENE-WIDTH field-width) SQUARE dx) 2)
                            y)
                  (make-col (cdr col) (+ y SQUARE)))))
    (make-col field-data
              (/ (+ (- SCENE-HEIGHT field-height) SQUARE dy) 2)))
  (define map-field
    (place-images (map-image-list)
                  (map-pos-list)
                  (cond ((= (stage-selecting env) 1) stage1)
                        ((= (stage-selecting env) 2) stage2)
                        ((= (stage-selecting env) 3) stage3)
                        ((= (stage-selecting env) 4) stage4)
                        ((= (stage-selecting env) 5)
                         (place-image mars
                                      (/ (+ SCENE-WIDTH dx) 2)
                                      (/ (+ SCENE-HEIGHT dy) 2)
                                      stage5))
                        (else SCENE))))
  (define (add-step-counter image)
    (define text
      (text/font (number->string (car (stage-state-list env)))
                 128 "black" #f 'modern 'italic 'bold #f))
    (place-image text
                 (- SCENE-WIDTH 96)
                 (- SCENE-HEIGHT 64)
                 (place-image (rectangle 192 128 "solid" "white")
                              (- SCENE-WIDTH 96)
                              (- SCENE-HEIGHT 64)
                              image)))
  (define (add-key-counter image)
    (let ((key-count
           (text/font (number->string (cadr (stage-state-list env)))
                      64 "black" #f 'modern 'normal 'bold #f)))
      (place-image key-count
                   (- SCENE-WIDTH 32)
                   (- SCENE-HEIGHT 160)
                   (place-image key
                                (- SCENE-WIDTH 72)
                                (- SCENE-HEIGHT 160)
                                image))))
  (if (zero? (cadr (stage-state-list env)))
      (add-step-counter map-field)
      (add-key-counter (add-step-counter map-field))))

(define (stage-key-event env key)
  (define stage-env (stage-state-list env))
  (define (dec-remain-act n) (- (car stage-env) n))
  (define key-count (cadr stage-env))
  (define mapsize (caddr stage-env))
  (define field-data (cadddr stage-env))
  (define move-object-list (car (cddddr stage-env)))
  (define switch-func-list (cadr (cddddr stage-env)))
  (define device-list (caddr (cddddr stage-env)))
  (define (update-pos pos dir)
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
  (define (rev-dir dir)
    (cond ((string=? dir "up") "down")
          ((string=? dir "down") "down")
          ((string=? dir "ledt") "right")
          ((string=? dir "right") "left")))
  (define (player-move env)
    (define dir key)
    (define cur-pos (player-pos-in-stage env))
    (define new-pos (update-pos cur-pos dir))
    (define gimmick (take-element2 field-data new-pos))
    (cond ((eaten? (edit env pos new-pos))
           (fail-process (edit env pos new-pos) 'step))
          ((ormap (lambda (l) (pos=? new-pos (car l)))
                  (take-element (stage-state-list env) 7))
           (fail-process (edit env pos new-pos) 'laser))
          ((list? gimmick)
           (cond
             ((eq? (car gimmick) 's)
              (edit env
                    pos new-pos
                    stage (edit (enemy-move
                                 (recalc-lazer
                                  ((take-element switch-func-list
                                                 (cadr gimmick))
                                   stage-env)))
                                0 (dec-remain-act 1))))
             ((eq? (car gimmick) 'x)
              (edit env
                    pos new-pos
                    stage (enemy-move
                           (edit stage-env
                                 0 (+ (car stage-env) (- (cadr gimmick) 1))
                                 3 (change-element2 field-data
                                                    new-pos '0)))))))
          ((or (ormap (lambda (g) (eq? gimmick g))
                      '(w b Iu Id Il Ir iu id il iu))
               (pos=? cur-pos new-pos)
               (and (eq? gimmick 'u) (string=? dir "down"))
               (and (eq? gimmick 'd) (string=? dir "up"))
               (and (eq? gimmick 'l) (string=? dir "right"))
               (and (eq? gimmick 'r) (string=? dir "left")))
           env)
          ((eq? gimmick '+)
           (edit env
                 pos new-pos
                 stage (enemy-move stage-env)))
          ((eq? gimmick '-)
           (edit env
                 pos new-pos
                 stage (enemy-move
                        (edit stage-env 0 (dec-remain-act 2)))))
          ((eq? gimmick 'o)
           (define next-pos (update-pos new-pos dir))
           (if (eq? (take-element2 field-data next-pos) 0)
               (edit env
                     stage (enemy-move
                            (recalc-lazer
                             (edit stage-env
                                   0 (dec-remain-act 1)
                                   3 (change-element2
                                      (change-element2 field-data new-pos 0)
                                      next-pos 'o)))))
               env))
          ((eq? gimmick 'g) (edit env
                                  select (if (= (stage-selecting env) -1)
                                             0
                                             (stage-selecting env))
                                  screen 5))
          ((eq? gimmick 'k)
           (edit env
                 pos new-pos
                 stage (enemy-move
                        (edit stage-env
                              0 (dec-remain-act 1)
                              1 (+ key-count 1)
                              3 (change-element2 field-data new-pos 0)))))
          ((eq? gimmick 'L)
           (if (zero? key-count)
               env
               (edit env
                     pos new-pos
                     stage (enemy-move
                            (recalc-lazer
                             (edit stage-env
                                   0 (dec-remain-act 1)
                                   1 (- key-count 1)
                                   3 (change-element2 field-data new-pos 'U)))))))
          ((eq? gimmick 'ju)
           (edit env
                 pos (update-pos (update-pos new-pos "up") "up")
                 stage (enemy-move
                        (edit stage-env
                              0 (dec-remain-act 1)))))
          ((eq? gimmick 'jd)
           (edit env
                 pos (update-pos (update-pos new-pos "down") "down")
                 stage (enemy-move
                        (edit stage-env
                              0 (dec-remain-act 1)))))
          ((eq? gimmick 'jr)
           (edit env
                 pos (update-pos (update-pos new-pos "right") "right")
                 stage (edit (enemy-move stage-env)
                             0 (dec-remain-act 1))))
          ((eq? gimmick 'jl)
           (edit env
                 pos (update-pos (update-pos new-pos "left") "left")
                 stage (edit (enemy-move stage-env)
                             0 (dec-remain-act 1))))
          (else
           (edit env
                 pos new-pos
                 stage (edit (enemy-move stage-env) 0 (dec-remain-act 1))))))
  (define (objects-move env move-object-list n)
    (define stage-env (stage-state-list env))
    (define field-data (cadddr stage-env))
    (define object-list (car (cddddr stage-env)))
    (define (object-move object n)
      (define type (car object))
      (define cur-pos (cadr object))
      (cond ((eq? type 'i)
             (define new-pos (update-pos cur-pos key))
             (define gimmick (take-element2 field-data new-pos))
             (if (eq? gimmick 0)
                 (edit stage-env
                       3 (change-element2
                          (change-element2 field-data cur-pos 0)
                          new-pos 'i)
                       4 (edit object-list
                               n (list 'i new-pos)))
                 stage-env))
            ((eq? type 'e)
             (define dir (caddr object))
             (define new-pos (update-pos cur-pos dir))
             (define rev-pos (update-pos cur-pos (rev-dir dir)))
             (define gimmick (take-element2 field-data new-pos))
             (define rev-pos-gimmick (take-element2 field-data rev-pos))
             (cond ((eq? gimmick 0)
                    (edit stage-env
                          3 (change-element2
                             (change-element2 field-data cur-pos 0)
                             new-pos 'e)
                          4 (edit object-list
                                  n (list 'e new-pos dir))))
                   ((and (ormap (lambda (x) (= gimmick x)) '(w b o x))
                         (ormap (lambda (x) (= rev-pos-gimmick x)) '(w b o x)))
                    (edit stage-env
                          4 (edit object-list
                                  n (list 'e cur-pos rev-dir))))
                   ((ormap (lambda (x) (= gimmick x)) '(w b o x))
                    (edit stage-env
                          3 (change-element2
                             (change-element2 field-data cur-pos 0)
                             rev-pos 'e)
                          4 (edit object-list
                                  n (list 'e rev-pos rev-dir))))))
            (else stage-env)))
    (if (null? move-object-list)
        env
        (objects-move (edit env
                            stage
                            (object-move (car move-object-list) n))
                      (cdr move-object-list)
                      (+ n 1))))
  (define (enemy-move stage-env)
    (define enemy-list (take-element stage-env 8))
    (define (move en el n)
      (define f-data (take-element en 3))
      (define e-list (take-element en 8))
      (if (null? el)
          en
          (let* ((enemy (car el))
                 (e-pos (car enemy))
                 (move-list (cadddr enemy))
                 (new-pos
                  (if (eq? (caddr enemy) 'n)
                      (if (= (cadr enemy) (- (length move-list) 1))
                          (take-element move-list (- (cadr enemy) 1))
                          (take-element move-list (+ (cadr enemy) 1)))
                      (if (= (cadr enemy) 0)
                          (take-element move-list 1)
                          (take-element move-list (- (cadr enemy) 1))))))
            (move
             (edit
              en
              3 (change-element2
                 (change-element2
                  f-data
                  (if (ormap (lambda (g)
                               (eq? (take-element2 f-data new-pos) g))
                             '(w b o))
                      (if (eq? (caddr enemy) 'n)
                          (take-element move-list (- (cadr enemy) 1))
                          (take-element move-list 1))
                      new-pos)
                  (take-element2 f-data e-pos))
                 e-pos '0)
              8 (change-element2
                 (change-element2
                  (change-element2
                   e-list
                   `(0 . ,n)
                   (if (ormap (lambda (g)
                                (eq? (take-element2 f-data new-pos) g))
                              '(w b o))
                       (if (eq? (caddr enemy) 'n)
                           (take-element move-list (- (cadr enemy) 1))
                           (take-element move-list 1))
                       new-pos))
                  `(1 . ,n)
                  (if (eq? (caddr enemy) 'n)
                      (if (or (= (cadr enemy) (- (length move-list) 1))
                              (ormap (lambda (g)
                                       (eq? (take-element2 f-data new-pos) g))
                                     '(w b o)))
                          (- (cadr enemy) 1)
                          (+ (cadr enemy) 1))
                      (if (or (= (cadr enemy) 0)
                              (ormap (lambda (g)
                                       (eq? (take-element2 f-data new-pos) g))
                                     '(w b o)))
                          1
                          (- (cadr enemy) 1))))
                 `(2 . ,n)
                 (if (eq? (caddr enemy) 'n)
                     (if (or (= (cadr enemy) (- (length move-list) 1))
                             (ormap (lambda (g)
                                      (eq? (take-element2 f-data new-pos) g))
                                    '(w b o)))
                         'p
                         'n)
                     (if (or (= (cadr enemy) 0)
                             (ormap (lambda (g)
                                      (eq? (take-element2 f-data new-pos) g))
                                    '(w b o)))
                         'n
                         'p))))
             (cdr el)
             (+ n 1)))))
    (move stage-env enemy-list 0))
  (define (fail-check env)
    (if (and (= (screen-type env) 2)
             (<= (car (stage-state-list env)) 0))
        (fail-process env 'step)
        env))
  (define (eaten? env)
    (define player-pos (player-pos-in-stage env))
    (define enemy-list (take-element (stage-state-list env) 8))
    (ormap (lambda (enemy)
             (pos=? player-pos (car enemy)))
           enemy-list))
  (define (eaten-check env)
    (if (eaten? env)
        (fail-process env 'step)
        env))
  (define (burnt-check env)
    (if (ormap (lambda (l) (pos=? (player-pos-in-stage env) (car l)))
               (take-element (stage-state-list env) 7))
        (fail-process env 'laser)
        env))
  (cond ((string=? key "p") (edit env screen 3))
        ((string=? key "r") (init-stage env))
        ((dir? key)
         (fail-check
          (cond
            ((and (null? move-object-list) (null? device-list))
             (player-move env))
            (else
             (burnt-check
              (eaten-check
               (objects-move (player-move env) move-object-list 0)))))))
        (else env)))

(define (tick-process env)
  (define f-timer (fail-timer env))
  (define p-timer (player-timer env))
  (cond ((and (= (screen-type env) 2) (= p-timer 3)) (edit env 7 0))
        ((= (screen-type env) 2) (edit env 7 (+ p-timer 1)))
        ((not (= (screen-type env) 4)) env)
        ((zero? f-timer) (init-stage (edit env screen 2)))
        (else (edit env 6 (- f-timer 1)))))

(define (fail-process env type)
  (cond ((eq? type 'step) (play-sound explo-step #t))
        ((eq? type 'laser) (play-sound explo-laser #t)))
  (edit env
        screen 4
        6 4))

(define (recalc-lazer state-list)
  (define field-data (take-element state-list 3))
  (define stage-size (take-element state-list 2))
  (define device-list (take-element state-list 6))
  (define (next-pos pos dir)
    (define x (car pos))
    (define y (cdr pos))
    (cond ((eq? dir 'u)
           (if (= y 0) pos (cons x (- y 1))))
          ((eq? dir 'd)
           (if (= y (- (cdr stage-size) 1)) pos (cons x (+ y 1))))
          ((eq? dir 'l)
           (if (= x 0) pos (cons (- x 1) y)))
          ((eq? dir 'r)
           (if (= x (- (car stage-size) 1)) pos (cons (+ x 1) y)))))
  (define (calc d-list l-list)
    (define (search pos dir result)
      (define new-pos (next-pos pos dir))
      (if (or (pos=? pos new-pos)
              (ormap (lambda (g) (eq? (take-element2 field-data new-pos) g))
                     '(w o L i)))
          result
          (search new-pos dir (cons (list new-pos
                                          (if (or (eq? dir 'l) (eq? dir 'r))
                                              'h
                                              'v))
                                    result))))
    (if (null? d-list)
      l-list
      (calc (cdr d-list)
            (if (caar d-list)
                (append (search (cadar d-list) (caddar d-list) '())
                        l-list)
                l-list))))
  (edit state-list
        7 (calc device-list '())))

(define (init-stage env)
  (define map-data
    (let ((stage (stage-selecting env)))
      (cond ((= stage 0) map-data-tutorial)
            ((= stage 1) map-data-1)
            ((= stage 2) map-data-2)
            ((= stage 3) map-data-3)
            ((= stage 4) map-data-4)
            ((= stage 5) map-data-5)
            ((= stage 6) map-data-6)
            (else map-data-test))))
  (edit env
        pos (init-player-pos map-data)
        stage (recalc-lazer (list (init-step-remain map-data)
                                  (init-key-count map-data)
                                  (map-size map-data)
                                  (field-data map-data)
                                  (move-object-list map-data)
                                  (switch-func-list map-data)
                                  (lazer-irradiation-list map-data)
                                  '()
                                  (enemy-list map-data)))))

(define (dir? key)
  (or (string=? key "up")
      (string=? key "down")
      (string=? key "left")
      (string=? key "right")))

(define (pause-screen env);;上野
  (define me-y (cadr (pause-state-list env)))
               (place-image (text "PAUSE" 100 "white")
                           640
                           120
  (place-image (text "<1> CLOSE" 40 "white")
               640
               330
               (place-image (text "<2> STAGE-SELECT" 40 "white")
                            640
                            380
                            (place-image (text "<3> RESTART" 40 "white")
                                         640
                                         430
                                         (empty-scene SCENE-WIDTH SCENE-HEIGHT "black"))))))

(define (pause-key-event env key)
  (define me-y (cadr (pause-state-list env)))
  (cond ((string=? key "1");;1を押したらpausemenuを閉じる
          (edit env screen 2))
        
          ((string=? key "2");;2を押したらstage-selectへ
            (edit env screen 1))
           
       ((string=? key "3");;3を押したらRESTART
            (init-stage (edit env screen 2)));;キャラを初期位置に戻す。行動回数のリセット
       (else env)))

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
  (define y
      (if (= (car (stage-result env)) 0)
          BOTTON-NEX-Y
          BOTTON-SEL-Y))
  (define (title-success s)
    (place-image (text "SUCCESS" TITLE-TXTSIZE "red") cx TITLE-Y s))
  (define (title-clear s)
    (place-image (text "ALL CLEAR" (* TITLE-TXTSIZE 1.5) "red") cx TITLE-Y s))
  (define (txt-nextstage s)
    (place-image (text "next stage" BOTTON-TXTSIZE "black") cx BOTTON-NEX-Y s))
  (define (botton-nextstage s)
    (txt-nextstage (place-image botton cx BOTTON-NEX-Y s)))
  (define (txt-selectstage s)
    (place-image (text "select stage" BOTTON-TXTSIZE "black") cx BOTTON-SEL-Y s))
  (define (botton-selectstage s)
    (txt-selectstage (place-image botton cx BOTTON-SEL-Y s)))
  (define (txt-returntitle s)
    (place-image (text "return title" BOTTON-TXTSIZE "black") cx BOTTON-NEX-Y s))
  (define (botton-returntitle s)
    (txt-returntitle (place-image botton cx BOTTON-NEX-Y s)))
  (define (outline-select s)
    (place-image (rectangle (+ BOTTON-WIDTH (/ BOTTON-WIDTH 8))
                            (+ BOTTON-HEIGHT (/ BOTTON-WIDTH 8))
                            "outline" "red") cx y s))
  (define (dot-select s)
    (place-image (circle 30 "solid" "black") (- cx (/ cx 3)) y s))
  (define (tri-select s)
    (place-image (text ">" 100 "black") (- cx (/ BOTTON-WIDTH 2)) (+ y (/ BOTTON-HEIGHT 16)) s))
  (if (= (stage-selecting env) last-stage)
      (tri-select (txt-returntitle (title-clear SCENE)))
      (tri-select (txt-selectstage (txt-nextstage (title-success
                                                   ;;test用 (select-print env SCENE)
                                                             SCENE
                                                             ))))))

(define (success-key-event env key)
  (define prev "up")
  (define next "down")
  (define do "\r")
  (if (= (stage-selecting env) last-stage)
      (cond ((string=? key do) WORLD-ENVIROMENT)
            (else env))  
      (cond ((= (car (stage-result env)) 0)
         (cond ((string=? key do)
                (init-stage
                 (edit env
                       screen 2
                       select (+ (stage-selecting env) 1)
                       result (list 0))))
               ((string=? key next)
                (edit env result (list 1)))
               (else env)))
        ((= (car (stage-result env)) 1)
         (cond ((string=? key do)
                (edit env screen 1 select 0 result (list 0)))
               ((string=? key prev)
                (edit env result (list 0)))
               (else env)))
        (else env))))
