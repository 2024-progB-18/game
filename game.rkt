
#lang racket
(require reactor)
(require "reactor-lib.rkt")
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;;背景(外枠)スクリーンの定義
(define SCENE-WIDTH 1280)
(define SCENE-HEIGHT 768) 
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT "beige"))

;;１マスの大きさ
(define SQUARE 64)

;;ステージの大きさ
(define STAGE-WIDTH 30)
(define STAGE-HEIGHT 16)

;;環境変数周りの定義
(define WORLD-ENVIROMENT
  (list 0 0 (cons 0 0) '() (list 1 380) (list 0)))
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

(define (pos=? p1 p2)
  (and (= (car p1) (car p2))
       (= (cdr p1) (cdr p2))))

;;test用関数
(define (select-print env s)
  (place-image (text (number->string (screen-type env)) 25 "black")
               (- SCENE-WIDTH 25) (- SCENE-HEIGHT 25) s))

;;画像のロード
(define stage1 (bitmap/file "stage1.png"))
(define stage2 (bitmap/file "stage2.png"))
(define stage3 (bitmap/file "stage3.png"))
(define floor1 (bitmap/file "floor1.png"))
(define floor2 (bitmap/file "lib-floor.png"))
(define floor3 (bitmap/file "webclass-tile1.png"))
(define wall2 (bitmap/file "lib-book.png"))
(define wall3 (bitmap/file "wall3.png"))
(define blank (bitmap/file "blank.png"))
(define smile (bitmap/file "smile.png"))
(define iron-block (bitmap/file "iron-barred-block.png"))
(define grass (bitmap/file "green-grass.png"))
(define plus1 (bitmap/file "plus1.png"))
(define plus3 (bitmap/file "plus3.png"))
(define plus5 (bitmap/file "plus5.png"))
(define minus1 (bitmap/file "minus1.png"))
(define minus2 (bitmap/file "minus2.png"))
(define pushobject (bitmap/file "wooden-box.png"))
(define limit (bitmap/file "limit.png"))
(define goal (bitmap/file "goal-flag.png"))
(define key (bitmap/file "key.png"))
(define lock (bitmap/file "lock.png"))
(define unlock (bitmap/file "unlock.png"))
(define rev-switch (bitmap/file "rev-switch.png"))
(define jump (bitmap/file "jump-pad.png"))
(define device (bitmap/file "laser-device.png"))
(define laser-h (bitmap/file "laser-horizontal.png"))
(define laser-v (bitmap/file "laser-vertical.png"))

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
  `(99
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
                       (change-element2 (take-element s-data 3) '(5 . 0) '(s 1))
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
                       (change-element2 (take-element s-data 3) '(5 . 0) '(s 0))
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
  `(43
    0
    (0 . 0)
    (10 . 8)
    ((0 0 0 0 0 b b b b b)
     (0 0 0 0 0 r (s 0) 0 b b)
     (w (x 3) 0 0 0 0 b 0 Il b)
     (0 0 (s 0) r 0 0 b 0 0 0)
     (0 0 0 w 0 0 0 0 b 0)
     (0 w w w 0 0 w 0 b 0)
     (L 0 0 w 0 0 w 0 b L)
     (k w k (x -2) l 0 w (x 5) w g))
    ()
    (,(lambda (s-data)
        (edit s-data
              3
              (change-element2
               (change-element2
                (change-element2 (take-element s-data 3) '(6 . 1) '(s 1))
                '(2 . 3) '(s 1))
               '(8 . 2) 'Id)
              6
              '((#t (8 . 2) d))))
     ,(lambda (s-data)
        (edit s-data
              3
              (change-element2
               (change-element2
                (change-element2 (take-element s-data 3) '(6 . 1) '(s 0))
                '(2 . 3) '(s 0))
               '(8 . 2) 'Il)
              6
              '((#t (8 . 2) l)))))
    ((#t (8 . 2) l))
    ()))
(define map-data-4
  '(99
    0
    (3 . 3)
    (13 . 12)
    ((w w w w w w w w w w w w w)
     (w 0 0 0 0 0 w 0 0 0 0 0 w)
     (w 0 0 0 0 0 w 0 0 0 0 0 w)
     (w 0 0 0 0 0 w 0 0 i 0 0 w)
     (w 0 0 0 0 0 w 0 0 0 0 0 w)
     (w 0 0 0 0 0 w 0 0 w 0 0 w)
     (w 0 0 0 0 0 w 0 0 0 0 0 w)
     (w 0 0 g 0 0 w 0 0 0 0 0 w)
     (w 0 0 0 0 0 w 0 0 0 0 0 w)
     (w 0 0 0 0 0 w 0 0 0 0 0 w)
     (w 0 0 0 0 0 w 0 0 0 0 0 w)
     (w w w w w w w w w w w w w))
    ((i (9 . 3)))
    ()
    ()
    ()))
(define map-data-5
  '(99
    0
    (0 . 0)
    (2 . 2)
    ((0 0)
     (0 g))
    ()
    ()
    ()
    ()))
(define map-data-6
  '(99
    0
    (0 . 0)
    (2 . 2)
    ((0 0)
     (0 g))
    ()
    ()
    ()
    ()))
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
               (change-element2 (take-element s-data 3) '(13 . 0) '(s 1))
               '(13 . 1) 'w)))
     ,(lambda (s-data)
        (edit s-data
              3
              (change-element2
               (change-element2 (take-element s-data 3) '(13 . 0) '(s 0))
               '(13 . 1) 0))))
    ((#t (8 . 3) l) (#t (10 . 3) r))
    ()))

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
        ((= (screen-type env) 3) (pause-screen env))
        ((= (screen-type env) 4) fail-screen)
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
  (cond ((string=? key "\r") (list 1
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
                       ((string=? key "down") (if (= (stage-selecting env) 0)
                                                  1
                                                  (+ (stage-selecting env) 3)))
                       (else (stage-selecting env)))))
    ;ステージ1を選択中左を押されたら6に移動
    (cond ((> new-env 6) (set! new-env 6))
          ((< new-env 0) (set! new-env 0))
          )
    (cond ((dir? key) (edit env select new-env))
          ((string=? key "\r")
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
  (define (map-image-list)
    (define (make-row row pos)
      (define ground
        (cond ((= (stage-selecting env) 1) floor1)
              ((= (stage-selecting env) 2) floor2)
              ((= (stage-selecting env) 3) floor3)
              (else grass)))
      (define switch
        (cond ((= (stage-selecting env) 2) rev-switch)
              (else rev-switch)))
      (define wall
        (cond ((= (stage-selecting env) 2) wall2)
              ((= (stage-selecting env) 3) wall3)
              (else iron-block)))
      (if (null? row)
          '()
          (let* ((c (car row))
                 (image
                  (cond
                    ((pos=? (player-pos-in-stage env) pos) smile)
                    ((list? c)
                     (cond
                       ((eq? (car c) 's)
                        (place-image switch 32 32 ground))
                       ((and (eq? (car c) 'x) (= (cadr c) 3))
                        (place-image plus3 32 32 ground))
                       ((and (eq? (car c) 'x) (= (cadr c) 5))
                        (place-image plus5 32 32 ground))
                       ((and (eq? (car c) 'x) (= (cadr c) -2))
                        (place-image minus2 32 32 ground))))
                    ((eq? c 'w) wall)
                    ((eq? c 'b) blank)
                    ((eq? c '+) (place-image plus1 32 32 ground))
                    ((eq? c '-) (place-image minus1 32 32 ground))
                    ((eq? c 'o) pushobject)
                    ((eq? c 'u) (place-image limit 32 32 ground))
                    ((eq? c 'd) (place-image (rotate 180 limit) 32 32 ground))
                    ((eq? c 'r) (place-image (rotate 270 limit) 32 32 ground))
                    ((eq? c 'l) (place-image (rotate 90 limit) 32 32 ground))
                    ((eq? c 'g) (place-image goal 32 32 ground))
                    ((eq? c 'k) (place-image key 32 32 ground))
                    ((eq? c 'L) lock)
                    ((eq? c 'U) (place-image unlock 32 32 ground))
                    ((eq? c 'i) empty-image)
                    ((eq? c 'e) empty-image)
                    ((eq? c 'ju) (place-image jump 32 32 ground))
                    ((eq? c 'jd) (place-image (rotate 180 jump) 32 32 ground))
                    ((eq? c 'jr) (place-image (rotate 270 jump) 32 32 ground))
                    ((eq? c 'jl) (place-image (rotate 90 jump) 32 32 ground))
                    ((eq? c 'Iu) (place-image device 32 32 ground))
                    ((eq? c 'Id) (place-image (rotate 180 device) 32 32 ground))
                    ((eq? c 'Ir) (place-image (rotate 270 device) 32 32 ground))
                    ((eq? c 'Il) (place-image (rotate 90 device) 32 32 ground))
                    (else ground))))
            (cons
             (cond ((ormap (lambda (l) (and (eq? (cadr l) 'h)
                                            (pos=? (car l) pos)))
                           lazer-list)
                    (place-image laser-h 32 32 image))
                   ((ormap (lambda (l) (and (eq? (cadr l) 'v)
                                            (pos=? (car l) pos)))
                           lazer-list)
                    (place-image laser-v 32 32 image))
                   (else image))
             (make-row (cdr row) (cons (+ (car pos) 1) (cdr pos)))))))
    (define (make-col col pos)
      (if (null? col)
          '()
          (append (make-row (car col) (cons 0 pos))
                  (make-col (cdr col) (+ pos 1)))))
    (make-col field-data 0))
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
                            (/ (+ (- SCENE-WIDTH field-width) SQUARE) 2)
                            y)
                  (make-col (cdr col) (+ y SQUARE)))))
    (make-col field-data
              (/ (+ (- SCENE-HEIGHT field-height) SQUARE) 2)))
  (define map-field
    (place-images (map-image-list)
                  (map-pos-list)
                  (cond ((= (stage-selecting env) 1) stage1)
                        ((= (stage-selecting env) 2) stage2)
                        ((= (stage-selecting env) 3) stage3)
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
    (cond ((list? gimmick)
           (cond
             ((eq? (car gimmick) 's)
              (edit env
                    pos new-pos
                    stage (recalc-lazer
                           ((take-element switch-func-list
                                                  (cadr gimmick))
                                    stage-env))))
             ((eq? (car gimmick) 'x)
              (edit env
                    pos new-pos
                    stage (edit stage-env
                                0 (+ (car stage-env) (- (cadr gimmick) 1))
                                3 (change-element2 field-data
                                                   new-pos '0))))))
          ((or (eq? gimmick 'w)
               (eq? gimmick 'b)
               (eq? gimmick 'Iu)
               (eq? gimmick 'Id)
               (eq? gimmick 'Il)
               (eq? gimmick 'Ir)
               (pos=? cur-pos new-pos)
               (and (eq? gimmick 'u) (string=? dir "down"))
               (and (eq? gimmick 'd) (string=? dir "up"))
               (and (eq? gimmick 'l) (string=? dir "right"))
               (and (eq? gimmick 'r) (string=? dir "left")))
           env)
          ((eq? gimmick '+)
           (edit env
                 pos new-pos))
          ((eq? gimmick '-)
           (edit env
                 pos new-pos
                 stage (edit stage-env 0 (dec-remain-act 2))))
          ((eq? gimmick 'o)
           (define next-pos (update-pos new-pos dir))
           (if (eq? (take-element2 field-data next-pos) 0)
               (edit env
                     stage (recalc-lazer
                            (edit stage-env
                                  0 (dec-remain-act 1)
                                  3 (change-element2
                                     (change-element2 field-data new-pos 0)
                                     next-pos 'o))))
               env))
          ((eq? gimmick 'g) (edit env
                                  select (if (= (stage-selecting env) -1)
                                             0
                                             (stage-selecting env))
                                  screen 5))
          ((eq? gimmick 'k)
           (edit env
                 pos new-pos
                 stage (edit stage-env
                             0 (dec-remain-act 1)
                             1 (+ key-count 1)
                             3 (change-element2 field-data new-pos 0))))
          ((eq? gimmick 'L)
           (if (zero? key-count)
               env
               (edit env
                     pos new-pos
                     stage (edit stage-env
                                 0 (dec-remain-act 1)
                                 1 (- key-count 1)
                                 3 (change-element2 field-data new-pos 'U)))))
          ((eq? gimmick 'ju)
           (edit env pos (update-pos (update-pos new-pos "up") "up")))
          ((eq? gimmick 'jd)
           (edit env pos (update-pos (update-pos new-pos "down") "down")))
          ((eq? gimmick 'jr)
           (edit env pos (update-pos (update-pos new-pos "right") "right")))
          ((eq? gimmick 'jl)
           (edit env pos (update-pos (update-pos new-pos "left") "left")))
          (else
           (edit env
                 pos new-pos
                 stage (edit stage-env 0 (dec-remain-act 1))))))
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
  (define (eaten-check env)
    (if (eq? (take-element2 (cadddr (stage-state-list env))
                            (player-pos-in-stage env))
             'e)
        (fail-process env)
        env))
  (define (burnt-check env)
    (if (ormap (lambda (l) (pos=? (player-pos-in-stage env) (car l)))
               (take-element (stage-state-list env) 7))
        (fail-process env)
        env))
  (cond ((string=? key "p") (edit env screen 3))
        ((string=? key "r") (init-stage env))
        ((dir? key)
         (cond
           ((and (null? move-object-list) (null? device-list))
            (player-move env))
           (else
            (burnt-check
             (eaten-check
              (objects-move (player-move env) move-object-list 0))))))
        (else env)))

(define (fail-process env)
  (init-stage env))

(define (recalc-lazer state-list)
  (define field-data (take-element state-list 3))
  (define device-list (take-element state-list 6))
  (define (next-pos pos dir)
    (cond ((eq? dir 'u) (cons (car pos) (- (cdr pos) 1)))
          ((eq? dir 'd) (cons (car pos) (+ (cdr pos) 1)))
          ((eq? dir 'l) (cons (- (car pos) 1) (cdr pos)))
          ((eq? dir 'r) (cons (+ (car pos) 1) (cdr pos)))))
  (define (calc d-list l-list)
    (define (search pos dir result)
      (define new-pos (next-pos pos dir))
      (if (ormap (lambda (g) (eq? (take-element2 field-data new-pos) g))
                 '(w o L i))
          result
          (search new-pos dir (cons (list new-pos
                                          (if (or (eq? dir 'l) (eq? dir 'r))
                                              'h
                                              'v))
                                    result))))
    (if (null? d-list)
      l-list
      (calc (cdr d-list) (if (caar d-list)
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
   

;;
(define fail-screen
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
  (if (= (stage-selecting env) 6)
      (tri-select (txt-returntitle (title-clear SCENE)))
      (tri-select (txt-selectstage (txt-nextstage (title-success
                                                   ;;test用 (select-print env SCENE)
                                                             SCENE
                                                             ))))))

(define (success-key-event env key)
  (define prev "up")
  (define next "down")
  (define do "\r")
  (if (= (stage-selecting env) 6)
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
