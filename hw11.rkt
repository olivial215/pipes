;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw8_dylanp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; IMPORTED FROM HW6 aka PART 1 OF PIPE DREAM


;; PART 2 TASK 1
(define-struct pipe [top bot left right])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for 
;; one of top, bot, left, right indicates an opening in that direction.
;; A Starting Pipe contains only one #true 

(define PIPE-TL (make-pipe #true #false #true #false))
(define PIPE-TR (make-pipe #true #false #false #true))
(define PIPE-BR (make-pipe #false #true #false #true))
(define PIPE-BL (make-pipe #false #true #true #false))
(define PIPE-TB (make-pipe #true #true #false #false))
(define PIPE-LR (make-pipe #false #false #true #true))
(define PIPE-TBLR (make-pipe #true #true #true #true))

(define START-PIPE-T (make-pipe #true #false #false #false))
(define START-PIPE-B (make-pipe #false #true #false #false))
(define START-PIPE-L (make-pipe #false #false #true #false))
(define START-PIPE-R (make-pipe #false #false #false #true))

(define (pipe-temp p)
  (cond
    [(pipe-top p) ...]
    [(pipe-bot p) ...]
    [(pipe-left p) ...]
    [(pipe-right p) ...]))


(define ALL-POSSIBLE-PIPES (list PIPE-TL PIPE-TR PIPE-BR PIPE-BL PIPE-TB PIPE-LR PIPE-TBLR))



;; PART 2 TASK 2
;; pipe->image: Pipe Integer Integer Boolean String -> Image

;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length.
;; If filled? then draw the pipe with goo
;; gf-direction is "up" "down" "left" "right" or "none"
;; If pipe is PIPE-TBLR and filled? then draw pipe given goo flow direction. 
(define (pipe->image pipe tile-side-length pipe-width filled? gf-direction)
  (local [; pcolor : Boolean -> String
          ; given filled status, returns the color of the pipe 
               (define (pcolor f)
               (if (boolean=? f #f)
                   "black"
                   "green"))]
         (cond [(and filled? (pipe-top pipe) (pipe-bot pipe) (pipe-left pipe) (pipe-right pipe)) 
                (cond [(or (string=? gf-direction "right") (string=? gf-direction "left")) 
                       (overlay (rectangle tile-side-length pipe-width "solid" "green")
                                (draw-tblr tile-side-length pipe-width "black"))]
                      [(or (string=? gf-direction "up") (string=? gf-direction "down"))
                       (overlay (rectangle pipe-width tile-side-length "solid" "green")
                                (draw-tblr tile-side-length pipe-width "black"))])]
               [(and (pipe-top pipe) (pipe-bot pipe) (pipe-left pipe) (pipe-right pipe))
                (draw-tblr tile-side-length pipe-width "black")] ; PIPE-TBLR
               [(and (pipe-top pipe) (pipe-bot pipe)) 
                (draw-tb tile-side-length pipe-width (pcolor filled?))] ; PIPE-TB
               [(and (pipe-left pipe) (pipe-right pipe)) 
                (draw-lr tile-side-length pipe-width (pcolor filled?))] ; PIPE-LR
               [(and (pipe-top pipe) (pipe-left pipe))
                (draw-tl tile-side-length pipe-width (pcolor filled?))] ; PIPE-TL
               [(and (pipe-top pipe) (pipe-right pipe))
                (draw-tr tile-side-length pipe-width (pcolor filled?))] ; PIPE-TR
               [(and (pipe-bot pipe) (pipe-right pipe))
                (draw-br tile-side-length pipe-width (pcolor filled?))] ; PIPE-BR
               [(and (pipe-bot pipe) (pipe-left pipe))
                (draw-bl tile-side-length pipe-width (pcolor filled?))] ; PIPE-BL
               [(pipe-top pipe)
                (draw-t tile-side-length pipe-width (pcolor filled?))] ; PIPE-T
               [(pipe-bot pipe)
                (draw-b tile-side-length pipe-width (pcolor filled?))] ; PIPE-B
               [(pipe-left pipe)
                (draw-l tile-side-length pipe-width (pcolor filled?))] ; PIPE-L
               [(pipe-right pipe)
                (draw-r tile-side-length pipe-width (pcolor filled?))]))) ; PIPE-R

   
(define TILECOLOR "gray")

; draw-vertical : Number Number -> Image
; draws a vertical rectangle for constructing pipes  
(define (draw-vertical tsl pw)
  (rectangle (/ (- tsl pw) 2) tsl "solid" TILECOLOR))

; draw-corner : Number Number -> Image
; draws a corner for constructing pipes 
(define (draw-corner tsl pw)
  (rectangle (/ (- tsl pw) 2) (/ (- tsl pw) 2) "solid" TILECOLOR))

(define (draw-tblr tsl pw pcolor)
  (overlay/align "right" "top" (draw-corner tsl pw)
                 (overlay/align "left" "top" (draw-corner tsl pw)
                                (overlay/align "right" "bottom" (draw-corner tsl pw)
                                               (overlay/align "left" "bottom" (draw-corner tsl pw)
                                                              (square tsl "solid" pcolor))))))

(define (draw-tb tsl pw pcolor)
  (overlay/align "left" "middle" (draw-vertical tsl pw)
                 (overlay/align "right" "middle" (draw-vertical tsl pw)
                                (square tsl "solid" pcolor))))

(define (draw-lr tsl pw pcolor)
  (rotate 90 (draw-tb tsl pw pcolor)))

(define (draw-tl tsl pw pcolor)
  (overlay/align "right" "middle" (draw-vertical tsl pw)
                 (overlay/align "middle" "bottom" (rotate 90 (draw-vertical tsl pw))
                                (overlay/align "left" "top" (draw-corner tsl pw)
                                               (square tsl "solid" pcolor)))))

(define (draw-tr tsl pw pcolor)
  (rotate 270 (draw-tl tsl pw pcolor)))

(define (draw-br tsl pw pcolor)
  (rotate 180 (draw-tl tsl pw pcolor)))

(define (draw-bl tsl pw pcolor)
  (rotate 90 (draw-tl tsl pw pcolor)))

(define (draw-t tsl pw pcolor)
  (overlay/align "middle" "bottom" (rotate 90 (draw-vertical tsl pw)) (draw-tb tsl pw pcolor)))

(define (draw-b tsl pw pcolor)
  (overlay/align "middle" "top" (rotate 90 (draw-vertical tsl pw)) (draw-tb tsl pw pcolor)))

(define (draw-l tsl pw pcolor)
  (overlay/align "right" "middle" (draw-vertical tsl pw) (draw-lr tsl pw pcolor)))

(define (draw-r tsl pw pcolor)
  (overlay/align "left" "middle" (draw-vertical tsl pw) (draw-lr tsl pw pcolor)))


(define-struct pipe-coord [pipe row col])
; A PipeCoord is a [Pipe Number Number]
; Interpretation: pipe location on a square grid
(define PC-START1 (make-pipe-coord START-PIPE-R 5 1))
(define PC-START2 (make-pipe-coord START-PIPE-T 3 4))
(define PC-START3 (make-pipe-coord START-PIPE-B 1 2))
(define PCOORD1 (make-pipe-coord PIPE-TL 1 2)) 
(define PCOORD2 (make-pipe-coord PIPE-TR 2 3))
(define PCOORD3 (make-pipe-coord PIPE-LR 6 6))


(define-struct grid [lopc n])
; A Grid is a (make-grid [ListOf PipeCoord] Number)
; interpretation: Pipes placed on a n x n grid of squares length
(define STARTING-GRID (make-grid (list PC-START1) 7))
(define EMPTY-GRID (make-grid '() 7))
(define GRID1 (make-grid (list PCOORD1) 7))
(define GRID2 (make-grid (list PCOORD1 PCOORD2 PCOORD3) 7))
(define GRID3 (make-grid (list (make-pipe-coord PIPE-TL 2 4) (make-pipe-coord PIPE-LR 2 3)) 7))


;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.
(check-expect (place-pipe EMPTY-GRID PIPE-TL 1 1)
              (make-grid (list (make-pipe-coord
                                (make-pipe #true #false #true #false) 1 1)) 7))
(check-expect (place-pipe GRID1 PIPE-TB 7 7)
              (make-grid (list (make-pipe-coord (make-pipe #true #true #false #false) 7 7)
                               (make-pipe-coord (make-pipe #true #false #true #false) 1 2)) 7))

(define (place-pipe grid pipe row col)
  (local
    ; add-pipe : [ListOf PipeCoord] PipeCoord -> [ListOf PipeCoord]
    ; adds given pipe coord to list
    [(define (add-pipe lopc pc)
       (cond
         [(empty? lopc) (cons pc '())]
         [else (cons pc lopc)]))]
  (make-grid (add-pipe (grid-lopc grid) (make-pipe-coord pipe row col)) (grid-n grid))))


;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.
(check-expect (pipe-at EMPTY-GRID 1 1) #false)
(check-expect (pipe-at GRID1 1 2) (make-pipe #true #false #true #false))
(check-expect (pipe-at GRID2 5 5) #false)
(check-expect (pipe-at GRID2 6 6) (make-pipe #false #false #true #true))

(define (pipe-at grid row col)
  (local
    ; find-pipe [ListOf PipeCoord] Number Number -> [Optional Pipe]
    ; given list of pipecoords, returns pipe thats in given coords or #f if empty 
    [(define (find-pipe lopc row col)
      (cond [(empty? lopc) #false]
            [(cons? lopc) (if (and (= (pipe-coord-row (first lopc)) row)
                                   (= (pipe-coord-col (first lopc)) col))
                              (pipe-coord-pipe (first lopc))
                              (find-pipe (rest lopc) row col))]))]
  (find-pipe (grid-lopc grid) row col)))


;; grid->image: Grid Integer Integer -> Image
;; Draws the grid of pipes. Every tile should be a square with side length
;; tile-side-length and every pipe should have width pipe-width.
(define (grid->image grid tsl pw gf)
  (local [; draw-row : Number -> Image
          ; draws the given row
          (define (draw-row row)
            (grid-row->image grid row 0 tsl pw gf))]
    (foldr above empty-image (build-list (grid-n grid) draw-row))))

; grid-row->image : Grid Integer Integer Integer GooFlow-> Image
; Produces image of row given grid, current row, tile side length, pipe width, and GooFlow  
(define (grid-row->image grid row col tsl pw gf)
  (cond [(empty? (grid-lopc grid))
         (foldr beside
                (empty-tile tsl)
                (make-list (- (grid-n grid) 1) (empty-tile tsl)))]
        [(= col (grid-n grid)) empty-image]
        [else (if (false? (pipe-at grid row col))
                  (beside
                   (empty-tile tsl)
                   (grid-row->image grid row (+ 1 col) tsl pw gf))
                  (beside
                   empty-image
                   (pipe->image (pipe-at grid row col) tsl pw (in-goo-flow? (make-pipe-coord (pipe-at grid row col) row col) gf) (goo-flow-direction (grid-goo-propagate gf grid)))
                   
                   (grid-row->image grid row (+ 1 col) tsl pw gf)))]))

;; in-goo-flow? : PipeCoord GooFlow -> Boolean
;; Is the pipe in the list of pipes of GooFlow?
(define (in-goo-flow? p gf)
  (local [;; equal-pipes : Pipe -> Boolean
          ;; Are the given pipes equal?
          (define (equal-pipes gf-pipe)
            (and (= (pipe-coord-col gf-pipe)
                    (pipe-coord-col p))
                 (= (pipe-coord-row gf-pipe)
                    (pipe-coord-row p))))]
  (ormap equal-pipes (goo-flow-path gf))))

; empty-tile : Number -> Image
; given tile side length, produces image of an empty tile
(check-expect (empty-tile 30) (overlay (square 30 "outline" "black") (square 30 "solid" "white")))
(check-expect (empty-tile 20) (overlay (square 20 "outline" "black") (square 20 "solid" "white")))
(check-expect (empty-tile 10) (overlay (square 10 "outline" "black") (square 10 "solid" "white")))
(define (empty-tile tsl)
  (overlay (square tsl "outline" "black") (square tsl "solid" "white")))



;; PART 2 TASK 3
(define-struct goo-flow [path direction])
; A GooFlow is a (make-gooflow [ListOf PipeCoord] String)
; Interpretation: the path taken by the goo and the direction in which it is flowing 

(define GF-START1 (make-goo-flow (list PC-START1) "right"))
(define GF-START2 (make-goo-flow (list PC-START2) "up"))
(define GF-START3 (make-goo-flow (list PC-START3) "down"))
(define GF1 (make-goo-flow (list (make-pipe-coord START-PIPE-B 1 1)
                                 (make-pipe-coord PIPE-TB 2 1)
                                 (make-pipe-coord PIPE-TR 3 1)) "right"))
(define GF2 (make-goo-flow (list (make-pipe-coord START-PIPE-B 1 1)
                                 (make-pipe-coord PIPE-TB 2 1)
                                 (make-pipe-coord PIPE-TB 3 1)) "down"))
(define GF3 (make-goo-flow (list (make-pipe-coord START-PIPE-B 1 4)
                                 (make-pipe-coord PIPE-TL 2 4)
                                 (make-pipe-coord PIPE-LR 2 3)) "left"))



;; PART 2 TASK 4
; grid-goo-propagate : GooFlow Grid -> GooFlow
; moves goo forward by one tile. If goo is stuck, produce the same goo

(check-expect (grid-goo-propagate GF1 (make-grid (list (make-pipe-coord START-PIPE-B 1 1)
                                                       (make-pipe-coord PIPE-TB 2 1)
                                                       (make-pipe-coord PIPE-TR 3 1)
                                                       (make-pipe-coord PIPE-BL 3 2)
                                                       (make-pipe-coord PIPE-LR 4 2)) 7))
              (make-goo-flow (list (make-pipe-coord START-PIPE-B 1 1)
                                   (make-pipe-coord PIPE-TB 2 1)
                                   (make-pipe-coord PIPE-TR 3 1)
                                   (make-pipe-coord PIPE-BL 3 2)) "down"))

(check-expect (grid-goo-propagate GF2 (make-grid (list (make-pipe-coord START-PIPE-B 1 1)
                                                       (make-pipe-coord PIPE-TB 2 1)
                                                       (make-pipe-coord PIPE-TB 3 1)
                                                       (make-pipe-coord PIPE-TB 4 1)
                                                       (make-pipe-coord PIPE-TR 5 1)) 7))
              (make-goo-flow (list (make-pipe-coord START-PIPE-B 1 1)
                                   (make-pipe-coord PIPE-TB 2 1)
                                   (make-pipe-coord PIPE-TB 3 1)
                                   (make-pipe-coord PIPE-TB 4 1)) "down"))

(check-expect (grid-goo-propagate (make-goo-flow (list (make-pipe-coord START-PIPE-B 1 1)
                                                       (make-pipe-coord PIPE-TB 2 1)
                                                       (make-pipe-coord PIPE-TL 3 1)
                                                       (make-pipe-coord PIPE-LR 3 0)) "left")
                                  (make-grid (list (make-pipe-coord START-PIPE-B 1 1)
                                                    (make-pipe-coord PIPE-TB 2 1)
                                                    (make-pipe-coord PIPE-TL 3 1)
                                                    (make-pipe-coord PIPE-LR 3 0)
                                                    (make-pipe-coord PIPE-BR 3 -1)) 7))
              (make-goo-flow (list (make-pipe-coord START-PIPE-B 1 1)
                                   (make-pipe-coord PIPE-TB 2 1)
                                   (make-pipe-coord PIPE-TL 3 1)
                                   (make-pipe-coord PIPE-LR 3 0)) "left"))

(check-expect (grid-goo-propagate (make-goo-flow (list (make-pipe-coord START-PIPE-B 1 1)
                                                       (make-pipe-coord PIPE-TB 2 1)
                                                       (make-pipe-coord PIPE-TL 3 1)
                                                       (make-pipe-coord PIPE-LR 3 0)) "left")
                                  (make-grid (list (make-pipe-coord START-PIPE-B 1 1)
                                                    (make-pipe-coord PIPE-TB 2 1)
                                                    (make-pipe-coord PIPE-TL 3 1)
                                                    (make-pipe-coord PIPE-LR 3 0)
                                                    (make-pipe-coord PIPE-BR 3 -1)) 7))
              (make-goo-flow (list (make-pipe-coord START-PIPE-B 1 1)
                                   (make-pipe-coord PIPE-TB 2 1)
                                   (make-pipe-coord PIPE-TL 3 1)
                                   (make-pipe-coord PIPE-LR 3 0)) "left"))

(check-expect (grid-goo-propagate (make-goo-flow (list (make-pipe-coord START-PIPE-T 1 4)
                                                       (make-pipe-coord PIPE-TB 1 5)
                                                       (make-pipe-coord PIPE-TB 1 6)) "up")
                                  (make-grid (list (make-pipe-coord START-PIPE-T 1 4)
                                                   (make-pipe-coord PIPE-TB 1 5)
                                                   (make-pipe-coord PIPE-TB 1 6)
                                                   (make-pipe-coord PIPE-TB 1 7)) 7))
              (make-goo-flow (list (make-pipe-coord START-PIPE-T 1 4)
                                   (make-pipe-coord PIPE-TB 1 5)
                                   (make-pipe-coord PIPE-TB 1 6)) "up"))

(check-expect (grid-goo-propagate (make-goo-flow (list (make-pipe-coord START-PIPE-B 1 1)
                                                       (make-pipe-coord PIPE-TB 2 1)
                                                       (make-pipe-coord PIPE-TR 3 1)
                                                       (make-pipe-coord PIPE-LR 3 2)) "right")
                                  (make-grid (list (make-pipe-coord START-PIPE-B 1 1)
                                                    (make-pipe-coord PIPE-TB 2 1)
                                                    (make-pipe-coord PIPE-TR 3 1)
                                                    (make-pipe-coord PIPE-LR 3 2)
                                                    (make-pipe-coord PIPE-BL 3 3)) 7))
              (make-goo-flow (list (make-pipe-coord START-PIPE-B 1 1)
                                   (make-pipe-coord PIPE-TB 2 1)
                                   (make-pipe-coord PIPE-TR 3 1)
                                   (make-pipe-coord PIPE-LR 3 2)
                                   (make-pipe-coord PIPE-BL 3 3)) "down"))

(check-expect (grid-goo-propagate GF3 (make-grid (list (make-pipe-coord START-PIPE-B 1 4)
                                                       (make-pipe-coord PIPE-TL 2 4)
                                                       (make-pipe-coord PIPE-LR 2 3)) 7))
              GF3)

(check-expect (grid-goo-propagate GF3 (make-grid (list (make-pipe-coord START-PIPE-B 1 4)
                                                       (make-pipe-coord PIPE-TL 2 4)
                                                       (make-pipe-coord PIPE-LR 2 3)
                                                       (make-pipe-coord PIPE-TB 2 2)) 7))
              GF3)

(check-expect (grid-goo-propagate (make-goo-flow (list (make-pipe-coord START-PIPE-B 1 1)
                                                       (make-pipe-coord PIPE-TB 2 1)
                                                       (make-pipe-coord PIPE-TR 3 1)
                                                       (make-pipe-coord PIPE-LR 3 2)) "right")
                                  (make-grid (list (make-pipe-coord START-PIPE-B 1 1)
                                                    (make-pipe-coord PIPE-TB 2 1)
                                                    (make-pipe-coord PIPE-TR 3 1)
                                                    (make-pipe-coord PIPE-LR 3 2)
                                                    (make-pipe-coord PIPE-TBLR 3 3)) 7))
              (make-goo-flow (list (make-pipe-coord START-PIPE-B 1 1)
                                   (make-pipe-coord PIPE-TB 2 1)
                                   (make-pipe-coord PIPE-TR 3 1)
                                   (make-pipe-coord PIPE-LR 3 2)
                                   (make-pipe-coord PIPE-TBLR 3 3)) "right"))

(check-expect (grid-goo-propagate (make-goo-flow (list (make-pipe-coord START-PIPE-B 1 1)
                                                       (make-pipe-coord PIPE-TB 2 1)
                                                       (make-pipe-coord PIPE-TR 3 1)
                                                       (make-pipe-coord PIPE-LR 3 2)) "right")
                                  (make-grid (list (make-pipe-coord START-PIPE-B 1 1)
                                                    (make-pipe-coord PIPE-TB 2 1)
                                                    (make-pipe-coord PIPE-TR 3 1)
                                                    (make-pipe-coord PIPE-LR 3 2)
                                                    (make-pipe-coord PIPE-TL 3 3)) 7))
              (make-goo-flow (list (make-pipe-coord START-PIPE-B 1 1)
                                   (make-pipe-coord PIPE-TB 2 1)
                                   (make-pipe-coord PIPE-TR 3 1)
                                   (make-pipe-coord PIPE-LR 3 2)
                                   (make-pipe-coord PIPE-TL 3 3)) "up"))

(define (grid-goo-propagate gf g)
  (local
    [; next-row : PipeCoord -> Number
     ; returns the row of next tile based on direction of gooflow 
     (define (next-row pc)
       (cond [(string=? (goo-flow-direction gf) "up") (- (pipe-coord-row pc) 1)]
             [(string=? (goo-flow-direction gf) "down") (+ (pipe-coord-row pc) 1)]
             [(string=? (goo-flow-direction gf) "left") (pipe-coord-row pc)]
             [(string=? (goo-flow-direction gf) "right") (pipe-coord-row pc)]))
     ; next-col : PipeCoord -> Number
     ; returns the column of next tile based on direction of gooflow 
     (define (next-col pc)
       (cond [(string=? (goo-flow-direction gf) "up") (pipe-coord-col pc)]
             [(string=? (goo-flow-direction gf) "down") (pipe-coord-col pc)]
             [(string=? (goo-flow-direction gf) "left") (- (pipe-coord-col pc) 1)]
             [(string=? (goo-flow-direction gf) "right") (+ (pipe-coord-col pc) 1)]))
     ; update-direction : Pipe -> String
     ; returns updated direction given the pipe at the next tile (assuming a there is a valid pipe there)  
     (define (update-direction p) 
       (cond [(or (and (pipe-top p) (pipe-bot p) (pipe-left p) (pipe-right p))
                  (and (pipe-top p) (pipe-bot p))
                  (and (pipe-left p) (pipe-right p)))
              (goo-flow-direction gf)]
             [(or (string=? (goo-flow-direction gf) "down")
                  (string=? (goo-flow-direction gf) "up"))
              (if (pipe-left p) "left" "right")]
             [(or (string=? (goo-flow-direction gf) "left")
                  (string=? (goo-flow-direction gf) "right"))
              (if (pipe-top p) "up" "down")]))
     ; last-pipe : [ListOf PipeCoord] -> Pipe
     ; returns the last pipe in a list of pipes
     (define (last-pipe lopc)
       (first (reverse lopc)))]
                                
    (cond [(or (> (next-row (last-pipe (goo-flow-path gf))) (- (grid-n g) 1))
               (< (next-row (last-pipe (goo-flow-path gf))) 0)
               (> (next-col (last-pipe (goo-flow-path gf))) (- (grid-n g) 1))
               (< (next-col (last-pipe (goo-flow-path gf))) 0))
           gf]
          
          [(false? (pipe-at g (next-row (last-pipe (goo-flow-path gf)))
                              (next-col (last-pipe (goo-flow-path gf)))))
           gf]
          
          [(or (and (string=? (goo-flow-direction gf) "right")
                    (false? (pipe-left (pipe-at g (next-row (last-pipe (goo-flow-path gf)))
                                                  (next-col (last-pipe (goo-flow-path gf)))))))
               (and (string=? (goo-flow-direction gf) "left")
                    (false? (pipe-right (pipe-at g (next-row (last-pipe (goo-flow-path gf)))
                                                   (next-col (last-pipe (goo-flow-path gf)))))))

               (and (string=? (goo-flow-direction gf) "up")
                    (false? (pipe-bot (pipe-at g (next-row (last-pipe (goo-flow-path gf)))
                                                 (next-col (last-pipe (goo-flow-path gf)))))))

               (and (string=? (goo-flow-direction gf) "down")
                    (false? (pipe-top (pipe-at g (next-row (last-pipe (goo-flow-path gf)))
                                                 (next-col (last-pipe (goo-flow-path gf))))))))
           gf]
          
          [else (make-goo-flow
                 (foldr cons (list (make-pipe-coord
                                    (pipe-at g (next-row (last-pipe (goo-flow-path gf)))
                                              (next-col (last-pipe (goo-flow-path gf))))
                                    (next-row (last-pipe (goo-flow-path gf)))
                                    (next-col (last-pipe (goo-flow-path gf))))) (goo-flow-path gf))
                 (update-direction (pipe-at g (next-row (last-pipe (goo-flow-path gf)))
                                              (next-col (last-pipe (goo-flow-path gf))))))])))



;; PART 2 TASK 5
(define-struct gamestate [grid inc-pipes tsl pw pstart gf])
;; A GameState is a (make-gamestate Grid [List-of Pipe] Integer Integer PipeCoord GooFlow)
;; Interpretation: The state of the game, which consists of
;; - grid - the current grid state
;; - inc-pipes - a list of the incoming pipes
;; - tsl - the tile-side length
;; - pw - the pipe width
;; - pstart - the starting pipe
;; - gf - the GooFlow, which starts at the starting pipe
(define STARTING-GS (make-gamestate STARTING-GRID
                                    (list PIPE-TL PIPE-TB PIPE-TBLR PIPE-BL PIPE-TB PIPE-BR PIPE-LR)
                                    48
                                    16
                                    PC-START1
                                    GF-START1))
(define GS1 (make-gamestate (make-grid (list PC-START2) 7)
                            empty
                            48
                            16
                            PC-START2
                            GF-START2))
(define GS2 (make-gamestate (make-grid (append (grid-lopc GRID2) (list PC-START1)) 7)
                            empty
                            48
                            16
                            PC-START1
                            GF-START1))
(define GS3 (make-gamestate (make-grid (append (grid-lopc GRID1) (list PC-START3)) 7)
                            (list PIPE-BR PIPE-LR PIPE-BL PIPE-TR PIPE-TBLR PIPE-LR)
                            48
                            16
                            PC-START3
                            GF-START3))
(define GS4 (make-gamestate (make-grid (append (grid-lopc GRID3) (goo-flow-path GF3)) 7)
                            (list PIPE-TL PIPE-TB PIPE-TBLR PIPE-BL PIPE-TB PIPE-BR PIPE-LR)
                            48
                            16
                            (make-pipe-coord START-PIPE-B 1 4)
                            GF3))
;; Template:
;; gs-temp : GameState -> ?
(define (gs-temp gs)
  (... (gamestate-grid gs) ...
       (if (empty? (gamestate-inc-pipes gs)) ... ...)
       (gamestate-tsl gs) ...
       (gamestate-pw gs) ...
       (gamestate-pstart gs) ...
       (gamestate-gf gs) ...))



;; PART 2 TASK 6
;; gamestate-init : Number Number Number Direction [List-of Pipe] -> GameState
;; Initializes the gamestate using the grid dimension, the x and y coordinates of the starting pipe,
;; the direction of the starting pipe, and the list of incoming pipes
(check-expect (gamestate-init 7 5 1 "right" (list PIPE-TL PIPE-TB PIPE-TBLR PIPE-BL PIPE-TB PIPE-BR PIPE-LR)) STARTING-GS)
(check-expect (gamestate-init 7 3 4 "up" empty) GS1)
(check-expect (gamestate-init 10 5 2 "left" (list PIPE-BR PIPE-TBLR PIPE-LR PIPE-TB PIPE-TB PIPE-TR PIPE-BL))
              (make-gamestate (make-grid (list (make-pipe-coord START-PIPE-L 5 2)) 10)
                              (list PIPE-BR PIPE-TBLR PIPE-LR PIPE-TB PIPE-TB PIPE-TR PIPE-BL)
                              48
                              16
                              (make-pipe-coord START-PIPE-L 5 2)
                              (make-goo-flow (list (make-pipe-coord START-PIPE-L 5 2)) "left")))
(check-expect (gamestate-init 4 0 3 "down" (list PIPE-TBLR PIPE-TBLR PIPE-LR PIPE-TB PIPE-BR PIPE-BL))
              (make-gamestate (make-grid (list (make-pipe-coord START-PIPE-B 0 3)) 4)
                              (list PIPE-TBLR PIPE-TBLR PIPE-LR PIPE-TB PIPE-BR PIPE-BL)
                              48
                              16
                              (make-pipe-coord START-PIPE-B 0 3)
                              (make-goo-flow (list (make-pipe-coord START-PIPE-B 0 3)) "down")))

(define (gamestate-init grid-dim sp-x sp-y sp-dir inc-pipes)
  (make-gamestate (make-grid (list (make-pipe-coord (starting-pipe sp-dir) sp-x sp-y)) grid-dim)
                  inc-pipes
                  48
                  16
                  (make-pipe-coord (starting-pipe sp-dir) sp-x sp-y)
                  (make-goo-flow (list (make-pipe-coord (starting-pipe sp-dir) sp-x sp-y)) sp-dir)))


;; starting-pipe : Direction -> Pipe
;; Chooses the starting pipe based on the direction given
(check-expect (starting-pipe "up") START-PIPE-T)
(check-expect (starting-pipe "down") START-PIPE-B)
(check-expect (starting-pipe "left") START-PIPE-L)
(check-expect (starting-pipe "right") START-PIPE-R)
(define (starting-pipe dir)
  (cond
    [(string=? dir "up") START-PIPE-T]
    [(string=? dir "down") START-PIPE-B]
    [(string=? dir "left") START-PIPE-L]
    [(string=? dir "right") START-PIPE-R]))



;; PART 2 TASK 7
;; display-pipes : [List-of Pipe] -> Image
;; Displays the pipes in a list of pipes, from left to right
(check-expect (display-pipes ALL-POSSIBLE-PIPES) (beside (pipe->image PIPE-TL 48 16 #f "none")
                                                         (beside (pipe->image PIPE-TR 48 16 #f "none")
                                                                 (beside (pipe->image PIPE-BR 48 16 #f "none")
                                                                         (beside (pipe->image PIPE-BL 48 16 #f "none")
                                                                                 (beside (pipe->image PIPE-TB 48 16 #f "none")
                                                                                         (beside (pipe->image PIPE-LR 48 16 #f "none")
                                                                                                 (beside (pipe->image PIPE-TBLR 48 16 #f "none")
                                                                                                         (square 1 "solid" "white")))))))))
(check-expect (display-pipes (list PIPE-TB PIPE-TL PIPE-LR PIPE-TBLR PIPE-LR PIPE-BR PIPE-TB))
              (beside (pipe->image PIPE-TB 48 16 #f "none")
                      (beside (pipe->image PIPE-TL 48 16 #f "none")
                              (beside (pipe->image PIPE-LR 48 16 #f "none")
                                      (beside (pipe->image PIPE-TBLR 48 16 #f "none")
                                              (beside (pipe->image PIPE-LR 48 16 #f "none")
                                                      (beside (pipe->image PIPE-BR 48 16 #f "none")
                                                              (beside (pipe->image PIPE-TB 48 16 #f "none")
                                                                      (square 1 "solid" "white")))))))))
(define (display-pipes lop)
  (cond
    [(empty? lop) (square 1 "solid" "white")]
    [(cons? lop) (beside (pipe->image (first lop) 48 16 #f "none")
                         (display-pipes (rest lop)))]))


;; draw-grid : GameState -> Image
;; Draws the grid for the game
(define (draw-grid gs)
  (above/align "left"
               (grid->image (gamestate-grid gs) (gamestate-tsl gs) (gamestate-pw gs) (gamestate-gf gs))
               (display-pipes (gamestate-inc-pipes gs))))




;; PART 2 TASK 8

;; place-pipe-or-propagate-goo : GameState Integer Integer MouseEvent -> GameState
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile.
;; If the user clicks on the screen and there are no incoming pipes available,
;; propagates the goo one tile.
(check-expect (place-pipe-or-propagate-goo STARTING-GS 100 150 "button-down")
              (make-gamestate (make-grid (append (list (make-pipe-coord PIPE-TL 3 2)) (grid-lopc (gamestate-grid STARTING-GS))) 7)
                              (list PIPE-TB PIPE-TBLR PIPE-BL PIPE-TB PIPE-BR PIPE-LR) 48 16 PC-START1 GF-START1))
(check-expect (place-pipe-or-propagate-goo GS1 0 0 "button-down")
              GS1)
(check-expect (place-pipe-or-propagate-goo GS2 100 100 "button-down") GS2)
(check-expect (place-pipe-or-propagate-goo GS3 100 50 "button-down")
              (make-gamestate (make-grid (append (list (make-pipe-coord PIPE-BR 1 2)) (grid-lopc (gamestate-grid GS3))) 7)
                              (list PIPE-LR PIPE-BL PIPE-TR PIPE-TBLR PIPE-LR) 48 16 PC-START3 GF-START3))
(check-expect (place-pipe-or-propagate-goo (make-gamestate (make-grid (list PC-START2 (make-pipe-coord PIPE-TB 2 4)) 7)
                                                           empty
                                                           48
                                                           16
                                                           PC-START2
                                                           GF-START2)
                                           100 100 "button-down")
              (make-gamestate (make-grid (list PC-START2 (make-pipe-coord PIPE-TB 2 4)) 7)
                              empty
                              48
                              16
                              PC-START2
                              (make-goo-flow (append (goo-flow-path GF-START2) (list (make-pipe-coord PIPE-TB 2 4))) "up")))

(define (place-pipe-or-propagate-goo gs x y me)
  (if (string=? me "button-down")
      (cond
        [(empty? (gamestate-inc-pipes gs)) (make-gamestate (gamestate-grid gs)
                                                           empty
                                                           (gamestate-tsl gs)
                                                           (gamestate-pw gs)
                                                           (gamestate-pstart gs)
                                                           (grid-goo-propagate (gamestate-gf gs) (gamestate-grid gs)))]
        [(cons? (gamestate-inc-pipes gs)) (make-gamestate (place-pipe (gamestate-grid gs)
                                                                      (first (gamestate-inc-pipes gs))
                                                                      (quotient y (gamestate-tsl gs))
                                                                      (quotient x (gamestate-tsl gs)))
                                                          (rest (gamestate-inc-pipes gs))
                                                          (gamestate-tsl gs)
                                                          (gamestate-pw gs)
                                                          (gamestate-pstart gs)
                                                          (gamestate-gf gs))])
      gs))



;; pipe-fantasy: GameState -> GameState
(define (pipe-fantasy initial-game-state)
  (big-bang
    initial-game-state
    [to-draw draw-grid]
    [on-mouse place-pipe-or-propagate-goo]))

;; TO PLAY, USE (pipe-fantasy STARTING-GS)



;; PART 2 TASK 9
(define GS-EX-1 (gamestate-init 7 1 1 "down" (list PIPE-TB PIPE-TL PIPE-LR PIPE-TBLR PIPE-LR PIPE-BR PIPE-TB)))
(define GS-EX-2 (gamestate-init 7 5 1 "right" (list PIPE-TL PIPE-TB PIPE-TBLR PIPE-BL PIPE-TB PIPE-BR PIPE-LR)))