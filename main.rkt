#lang racket

(module+ test
  (require rackunit))

;; A Color is one of
(define WHITE 'white)
(define RED 'red)
(define BLUE 'blue)
(define ORANGE 'orange)
(define GREEN 'green)
(define YELLOW 'yellow)
(define cube-colors (list WHITE RED BLUE ORANGE GREEN YELLOW))

; A FaceConfiguration is a (hash-of Color (listof Color))
; Representing a mapping from each face to its adjacent faces in clockwise order
; Example
(define cube-face-config
  (hash WHITE (list RED GREEN ORANGE BLUE)
        RED (list WHITE BLUE YELLOW GREEN)
        BLUE (list RED WHITE ORANGE YELLOW)
        ORANGE (list WHITE GREEN YELLOW BLUE)
        GREEN (list ORANGE WHITE RED YELLOW)
        YELLOW (list RED BLUE ORANGE GREEN)))

;; A Puzzle is a
(struct puzzle [face-config sticker-colors] #:transparent)
;; where
;; face-config is the puzzle's FaceConfig
;; sticker-colors is a (hash-of StickerSpec Color) representing the colors of each sticker
;; Represents a twisty puzzle that twists at the faces
;; examples:
(define (solved-3x3-cube)
  (make-solved-3x3-puzzle cube-face-config))

;; A StickerSpec is a list of 1-3 colors. '(red) is the red center, '(red blue) is the edge sticker on the red face between
;; the red and blue centers, and '(red blue white) is the corner sticker on the red face between the red, blue, and white centers.
;; Represents the location of a sticker on the cube in terms of the centers.
;; CONSTRAINT: no duplicate colors in the list.
;; CONSTRAINT: In the context of a puzzle, to avoid duplicate sticker specs for a corner piece, a corner StickerSpec's final two colors must appear in the same order
;; as in the puzzle's face configuration. For example, for a 3x3 cube, '(white red blue) is valid, but '(white blue red) is not.
;; CONSTRAINT: In the context of a puzzle, all colors must be of mutually adjacent faces. For example, '(white yellow) is not valid on a 3x3
;; TODO think of a representation that generalizes to bigger cubes like 4x4. maybe include numbers in second and third entries

;; A FaceMove is a
(struct face-move [face-color num-turns] #:transparent)
;; Representing a move that rotates a single face, specified by the face color.
;; where
;; face-color is a Color representing the face that is being rotated
;; num-turns is a Natural representing the number of quarter-turns to rotate the face clockwise
;; TODO think of a representation that generalizes to bigger cubes like 4x4. need to be able to express slice moves. maybe face + depth?

; Puzzle -> Boolean
; Is the puzzle solved?
(define (puzzle-solved? pzl)
  (for/and ([(spec color) (in-hash (puzzle-sticker-colors pzl))])
    (equal? color (spec-face-color spec))))

; FaceConfiguration -> Puzzle
(define (make-solved-3x3-puzzle face-config)
  (make-solved-puzzle-from-specs face-config (specs-for-3x3-puzzle face-config)))

(module+ test
  (check-true (puzzle-solved? (make-solved-3x3-puzzle cube-face-config))))

; FaceConfig (listof StickerSpec) -> Puzzle
; make a solved puzzle from the sticker specs
(define (make-solved-puzzle-from-specs face-config specs)
  (puzzle
   face-config
   (for/hash ([spec specs])
     (values spec (spec-face-color spec)))))

; (listof StickerSpec)
; vacuously solved!
(define specs-for-0x0-puzzle (list))

; FaceConfig -> (listof StickerSpec)
(define (specs-for-1x1-puzzle face-config)
  ; just centers
  ; always solved
  (map list (face-configuration-face-colors face-config)))

; FaceConfig -> (listof StickerSpec)
; get a 2x2 puzzle's sticker specs
(define (specs-for-2x2-puzzle face-configuration)
  ; a 2x2 is a 3x3 without centers or edges!
  (filter corner-spec? (specs-for-3x3-puzzle face-configuration)))

; FaceConfiguration -> (listof StickerSpec)
; get a 3x3 puzzle's sticker specs
(define (specs-for-3x3-puzzle face-config)
  (apply append (for/list ([face-color (face-configuration-face-colors face-config)])
                  (specs-for-face-color face-config face-color))))

; FaceConfig Color -> (listof StickerSpec)
; Get the specs for stickers on the given face. Order does not matter.
(define (specs-for-face-color face-config face-color)
    (define adjacent-face-colors (face-configuration-adjacent-face-colors face-config face-color))
    (define corner-pairs
      (cons (list (first adjacent-face-colors) (last adjacent-face-colors))
            (for/list ([first-color adjacent-face-colors]
                       [second-color (rest adjacent-face-colors)])
              (list first-color second-color))))
    (append
     ; face
     (list (make-center-spec face-color))
     ; edges
     (for/list ([adjacent-face-color adjacent-face-colors])
       (make-edge-spec face-color adjacent-face-color))
     ; corners
     (for/list ([corner-pair corner-pairs])
       (make-corner-spec face-color (first corner-pair) (second corner-pair)))))

(module+ test
  ; check that two lists are the same when order doesn't matter
  (define-check (check-same-multiset a b)
    (check-equal? (apply set a) (apply set b))
    (check-equal? (length a) (length b)))
  (let ([specs (specs-for-face-color cube-face-config WHITE)])
    (check-same-multiset specs
                         (list (make-center-spec WHITE)
                               (make-edge-spec WHITE RED)
                               (make-edge-spec WHITE GREEN)
                               (make-edge-spec WHITE ORANGE)
                               (make-edge-spec WHITE BLUE)
                               (make-corner-spec WHITE RED BLUE)
                               (make-corner-spec WHITE RED GREEN)
                               (make-corner-spec WHITE GREEN ORANGE)
                               (make-corner-spec WHITE ORANGE BLUE)))))

; Puzzle FaceMove -> Puzzle
(define (perform-face-move pzl mov)
  (for/fold ([pzl pzl])
            ([_ (in-range (modulo (face-move-num-turns mov) (puzzle-num-face-turns pzl)))])
    (perform-clockwise-face-rotation pzl (face-move-face-color mov))))

; Puzzle Color -> Puzzle
; Rotate the given face clockwise once
(define (perform-clockwise-face-rotation pzl face-color)
  (define face-config (puzzle-face-config pzl))
  (define adjacent-face-colors (face-configuration-adjacent-face-colors face-config face-color))
  (define original-pzl pzl)
  (for/fold ([pzl pzl])
            ([adjacent-face-color adjacent-face-colors])
    (define next-face-color (face-configuration-next-face-color face-config face-color adjacent-face-color))
    (define prev-face-color (face-configuration-prev-face-color face-config face-color adjacent-face-color))
    (define prev-prev-face-color (face-configuration-prev-face-color face-config face-color prev-face-color))
    (define spec-pairs
      ; (list dest source) pairs to set the stickers between the rotated and adjacent face
      ; adjacent face gets previous face's stickers (previous as in counter clockwise to the adjacent face from the rotated face)
      (list
       ; set the stickers on the rotated face
       (list (normalize-corner-spec (make-corner-spec face-color adjacent-face-color prev-face-color) face-config)
             (normalize-corner-spec (make-corner-spec face-color prev-face-color prev-prev-face-color) face-config))
       (list (make-edge-spec face-color adjacent-face-color)
             (make-edge-spec face-color prev-face-color))
       (list (normalize-corner-spec (make-corner-spec face-color adjacent-face-color next-face-color) face-config)
             (normalize-corner-spec (make-corner-spec face-color prev-face-color adjacent-face-color) face-config))
       ; set the stickers on the adjacent face
       (list (normalize-corner-spec (make-corner-spec adjacent-face-color face-color prev-face-color) face-config)
             (normalize-corner-spec (make-corner-spec prev-face-color face-color prev-prev-face-color) face-config))
       (list (make-edge-spec adjacent-face-color face-color)
             (make-edge-spec prev-face-color face-color))
       (list (normalize-corner-spec (make-corner-spec adjacent-face-color face-color next-face-color) face-config)
             (normalize-corner-spec (make-corner-spec prev-face-color face-color adjacent-face-color) face-config))))
    (for/fold ([pzl pzl])
              ([pair spec-pairs])
      (define dest (first pair))
      (define source (second pair))
      (puzzle-set-sticker-color pzl dest (puzzle-sticker-color original-pzl source)))))

(module+ test
  (let ([pzl (perform-clockwise-face-rotation (solved-3x3-cube) WHITE)])
    (check-equal? (puzzle-sticker-color pzl (make-edge-spec RED WHITE))
                  BLUE)
    (check-equal? (puzzle-sticker-color pzl (make-edge-spec BLUE WHITE))
                  ORANGE)
    (check-equal? (puzzle-sticker-color pzl (make-edge-spec ORANGE WHITE))
                  GREEN)
    (check-equal? (puzzle-sticker-color pzl (make-edge-spec GREEN WHITE))
                  RED)
    (check-equal? (puzzle-sticker-color pzl (make-corner-spec GREEN WHITE RED))
                  RED)
    (define pzl^ (perform-clockwise-face-rotation pzl RED))
    (check-equal? (puzzle-sticker-color pzl^ (make-edge-spec RED WHITE))
                  RED)
    (check-equal? (puzzle-sticker-color pzl^ (make-edge-spec RED BLUE))
                  BLUE)
    (check-equal? (puzzle-sticker-color pzl^ (make-edge-spec WHITE RED))
                  GREEN)
    (check-equal? (puzzle-sticker-color pzl^ (make-corner-spec WHITE RED BLUE))
                  RED)
    (check-equal? (puzzle-sticker-color pzl^ (make-corner-spec WHITE RED GREEN))
                  GREEN)
    (check-equal? (puzzle-sticker-color pzl^ (make-edge-spec YELLOW RED))
                  BLUE)
    (check-equal? (puzzle-sticker-color pzl^ (make-corner-spec YELLOW RED BLUE))
                  ORANGE)))

(define (puzzle-set-sticker-color pzl spec color)
  (struct-copy puzzle pzl
               [sticker-colors (hash-set (puzzle-sticker-colors pzl) spec color)]))

(define (puzzle-sticker-color pzl spec)
  (hash-ref (puzzle-sticker-colors pzl) spec))

; number of times to turn a face to get it back to where it started. On a cube, this number is 4.
; The order of the cyclic group generated by a single face turn.
(define (puzzle-num-face-turns pzl)
  (face-configuration-num-face-turns (puzzle-face-config pzl)))

; number of times to turn a face to get it back to where it started. On a cube, this number is 4.
; The order of the cyclic group generated by a single face turn.
(define (face-configuration-num-face-turns face-config)
  (if (zero? (length (face-configuration-face-colors)))
      0
      ; assume all faces have the same number of turns
      (length (face-configuration-adjacent-face-colors face-config (first (face-configuration-face-colors face-config))))))

; FaceConfiguration -> (listof Color)
; get all face colors, order doesn't matter
(define (face-configuration-face-colors face-config)
  (hash-keys face-config))

; FaceConfiguration Color -> (listof Color)
; get a face's adjacent face colors in clockwise order.
; First face doesn't have any inherent significance, but does matter to the implementation.
(define (face-configuration-adjacent-face-colors face-config face-color)
  (hash-ref face-config face-color))

(define (face-configuration-next-face-color face-config face-color adjacent-face-color)
  (define adjacent-colors (face-configuration-adjacent-face-colors face-config face-color))
  (define index-of-adjacent-face-color (index-of adjacent-colors adjacent-face-color))
  (unless index-of-adjacent-face-color
    (error 'face-configuration-next-face-color
           "adjacent face color not adjacent. face color: ~a adjacent face color: ~a"
           face-color
           adjacent-face-color))
  (list-ref adjacent-colors (modulo (add1 index-of-adjacent-face-color) (length adjacent-colors))))

(module+ test
  (check-equal? (face-configuration-next-face-color cube-face-config WHITE RED)
                GREEN)
  (check-equal? (face-configuration-next-face-color cube-face-config WHITE GREEN)
                ORANGE)
  (check-equal? (face-configuration-next-face-color cube-face-config WHITE ORANGE)
                BLUE)
  (check-equal? (face-configuration-next-face-color cube-face-config WHITE BLUE)
                RED))

(define (face-configuration-prev-face-color face-config face-color adjacent-face-color)
  (define adjacent-colors (face-configuration-adjacent-face-colors face-config face-color))
  (define index-of-adjacent-face-color (index-of adjacent-colors adjacent-face-color))
  (unless index-of-adjacent-face-color
    (error 'face-configuration-next-face-color
           "adjacent face color not adjacent. face color: ~a adjacent face color: ~a"
           face-color
           adjacent-face-color))
  (list-ref adjacent-colors (modulo (sub1 index-of-adjacent-face-color) (length adjacent-colors))))

(module+ test
  (check-equal? (face-configuration-prev-face-color cube-face-config WHITE GREEN)
                RED)
  (check-equal? (face-configuration-prev-face-color cube-face-config WHITE ORANGE)
                GREEN)
  (check-equal? (face-configuration-prev-face-color cube-face-config WHITE BLUE)
                ORANGE)
  (check-equal? (face-configuration-prev-face-color cube-face-config WHITE RED)
                BLUE))

(define (make-center-spec color) (list color))
; face color is the face that it's on, other color is the face it's near
(define (make-edge-spec face-color other-face-color)
  (list face-color other-face-color))
; face color is the face it's on, first and second color say which faces it's near.
; CONSTRAINT: first should be before second in the face's list of adjacent face colors according to the face config to avoid duplicates.
(define (make-corner-spec face-color first-face-color second-face-color)
  (list face-color first-face-color second-face-color))

; StickerSpec -> Color
; get the color of the face that a sticker spec is on
(define (spec-face-color spec) (first spec))

; StickerSpec -> Boolean
; is the spec for a corner sticker?
(define (corner-spec? spec) (= 3 (length spec)))

; StickerSpec FaceConfiguration -> StickerSpec
; make sure the corner spec is valid wrt the face configuration. Fixes it or errors if it isn't.
(define (normalize-corner-spec spec face-config)
  (define face-color (spec-face-color spec))
  (define second-color (second spec))
  (define third-color (third spec))
  (define adjacent-colors (face-configuration-adjacent-face-colors face-config face-color))
  (define index-of-second (index-of adjacent-colors second-color))
  (define index-of-third (index-of adjacent-colors third-color))
  (unless (and index-of-second
               index-of-third
               (or (= 1 (abs (- index-of-second index-of-third)))
                   (= (sub1 (length adjacent-colors)) (abs (- index-of-second index-of-third)))))
    (error 'normalize-corner-spec "corner spec doesn't have mutually adjacent face colors"))
  (if (< index-of-second index-of-third)
      (make-corner-spec face-color second-color third-color)
      (make-corner-spec face-color third-color second-color)))

(define (display-3x3 pzl)
  (define (color->char color) (substring (symbol->string color) 0 1))
  (define (spec->char spec)
    (color->char (puzzle-sticker-color pzl spec)))
  ;   ooo
  ;   ooo
  ;   ooo
  ;bbbyyyggg
  ;bbbyyyggg
  ;bbbyyyggg
  ;   rrr
  ;   rrr
  ;   rrr
  ;   www
  ;   www
  ;   www
  (string-join
   (list
    (format "   ~a~a~a"
            (spec->char (normalize-corner-spec (make-corner-spec ORANGE WHITE BLUE) cube-face-config))
            (spec->char (make-edge-spec ORANGE WHITE))
            (spec->char (normalize-corner-spec (make-corner-spec ORANGE WHITE GREEN) cube-face-config)))
    (format "   ~a~a~a"
            (spec->char (make-edge-spec ORANGE BLUE))
            (spec->char (make-center-spec ORANGE))
            (spec->char (make-edge-spec ORANGE GREEN)))
    (format "   ~a~a~a"
            (spec->char (normalize-corner-spec (make-corner-spec ORANGE YELLOW BLUE) cube-face-config))
            (spec->char (make-edge-spec ORANGE YELLOW))
            (spec->char (normalize-corner-spec (make-corner-spec ORANGE YELLOW GREEN) cube-face-config)))
    (format "~a~a~a~a~a~a~a~a~a"
            (spec->char (normalize-corner-spec (make-corner-spec BLUE WHITE ORANGE) cube-face-config))
            (spec->char (make-edge-spec BLUE ORANGE))
            (spec->char (normalize-corner-spec (make-corner-spec BLUE YELLOW ORANGE) cube-face-config))

            (spec->char (normalize-corner-spec (make-corner-spec YELLOW BLUE ORANGE) cube-face-config))
            (spec->char (make-edge-spec YELLOW ORANGE))
            (spec->char (normalize-corner-spec (make-corner-spec YELLOW ORANGE GREEN) cube-face-config))

            (spec->char (normalize-corner-spec (make-corner-spec GREEN YELLOW ORANGE) cube-face-config))
            (spec->char (make-edge-spec GREEN ORANGE))
            (spec->char (normalize-corner-spec (make-corner-spec GREEN WHITE ORANGE) cube-face-config)))
    (format "~a~a~a~a~a~a~a~a~a"
            (spec->char (make-edge-spec BLUE WHITE))
            (spec->char (make-center-spec BLUE))
            (spec->char (make-edge-spec BLUE YELLOW))


            (spec->char (make-edge-spec YELLOW BLUE))
            (spec->char (make-center-spec YELLOW))
            (spec->char (make-edge-spec YELLOW GREEN))

            (spec->char (make-edge-spec GREEN YELLOW))
            (spec->char (make-center-spec GREEN))
            (spec->char (make-edge-spec GREEN WHITE)))
    (format "~a~a~a~a~a~a~a~a~a"
            (spec->char (normalize-corner-spec (make-corner-spec BLUE WHITE RED) cube-face-config))
            (spec->char (make-edge-spec BLUE RED))
            (spec->char (normalize-corner-spec (make-corner-spec BLUE YELLOW RED) cube-face-config))

            (spec->char (normalize-corner-spec (make-corner-spec YELLOW BLUE RED) cube-face-config))
            (spec->char (make-edge-spec YELLOW RED))
            (spec->char (normalize-corner-spec (make-corner-spec YELLOW RED GREEN) cube-face-config))

            (spec->char (normalize-corner-spec (make-corner-spec GREEN YELLOW RED) cube-face-config))
            (spec->char (make-edge-spec GREEN RED))
            (spec->char (normalize-corner-spec (make-corner-spec GREEN WHITE RED) cube-face-config)))
    (format "   ~a~a~a"
            (spec->char (normalize-corner-spec (make-corner-spec RED YELLOW BLUE) cube-face-config))
            (spec->char (make-edge-spec RED YELLOW))
            (spec->char (normalize-corner-spec (make-corner-spec RED YELLOW GREEN) cube-face-config)))
    (format "   ~a~a~a"
            (spec->char (make-edge-spec RED BLUE))
            (spec->char (make-center-spec RED))
            (spec->char (make-edge-spec RED GREEN)))
    (format "   ~a~a~a"
            (spec->char (normalize-corner-spec (make-corner-spec RED WHITE BLUE) cube-face-config))
            (spec->char (make-edge-spec RED WHITE))
            (spec->char (normalize-corner-spec (make-corner-spec RED WHITE GREEN) cube-face-config)))

    (format "   ~a~a~a"
            (spec->char (normalize-corner-spec (make-corner-spec WHITE RED BLUE) cube-face-config))
            (spec->char (make-edge-spec WHITE RED))
            (spec->char (normalize-corner-spec (make-corner-spec WHITE RED GREEN) cube-face-config)))
    (format "   ~a~a~a"
            (spec->char (make-edge-spec WHITE BLUE))
            (spec->char (make-center-spec WHITE))
            (spec->char (make-edge-spec WHITE GREEN)))
    (format "   ~a~a~a"
            (spec->char (normalize-corner-spec (make-corner-spec WHITE ORANGE BLUE) cube-face-config))
            (spec->char (make-edge-spec WHITE ORANGE))
            (spec->char (normalize-corner-spec (make-corner-spec WHITE ORANGE GREEN) cube-face-config))))))
