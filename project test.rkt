#|
4/17/23
Final Project: https://adventofcode.com/2022/day/10
Mathyo Abou Asali - Razie Hyria
|#
;; ============================================================

#lang racket

;; Define a macro that creates a list of instructions.
(define-syntax-rule (program instructions ...)
  (list instructions ...))

;; Define a macro that creates an addx instruction with the given integer value.
(define-syntax-rule (instruction-addx integer)
  `(addx ,integer))

;; Define a macro that creates a noop instruction.
(define-syntax-rule (instruction-noop)
  'noop)


;; Parser-----------------------------------------------------------------

;; Parses a noop instruction and returns an 'instruction-noop object
(define (parse-noop)
  (instruction-noop))

;; Parses an addx instruction from the provided string
(define (parse-addx str)
  (let ((matches (regexp-match #rx"[-+]?[0-9]+" str))) ; Uses a regular expression to match an optional sign followed by digits in the string
    (if matches
        (string->number (car matches)) ; If there is a match, converts the matched string to a number and returns it
        (error (format "Invalid addx instruction: ~a" str))))) ; Otherwise, raises an error indicating an invalid instruction

;; Parses a line from the provided file, and returns the corresponding instruction
(define (parse-line line)
  (let* ((words (string-split line)) ; Splits the line into words
         (instruction (car words))) ; Gets the first word as the instruction
    (cond ((string=? instruction "noop") ; If the instruction is "noop", parses a noop instruction
           (parse-noop))
          ((string-prefix? "addx" instruction) ; If the instruction starts with "addx", parses an addx instruction from the second word
           (parse-addx (cadr words)))
          (else (error (format "Invalid instruction: ~a" line)))))) ; Otherwise, raises an error indicating an invalid instruction

;; Parses instructions from the provided file, and returns a list of corresponding instructions
(define (parse file)
  (with-input-from-file file ; Opens the file and creates an input port
    (lambda ()
      (let loop ((instructions '())) ; Defines a recursive procedure with an accumulator for the parsed instructions
        (let ((line (read-line))) ; Reads the next line from the input port
          (if (eof-object? line) ; If the end of the file is reached, returns the list of parsed instructions
              (begin
                (display "List of instructions: ")
                (display instructions)
                (newline)
                instructions)
              (let ((instruction (parse-line line))) ; Otherwise, parses the line and appends the resulting instruction to the accumulator
                (loop (append instructions (list instruction))))))))))

;; logic-----------------------------------------------------------------

;; handle-noop function that adds 1 to the current value of X
(define (handle-noop x-value)
  (+ x-value 1))

;; handle-addx function that adds the integer value to the current value of X and addx-total
(define (handle-addx x-value addx-total value)
  (values (+ x-value 2) (+ addx-total value)))

;; execute function that runs the program instructions and returns the total signal strength of a give cycle
(define (execute instructions cycle)
  (let ((addx-total 0)
        (x-value 1))
    (let loop ((remaining-instructions instructions))
      (if (or (null? remaining-instructions) (= x-value cycle))
          (+ 2 addx-total)
          (let ((current-instruction (car remaining-instructions)))
            (cond ((eq? 'noop current-instruction)
                   (set! x-value (handle-noop x-value))) ;; handle noop instruction
                  ((integer? current-instruction)
                   (let-values (((new-x-value new-addx-total) (handle-addx x-value addx-total current-instruction)))
                     (set! x-value new-x-value)
                     (set! addx-total new-addx-total)))) ;; handle addx instruction
            (if (= x-value cycle)
                (+ 1 addx-total) ;; signal strength is 1 plus the total addx value
                (loop (cdr remaining-instructions))))))))

;; compute-signal function that computes the signal strength and the value of X during a given cycle
(define (compute-signal instructions cycle)
  (let ((x-value (execute instructions cycle)))
    (list (* x-value cycle) x-value))) ;; return a list of signal strength and X-value

;; compute-cycle-signal function that computes and displays the signal strength and the value of X for a given cycle
(define (compute-cycle-signal instructions cycle)
  (let* ((signal-and-x (compute-signal instructions cycle))
         (signal-strength (car signal-and-x))
         (x-value (cadr signal-and-x)))
    (display "During the ")
    (display cycle)
    (display "th cycle, register X has the value ")
    (display x-value)
    (display " so the signal strength is ")
    (displayln signal-strength)
    signal-strength)) ;; return the signal strength

;; main---------------------------------------------------------------------------------------
(define (main instructions cycles)  
  (let ((total-signal-strength 0))   
    (for-each (lambda (cycle)  ;; For each cycle, compute the signal strength and add it to the total
                (set! total-signal-strength
                      (+ total-signal-strength (compute-cycle-signal instructions cycle))))
              cycles)    
    (display "The total signal strength is ")  ;; Print the total signal strength
    (displayln total-signal-strength)))

; Parse the instructions file and store the result in a variable
(define instructions (parse "D:/day-10-1.txt"))

; Compute the signal strength for the specified cycles and print the result
(main instructions '(20 60 100 140 180 220))








