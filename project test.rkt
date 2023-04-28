#lang racket
#|
The program macro takes a list of instructions and creates a list containing those instructions.
The instruction-addx macro takes an integer and returns an S-expression representing the addx instruction with the given integer as its argument.
The instruction-noop macro returns an S-expression representing the noop instruction.
The parse-integer function takes a string and returns a cons cell containing the integer at the beginning of the string and the index of the character after the integer.
The parse function takes a file name, reads the contents of the file, and uses parse-integer to parse each line into an instruction. It returns a list of instructions.
The interp function takes a list of instructions and a number of cycles to simulate. It uses a recursive loop function to simulate the execution of the instructions
for the given number of cycles. The loop function takes the current value of the X-register, the remaining instructions to execute, the current cycle number, and a
list of signals that have been generated so far. If there are no more instructions or the maximum number of cycles has been reached, the loop function returns the list of signals.
Otherwise, it executes the first instruction in the list and updates the X-register and signals list accordingly. The loop function then calls itself recursively with the updated values.
validate-instructions function takes a list of instructions as input and returns true if all instructions are either "noop" or "addx" with a valid integer value.
logic function takes a file name as input and returns the sum of the X-register values at the end of cycles 20, 60, 100, 140, 180, and 220. It calls parse to get the list
of instructions from the file, then calls validate-instructions to check that the instructions are valid. Finally, it calls interp with the list of instructions and each cycle value,
then sums the X-register values returned by interp.
|#
#|Grammar
<program> ::= <instruction>*
<instruction> ::= "addx" <integer>
                | "noop"
<integer> ::= <sign>? <digit>+
<sign> ::= "+" | "-"
<digit> ::= "0" | "1" | ... | "9"|#

(define-syntax-rule (program instructions ...)
  (list instructions ...))

(define-syntax-rule (instruction-addx integer)
  `(addx ,integer))

(define-syntax-rule (instruction-noop)
  'noop)

;; Parser

(define (parse-integer str)
  (let ((matches (regexp-match #rx"[-+]?[0-9]+" str)))
    (if matches
        (let ((integer (string->number (car matches))))
          (display "Parsed integer value: ")
          (displayln integer)
          integer)
        false)))



(define (parse file)
  (let loop ((port (open-input-file file))
             (instructions '()))
    (let ((line (read-line port)))
      (if (eof-object? line)
          (reverse instructions)
          (let* ((words (string-split line))
                 (instruction (car words)))
            (cond ((string=? instruction "noop")
                   (loop port (cons (instruction-noop) instructions)))
                  ((string-prefix? "addx" instruction)
                   (let ((integer (parse-integer (cadr words))))
                     (if integer
                         (loop port (cons (instruction-addx integer) instructions))
                         (error (format "Invalid instruction: ~a" line)))))
                  (else (error (format "Invalid instruction: ~a" line)))))))))





;; interp:

(define (interp file cycles)
  (let* ([instructions (parse file)]
         [cycle-list '(20 60 100 140 180 220)])
    (let loop ([X-register 1]
               [instructions instructions]
               [cycle 1]
               [signals '()])
      (cond
        [(or (null? instructions) (> cycle cycles))
         (reverse signals)]
        [(string=? (caar instructions) "noop")       (loop X-register (cdr instructions) (+ cycle 1) signals)]
        [else       (let* ([v (cadr (car instructions))]
                            [X-register (+ X-register v)])
                      (if (= cycle cycles)
                          (loop X-register (cdr instructions) (+ cycle 1) (cons X-register signals))
                          (loop X-register (cdr instructions) (+ cycle 1) signals)))]))))


;; logic functions
(define (validate-instructions instructions)
  (andmap (lambda (inst)
            (or (eq? inst 'noop)
                (and (list? inst)
                     (eq? (car inst) 'addx)
                     (integer? (cadr inst)))))
          instructions))

(define (print-part1 instructions cycle-list)
  (define signal-strengths (map (lambda (cycle) (car (interp instructions cycle))) cycle-list))
  (display "Signal Strengths: ")
  (display signal-strengths)
  (newline))

(define (print-part2 instructions cycle-list)
  (define signal-strengths (map (lambda (cycle) (car (interp instructions cycle))) cycle-list))
  (define sum (apply + (map * cycle-list signal-strengths)))
  (display "Sum of Signal Strengths: ")
  (display sum)
  (newline))

(define (run file)
  (let* ([instructions (parse file)]
         [cycle-list '(20 60 100 140 180 220)])
    (when (not (validate-instructions instructions))
      (error "Invalid instructions"))
    (print-part1 instructions cycle-list)
    (print-part2 instructions cycle-list)))

(run "D:/day-10-1.txt")





