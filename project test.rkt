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
          (begin
            (display "List of instructions: ")
            (display instructions)
            (newline)
            (reverse instructions))
          (let* ((words (string-split line))
                 (instruction (car words)))
            (cond ((string=? instruction "noop")
                   (loop port (append instructions (list (instruction-noop)))))
                  ((string-prefix? "addx" instruction)
                   (let ((integer (parse-integer (cadr words))))
                     (if integer
                         (loop port (append instructions (list (instruction-addx integer))))
                         (error (format "Invalid instruction: ~a" line)))))
                  (else (error (format "Invalid instruction: ~a" line)))))))))

;; interp:

(define (interp instructions cycles)
  (define (run-cycle instruction-list x-value cycle)
    (if (null? instruction-list)
        x-value
        (let* ((instruction (car instruction-list))
               (new-x-value (if (eq? instruction 'noop)
                                x-value
                                (+ x-value (cadr instruction)))))
          (if (= cycle 0)
              new-x-value
              (run-cycle (cdr instruction-list) new-x-value (- cycle 1))))))

  (define (compute-signal-strength instructions cycle)
    (* cycle (run-cycle instructions 1 cycle)))

  (for-each (lambda (cycle)
              (let ((signal-strength (compute-signal-strength instructions cycle)))
                (display "Cycle ")
                (display cycle)
                (display " has signal strength ")
                (displayln signal-strength)))
            cycles))


(define instructions (parse "D:/day-10-1.txt"))
(interp instructions '(20 60 100 140 180 220))





