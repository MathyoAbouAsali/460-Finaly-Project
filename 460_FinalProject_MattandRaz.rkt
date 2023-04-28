#lang racket
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
    (let ((x-value (run-cycle instructions 1 cycle)))
      (newline)
      (display "Cycle ")
      (display cycle)
      (display " has x-value ")
      (display x-value)
      (let ((signal-strength (* cycle x-value)))
        (display " and signal strength ")
        (display signal-strength)
        signal-strength)))

  (let ((signal-strengths (map (lambda (cycle) (compute-signal-strength instructions cycle)) cycles)))
    (newline)
    (display "Sum of signal strengths: ")
    (display (apply + signal-strengths)))
  (newline))


;; example usage

(define instructions (parse "D:DDrive Downloads/day10simple.txt"))
(interp instructions '(20 60 100 140 180 220))
(newline)
