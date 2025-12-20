(eval-when (compile load eval)
  (define (ensure-string s)
    (cond ((string? s) s)
          ((symbol? s) (symbol->string s))
          (else #f)))

  (define (enforce-string l)
    (for-each
     (lambda (x)
       (unless (string? x)
         (error (format #f "~a in ~a: is not a STRING!" x l))))
     l))

  (define (symbol-format fmt . args)
    "str * &rest[(str U sym)] -> symbol
     syntax passed must be converted to datum"
    (let* ((strgs    (map ensure-string args))
           (_ (enforce-string strgs))
           (fmtd-str (apply format `(#f ,fmt ,@strgs))))
      (string->symbol fmtd-str)))

  (define (syntax-format stx fmt . args)
    "stx * str * &rest[(str U sym)] -> identifier"
    (let* ((strgs
            (map (lambda (x)
                   (cond ((ensure-string x) => identity)
                         (else
                          (or (ensure-string (syntax->datum x))
                              x))))
                 args))
           (_ (enforce-string strgs))
           (fmtd-str (apply format `(#f ,fmt ,@strgs)))
           (fmtd-sym (string->symbol fmtd-str)))
      (datum->syntax stx fmtd-sym)))
  #||#)
