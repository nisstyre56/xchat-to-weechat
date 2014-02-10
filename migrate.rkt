#! /usr/bin/racket
#lang racket

(define (parse-flags bitf)
  (filter identity
   (map
    (lambda (flag)
      (cond
        [(bitwise-bit-set? bitf (cadr flag))
          (cons
            (car flag)
            (caddr flag))]
        [else #f]))
    [list (list "try_all_servers" 0 "no weechat equivalent")
      (list "use_default" 1 #t)
      (list "ssl" 2 "on")
      (list "autoconnect" 3 "on")
      (list "proxy" 4 "\"\"")
      (list "ssl_verify" 5 "off")
      (list "favourited" 6 "no weechat equivalent")])))

(define (make-server params)
  (let* ([lines (regexp-split
                  #px"\n"
                  params)]
         [assocs (map parse-equation
                  (filter-not
                    (curry equal? "")
                    lines))])
    (make-hash (gather-dups
                 (map
                   (lambda (p)
                     (match (car p)
                      ["F"
                       (cons
                         (translate-prop (car p))
                         (parse-flags
                           (string->number (cdr p))))]
                      [_ (cons
                            (translate-prop (car p))
                            (cdr p))]))
                   assocs)))))

(define (parse-equation line)
  (let* ([splitted (regexp-split
                    #px"="
                    line)]
        [name (car splitted)]
        [val (cadr splitted)])
    (cons name val)))

(define (parse-file str)
  (let ([servers (regexp-split
                   #px"(?m:^[:blank:]*$)"
                   str)])
    (map make-server (drop servers 1))))

(define (get-xchat)
  (parse-file
    (file->string
      (string->path
        "/home/wes/.xchat2/servlist_.conf"))))

(define translate-prop
  (let ([hash (make-hash
    '(["N" . "name"]
      ["I" . "nicks"]
      ["i" . "nicks"]
      ["U" . "username"]
      ["R" . "realname"]
      ["J" . "autojoin"]
      ["B" . "password"]
      ["S" . "addresses"]
      ["P" . "password"]
      ["E" . "encoding"]
      ["C" . "connect_cmd"]
      ["F" . "flags"]
      ["D" . "primary_server_number"]))])
    (lambda (key)
      (hash-ref hash key ""))))

(define translate-val
  (let ([hash (make-hash
    (list
      [cons "addresses" (lambda (val) (format "\"~a\"" val))]
      [cons "realname" (lambda (val) (regexp-replace #px"\\s" val ""))]
      [cons "username" (lambda (val) (regexp-replace #px"\\s" val ""))]))])
    (lambda (prop val)
      ((hash-ref hash
                prop
                (const identity))
       val))))

(define (take-while pred xs)
  (cond
    [(empty? xs) '()]
    [(not (pred (car xs))) '()]
    [else (cons
            (car xs)
            (take-while pred (cdr xs)))]))

(define (drop-while pred xs)
  (cond
    [(empty? xs) '()]
    [(not (pred (car xs))) xs]
    [else (drop-while pred (cdr xs))]))

(define (take-dups xs)
  (take-while
    (lambda (x) (equal?
                  (caar xs)
                  (car x)))
    xs))

(define (drop-dups xs)
  (drop-while
    (lambda (x) (equal?
                  (caar xs)
                  (car x)))
    xs))

(define (group-sorted xs)
  (cond
    [(empty? xs) '()]
    [else
      (let ([chunk (take-dups xs)]
            [rest (drop-dups xs)])
        (cons
          chunk
          (group-sorted rest)))]))

(define (collapse-pairs pairs)
  (cons
    (caar pairs)
    (list
      (map cdr pairs))))

(define (gather-dups assocs)
  (let* ([sorted
          (sort assocs
                string<?
                #:key car)])
    (map collapse-pairs
         (group-sorted sorted))))

(define (output-field server h field)
  (let ([result (hash-ref h field #f)])
    (if result
      (displayln (format
                  "~a.~a = ~a"
                  (caar server)
                  field
                  (translate-val field (caar result))))
      (displayln
        (format
          "~a.~a"
          (caar server)
          field)))))

(define (handle-flags h)
  (match (hash-ref h "flags" #f)
    [#f h]
    [flags
     (match flags
       ['() h]
       [flags
         (for ([flag (caar flags)])
          (hash-set! h (car flag) `((,(cdr flag))) ))
          (hash-remove! h "flags")
          h])]))

(define weechat-properties
  '("addresses"
    "proxy"
    "ipv6"
    "ssl"
    "ssl_cert"
    "ssl_priorities"
    "ssl_dhkey_size"
    "ssl_verify"
    "password"
    "capabilities"
    "sasl_mechanism"
    "sasl_username"
    "sasl_password"
    "sasl_timeout"
    "autoconnect"
    "autoreconnect"
    "autoreconnect_delay"
    "nicks"
    "username"
    "realname"
    "local_hostname"
    "command"
    "command_delay"
    "autojoin"
    "autorejoin"
    "autorejoin_delay"
    "connection_timeout"
    "anti_flood_prio_high"
    "anti_flood_prio_low"
    "away_check"
    "away_check_max_nicks"
    "default_msg_part"
    "default_msg_quit"
    "notify"))

(for ([h (map handle-flags (get-xchat))] #:unless
                      (not (hash-ref h "name" #f)))
     (let ([out
             (curry output-field (hash-ref h "name") h)])
       (displayln "[Server]")
       (for ([prop weechat-properties])
            (out prop))
       (displayln "")))
