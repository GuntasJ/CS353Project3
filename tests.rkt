#lang racket

(require rackunit)
(require "project3.rkt")

(define test-account '(456109801804 "\"J. L. Picard\"" 0.0))

;test function: get-account-number
(check-equal?
    (get-account-number test-account)
    456109801804)

;test function: get-account-name
(check-equal?
    (get-account-name test-account)
    "J. L. Picard")

;test function: get-account-balance
(check-equal?
    (get-account-balance test-account)
    0.0)

;test function: sublist
(check-equal?
    (sublist 2 4'(1 2 3 4 5))
    '(3 4))

;test function: create-purchase
(check-equal? 
    (create-purchase '("Purchase" "234987234981" "23456" "Culvers" "14.72"))
    (list 'Purchase 234987234981 23456 "Culvers" 14.72))

;test function: create-payment
(check-equal? 
    (create-payment '("Payment"	"982340982348"	"23460"	"Credit"	"23098479087234"	"10000.00"))
    (list 'Payment 982340982348 23460 'Credit "23098479087234" 10000.00))

;test function: get-payment-type
(check-equal?
    (get-payment-type (create-payment '("Payment"	"982340982348"	"23460"	"Credit"	"23098479087234"	"10000.00")))
    'Credit)