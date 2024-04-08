#lang racket

(provide get-account-number
         get-account-balance
         get-account-name
         sublist
         create-purchase
         create-payment
         get-payment-type)

;λ
;Helper sublist function to help parse some lists
;start is inlcusive, end is exclusive
(define (sublist start end lst)
    (define (make-sublist current end resulting-list)
        (cond
            [(= current end) resulting-list]
            [else (make-sublist (add1 current) end (append resulting-list (list (list-ref lst current))))]))
    (make-sublist start end '()))
    
;This function takes in a split purchase line by whitepace, and returns a list with the 'Purchase
;tag on it. This function is intended to be called on over the whole file. 
(define (create-purchase split-purchase-line)
        (list
            'Purchase
            (string->number (second split-purchase-line))
            (string->number (third split-purchase-line))
            (string-join (sublist 3 (sub1 (length split-purchase-line)) split-purchase-line))
            (string->number (list-ref split-purchase-line (sub1 (length split-purchase-line))))))

;This function takes in a split payment line by whitespace, and returns a list with the 'Payment tag
;It also uses some helper functions as payment has other types on it as well. This will be used
;over the file.
(define (create-payment split-payment-line)
    (append 
        (list 
            'Payment 
            (string->number (second split-payment-line))
            (string->number (third split-payment-line)))
        (cond 
            [(equal? (fourth split-payment-line) "Credit") (create-card-payment 'Credit split-payment-line)]
            [(equal? (fourth split-payment-line) "Debit")  (create-card-payment 'Debit split-payment-line)]
            [(equal? (fourth split-payment-line) "Cash")   (create-cash-payment split-payment-line)]
            [(equal? (fourth split-payment-line) "Check")  (create-check-payment split-payment-line)]
            [else (list 'Invalid-Payment)])))


;This function will attach a credit or debit tag on to the payment list. 
(define (create-card-payment card-type split-payment-line)
    (list
        card-type
        (fifth split-payment-line)
        (string->number (sixth split-payment-line))))

;This function will attach a cash tag on to the payment list
(define (create-cash-payment split-payment-line)
    (list
        'Cash
        (string->number (fifth split-payment-line))))

;This functino will attack a check tag on to the payment list
(define (create-check-payment split-payment-line)
    (list
        'Check
        (fifth split-payment-line)
        (string->number (sixth split-payment-line))))

;Reads in the ACCOUNTS.TXT file and returns the accounts in the form of lists
(define (create-accounts)
    (define account-file (file->lines "ACCOUNTS.TXT"))
    ;helper inner function to create a single account
    (define (create-account account-line)
        (define split-account-line (string-split account-line))
        ;List for account, in the form of accountNo, name (this needs to be stripped of whitespace), balance
        (list
            (string->number (first split-account-line))
            (string-join (sublist 1 (sub1 (length split-account-line)) split-account-line))
            (string->number (list-ref split-account-line (sub1 (length split-account-line))))))
    (map create-account account-file))

;Reads in the TRANSACTIONS.txt file and return the transactions in the form of lists
(define (create-transactions)
    (define transaction-file (file->lines "TRANSACTIONS.txt"))
    ;helper inner function to create a single transaction
    (define (create-transaction transaction-line)
        (define split-transaction-line (string-split transaction-line))
        ;Calls either purchase or payment making functiond depending on input tag
        (if (string=? (first split-transaction-line) "Purchase")
            (create-purchase split-transaction-line)
            (create-payment split-transaction-line)))
    ;Filters out the invalid payments. This is handeling the edge cases where the payment method is not supported.
    (filter (lambda (transaction) (not (equal? 'Invalid-Payment (fourth transaction)))) (map create-transaction transaction-file)))
    

;Helper methods to extract from transactions & accounts
(define (get-account-number account)
    (first account))

(define (get-transaction-account-number transaction)
    (second transaction))

;Gets account name and strips the quotations that surround it 
(define (get-account-name account)
    (define pre-edit-string (second account))
    (substring pre-edit-string 1 (sub1 (string-length pre-edit-string))))

(define (get-account-balance account)
    (third account))

(define (get-transaction-timestamp transaction)
    (third transaction))

(define (get-transaction-type transaction)
    (first transaction))

;Gets purchase name and strips the quotations that surround it
(define (get-purchase-name purchase)
    (define pre-edit-string (fourth purchase))
    (substring pre-edit-string 1 (sub1 (string-length pre-edit-string))))

(define (get-payment-type payment)
    (fourth payment))

(define (get-purchase-amount purchase)
    (fifth purchase))

;Gets the payment amount. As cash does not have a name or check number attached to it, it uses one less
;to get the last element
(define (get-payment-amount payment)
    (if (equal? (get-payment-type payment) 'Cash)
        (fifth payment)
        (sixth payment)))
;//////////////////////////////////////////////////
;Converts a transaction entry into a string
(define (transaction->string transaction)
    (if (equal? (get-transaction-type transaction) 'Purchase)
        (~a ;formatting. Min-width and aligns help to get the exact output
            (~a #:min-width 8 #:align 'left (get-transaction-timestamp transaction))
            (~a #:min-width 12 #:align 'left (get-transaction-type transaction)) 
            (~a #:min-width 29 #:align 'left (get-purchase-name transaction)) 
            (~a #:align 'right (~r #:precision '(= 2) #:min-width 10 (get-purchase-amount transaction))) "\n")

        (~a 
            (~a #:min-width 8 #:align 'left (get-transaction-timestamp transaction))
            (~a #:min-width 12 #:align 'left (get-transaction-type transaction))
            (~a #:min-width 29 #:align 'left (get-payment-type transaction)) 
            (~a #:align 'right (~r #:precision '(= 2) #:min-width 10 (get-payment-amount transaction))) "\n")))


;Filters out the account transactions that only belong to the account passed in
(define (get-account-transactions account transactions) 
    (filter 
        (λ (transaction) (equal? (get-account-number account) (get-transaction-account-number transaction))) 
        transactions))

;Creates statements for all accounts
(define (create-statements accounts transactions)
    ;helper function to create a single account
    (define (create-statement account)
        (define starting-balance (get-account-balance account))
        ;This is the header of the statement. 
        (define header (~a "STATEMENT OF ACCOUNT\n"
                            (~a #:min-width 20 #:align 'left (get-account-number account))
                            (~a #:min-width 25 #:align 'left (get-account-name account)) 
                            "Starting Balance:\t" (if (= 0 starting-balance)
                                                      (~r #:precision  0 starting-balance)
                                                      (~r #:precision '(= 2) starting-balance))))

        (define star-seperator "*********************************************************")
        ;Helper iterator functions that will call itself to calculate the total payment and purchase total
        ;It will also get the complete string form of all the transactions
        (define (iter account-transactions payment-total purchase-total statement-lines)
            (cond
                ;base condition return results when done
                [(empty? account-transactions) (list payment-total purchase-total statement-lines)]
                [else 
                    (define current-transaction (first account-transactions))
                    ;apply to payment-total or purchase-total depending on tag
                    (if (equal? (get-transaction-type current-transaction) 'Payment)
                        (iter 
                            (rest account-transactions) 
                            (+ (get-payment-amount current-transaction) payment-total)
                            purchase-total
                            (~a statement-lines (transaction->string current-transaction)))
                        (iter 
                            (rest account-transactions) 
                            payment-total
                            (+ (get-purchase-amount current-transaction) purchase-total)
                            (~a statement-lines (transaction->string current-transaction))))]))
        ;sort the results by the account number
        (define iter-result (iter (sort (get-account-transactions account transactions) < #:key get-transaction-timestamp)
                                 0 
                                 0 
                                 ""))

        (define payment-total (first iter-result))
        (define purchase-total (second iter-result))
        (define statement-lines (third iter-result))
        (define ending-balance (- (+ starting-balance purchase-total) payment-total))

        ;apply formatting to output
        (~a 
            header "\n\n" 
            statement-lines "\n\n"
            "Total Purchases:" (~a #:min-width 14 #:align 'right (~r purchase-total #:precision '(= 2))) "\n"
            "Total Payments: " (~a #:min-width 14 #:align 'right (~r payment-total #:precision '(= 2))) "\n"
            "Ending Balance: " (~a #:min-width 14 #:align 'right (~r ending-balance #:precision '(= 2))) "\n\n" 
            star-seperator))
    ;apply this function to all accounts
    (map create-statement accounts))

(define accounts (sort (create-accounts) < #:key get-account-number))
(define transactions (create-transactions))

;output function that writes to a new file. It will replace the statements.txt if it already exists
(call-with-output-file #:exists 'replace "STATEMENTS.txt"
    (λ (out)
        (define statements (create-statements accounts transactions))
        (for-each (λ (statement) (displayln statement out)) statements)))
        



