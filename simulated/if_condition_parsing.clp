; research in how to best tackle parsing the contents of the if statement
(defclass MAIN::statement
  (is-a USER)
  (slot parent
        (type SYMBOL
              INSTANCE)
        (allowed-symbols FALSE))
  (multislot contents
             (default ?NONE)))
(defclass MAIN::if-statement
  (is-a statement))
(defclass MAIN::else-statement
  (is-a statement))

(defrule MAIN::make-base-statement
         ?f <- (parse statement $?contents)
         =>
         (retract ?f)
         (make-instance of statement
                        (parent FALSE)
                        (contents ?contents)))

(defrule MAIN::identify-if-statement
         (declare (salience 2))
         ?o <- (object (is-a statement)
                       (contents $?before if $?rest)
                       (name ?name))
         =>
         (modify-instance ?o
                          (contents $?before 
                                    (make-instance of if-statement
                                                   (parent ?name)
                                                   (contents $?rest)))))

(defrule MAIN::identify-then-statement
         ?o <- (object (is-a statement)
                       (contents $?before then $?after)
                       (parent ?p)
                       (name ?n))
         ?z <- (object (is-a statement)
                       (name ?p)
                       (contents $?a ?n $?b))
         =>
         (if (> (length$ ?after)
                0) then
           (modify-instance ?z
                            (contents $?a ?n $?after $?b))
           (progn$ (?curr ?after)
                   (if (instancep ?curr) then
                     (send ?curr
                           put-parent ?p)))
           (modify-instance ?o
                            (contents $?before then))))

(defrule MAIN::hoist-then-statement-from-else-statement
         ?o <- (object (is-a else-statement)
                       (contents $?contents then)
                       (parent ?p)
                       (name ?n))
         ?k <- (object (is-a if-statement)
                       (name ?p)
                       (contents $?a ?n $?b))
         =>
         (modify-instance ?k
                          (contents $?a ?n then $?b))
         (modify-instance ?o
                          (contents $?contents)))

(defrule MAIN::hoist-everything-after-the-then-in-an-if-statement
         (declare (salience 3))
         ?o <- (object (is-a if-statement)
                       (contents $?a then $?b)
                       (name ?n)
                       (parent ?p))
         (test (> (length$ ?b) 0))
         (object (is-a statement)
                 (name ?p)
                 (contents $?c ?n $?d))
         =>
         (modify-instance ?o
                          (contents $?a then))
         (modify-instance ?p
                          (contents $?c ?n $?b $?d))
         (progn$ (?curr $?b)
                 (if (instancep ?curr) then
                   (send ?curr
                         put-parent ?p))))

(defrule MAIN::identify-else-statement
         (declare (salience 1))
         ?o <- (object (is-a statement)
                       (contents $?before else $?rest)
                       (name ?name))
         =>
         (modify-instance ?o
                          (contents $?before
                                    (make-instance of else-statement
                                                   (parent ?name)
                                                   (contents $?rest)))))

(defrule MAIN::identify-on-true-portion-of-if-with-else
         ?o <- (object (is-a if-statement)
                       (contents $?before ?else then)
                       (name ?if))
         (object (is-a else-statement)
                 (name ?else))
         (not (made on-true for ?if))
         =>
         (assert (made on-true for ?if))
         (modify-instance ?o 
                          (contents (make-instance of statement
                                                   (contents $?before)
                                                   (parent ?if))
                                    ?else
                                    then)))

(defrule MAIN::identify-on-true-portion-of-if-without-else
         ?o <- (object (is-a if-statement)
                       (contents $?before ?else then)
                       (name ?if))
         (object (is-a ~else-statement)
                 (name ?else))
         (not (made on-true for ?if))
         =>
         (assert (made on-true for ?if))
         (send ?else
               put-parent
               (make-instance of statement
                              (contents $?before
                                        ?else)
                              (parent ?if)))
         (modify-instance ?o
                          (contents (send ?else
                                          get-parent)
                                    then)))




(deffacts MAIN::test-parse
          (parse statement : donuts if 0 else 1 then ::)
          (parse statement : donuts2 if 0 if 1 else 2 then else 3 then ::)
          (parse statement : donuts3 if + - * if 128 256 else 320 then then ::)
          )

