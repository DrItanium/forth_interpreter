; A quick implementation of a forth interpreter in CLIPS
; some changes have to be made, the end function comment will use }
(defgeneric MAIN::make-entry)
(defgeneric MAIN::lookup-word)
(defglobal MAIN
           ?*dictionary-pointer* = FALSE
           ?*keep-executing* = TRUE
           ?*ignore-input* = FALSE
           ?*compiling* = FALSE
           ?*current-compilation-target* = FALSE
           ?*error-happened* = FALSE
           ?*error-message* = FALSE
           ?*symbol-end-function* = }
           ?*symbol-begin-function* = {
           ?*comment-symbol-begin* = '
           ?*comment-symbol-end* = '
           ; bootstrapping default contents
           ?*current-input* = (create$ { 0 ' -- 0 ' 0 }
                                       { 1 ' -- 1 ' 1 }
                                       { 2 ' -- 2 ' 2 }
                                       { true TRUE }
                                       { false FALSE }
                                       { 1+ 1 + } 
                                       { 1- 1 - }
                                       { 2+ 2 + }
                                       { 2- 2 - }
                                       { 2* 2 * }
                                       { 2/ 2 / }
                                       { 2div 2 div }
                                       { eqz ' n -- flag ' 0 = }
                                       { ltz 0 swap < }
                                       { gtz 0 swap > }
                                       { difference - abs }
                                       { on true swap store }
                                       { off false swap store }
                                       { over swap ' n1 n2 -- n1 n2 n1 ' dup 0 store swap 0 load }
                                       { dup? ' a -- a a "|" 0 ' dup if dup then }
                                       { nip ' a b -- b ' swap drop }
                                       { tuck ' a b -- b a b ' swap over }
                                       { 2dup ' a b -- a b a b ' over over }
                                       { 2drop ' a b -- ' drop drop }
                                       { view dup . }
                                       { space " " . }
                                       { bye quit }
                                       { -rot ' n1 n2 n3 -- n3 n1 n2 ' rot rot }
                                       )
           ?*current-length* = (length$ ?*current-input*)
           ?*current-index* = 1
           ?*memory-cell-count* = 4096
           ?*memory* = (create$)
           ?*no-arg-ops* = (create$ random 
                                    pi 
                                    time
                                    operating-system)
           ?*binary-ops* = (create$ + - * / mod ** div str-cat str-index max min
                                    eq neq = <> > < >= <= and or)
           ?*unary-ops* = (create$ abs integer float upcase lowcase str-length
                                   grad-deg geg-grad deg-rad rad-deg sqrt exp log
                                   log10 round seed length not
                                   numberp floatp integerp lexemep stringp symbolp evenp
                                   oddp multifieldp pointerp
                                   cos sin tan sec csc cot atan asin asec acsc acot acos
                                   cosh sinh tanh sech csch coth atanh asinh asech acsch acoth acosh)
           )

(deffunction MAIN::raise-error
             (?message)
             (bind ?*error-happened*
                   TRUE)
             (bind ?*error-message*
                   ?message))
(defclass stack
  (is-a USER)
  (multislot contents)
  (message-handler rotate-top-three primary)
  (message-handler push primary)
  (message-handler pop primary)
  (message-handler duplicate-top primary)
  (message-handler swap-top-two primary))
(defmessage-handler stack rotate-top-three primary
                    ()
                    (if (>= (length$ ?self:contents) 3) then
                      ; ( n1 n2 n3 -- n2 n3 n1 )
                      (bind ?n3
                            (nth$ 1
                                  ?self:contents))
                      (bind ?n2
                            (nth$ 2
                                  ?self:contents))
                      (bind ?n1
                            (nth$ 3
                                  ?self:contents))
                      (slot-direct-replace$ contents
                                            1 3
                                            ?n1
                                            ?n3
                                            ?n2)
                      else
                      (raise-error "Stack Underflow!")
                      FALSE))

(defmessage-handler stack duplicate-top primary
                    ()
                    (if (= (length$ ?self:contents) 0) then
                      (raise-error "Stack Empty!")
                      FALSE
                      else
                      (slot-direct-insert$ contents
                                           1
                                           (first$ ?self:contents))))
(defmessage-handler stack swap-top-two primary
                    ()
                    (if (>= (length$ ?self:contents) 2) then
                      (bind ?top
                            (nth$ 1 ?self:contents))
                      (bind ?next
                            (nth$ 2 ?self:contents))
                      (slot-direct-replace$ contents
                                            1 2
                                            ?next ?top)
                      else
                      (raise-error "STACK UNDERFLOW!")
                      FALSE))

(defmessage-handler stack push primary
                    (?value)
                    (slot-direct-insert$ contents
                                         1
                                         ?value))
(defmessage-handler stack pop primary
                    ()
                    (if (= (length$ ?self:contents) 0) then
                      (raise-error "STACK EMPTY!!")
                      FALSE
                      else
                      (bind ?front
                            (nth$ 1 ?self:contents))
                      (slot-direct-delete$ contents 1 1)
                      ?front))

(defclass constant
  (is-a USER)
  (slot value
        (default ?NONE))
  (message-handler invoke primary))

(defmessage-handler constant invoke primary
                    ()
                    (send [parameter] 
                          push
                          ?self:value))

(defclass MAIN::operation
  (is-a USER)
  (slot operation
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE)))

(defclass MAIN::wrapped-unary-operation
  (is-a operation)
  (message-handler invoke primary))

(defmessage-handler wrapped-unary-operation invoke primary
                    ()
                    (send [parameter]
                          push
                          (funcall (dynamic-get operation)
                                   (send [parameter] pop))))

(defclass MAIN::wrapped-binary-operation
  (is-a operation)
  (message-handler invoke primary))

(defmessage-handler wrapped-binary-operation invoke primary
                    ()
                    (send [parameter]
                          push
                          (funcall (dynamic-get operation) 
                                   (send [parameter] pop)
                                   (send [parameter] pop))))
(defclass MAIN::generic-operation
  (is-a operation)
  (message-handler invoke primary))

(defmessage-handler generic-operation invoke primary
                    ()
                    (funcall (dynamic-get operation)))

(defclass MAIN::zero-arg-operation
  (is-a operation)
  (message-handler invoke primary))

(defmessage-handler zero-arg-operation invoke primary
                    ()
                    (send [parameter]
                          push
                          (funcall (dynamic-get operation))))

(deffunction get-stack-depth
             ()
             (length$ (send [parameter] 
                            get-contents)))
(deffunction MAIN::make-operation
             (?symbol ?type)
             (make-instance of ?type
                            (operation ?symbol)))
(deffunction MAIN::binary-operation
             (?symbol)
             (make-operation ?symbol 
                             wrapped-binary-operation))
(deffunction MAIN::unary-operation
             (?symbol)
             (make-operation ?symbol
                             wrapped-unary-operation))

(deffunction MAIN::no-arg-operation
             (?symbol)
             (make-operation ?symbol
                             zero-arg-operation))

(deffunction MAIN::invoke-operation
             (?symbol)
             (make-operation ?symbol
                             generic-operation))

(defclass dictionary-entry
  (is-a USER)
  (slot title
        (type SYMBOL)
        (default ?NONE))
  (slot next
        (type INSTANCE
              SYMBOL)
        (allowed-symbols FALSE)
        (visibility public))
  (slot fake
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE))
  (slot compile-time-invoke
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE))
  (slot compiled
        (type SYMBOL)
        (allowed-symbols FALSE
                         TRUE))
  (multislot contents)
  (message-handler add-component primary)
  (message-handler install primary)
  (message-handler invoke primary))

(defmessage-handler dictionary-entry invoke primary
                    ()
                    (if ?self:compiled then
                      (progn$ (?c ?self:contents)
                              (send ?c invoke))))
(defmessage-handler dictionary-entry add-component primary
                    (?component)
                    (if (not ?self:compiled) then
                      (bind ?self:contents
                            ?self:contents
                            (make-entry ?component))))
(defmessage-handler dictionary-entry install primary
                    ()
                    (if (not ?self:compiled) then
                      (bind ?self:compiled
                            TRUE)
                      (bind ?self:next
                            ?*dictionary-pointer*)
                      (bind ?*dictionary-pointer*
                            (instance-name ?self))))

(deftemplate MAIN::order
             (slot current
                   (type SYMBOL)
                   (default ?NONE))
             (multislot rest
                        (type SYMBOL)))
(defrule MAIN::next-stage
         (declare (salience -10000))
         ?f <- (order (rest ?next $?rest))
         =>
         (modify ?f
                 (current ?next)
                 (rest ?rest)))

(defrule MAIN::done-with-flow
         (declare (salience -10000))
         ?f <- (order (rest))
         =>
         (retract ?f))

(defmethod lookup-word
  ((?name SYMBOL)
   (?target SYMBOL))
  FALSE)

(defmethod lookup-word
  ((?name SYMBOL)
   (?target dictionary-entry))
  (if (send ?target
            get-fake) then
    (lookup-word ?name
                 (send ?target
                       get-next))
    else
    (if (eq ?name 
            (send ?target
                  get-title)) then 
      ?target 
      else
      (lookup-word ?name
                   (send ?target
                         get-next)))))
(defmethod lookup-word
  (?value)
  FALSE)

(defmethod lookup-word
  ((?name SYMBOL))
  (lookup-word ?name
               ?*dictionary-pointer*))
(defmethod make-entry
  ((?word SYMBOL))
  (lookup-word ?word))
(defmethod make-entry
  ((?value NUMBER
           STRING))
  (make-instance of constant
                 (value ?value)))

(defmethod make-entry
  ((?inst INSTANCE))
  ?inst)

(deffunction MAIN::drop-top () (send [parameter] pop)) 
(deffunction MAIN::swap-top-two () (send [parameter] swap-top-two))
(deffunction MAIN::duplicate-top () (send [parameter] duplicate-top))
(deffunction MAIN::print-top () (printout t (send [parameter] pop)))
(deffunction MAIN::print-newline () (printout t crlf)) 
(deffunction MAIN::rot () (send [parameter] rotate-top-three))

(deffunction MAIN::add-word
             (?name ?fake ?compile-time-invoke $?contents)
             (bind ?q 
                   (make-instance of dictionary-entry
                                  (title ?name)
                                  (fake ?fake)
                                  (compile-time-invoke ?compile-time-invoke)))
             (progn$ (?c ?contents)
                     (send ?q
                           add-component ?c))
             (send ?q 
                   install))

(deffunction MAIN::add-clips-binary-word
             (?symbol)
             (add-word ?symbol FALSE FALSE (binary-operation ?symbol)))

(deffunction MAIN::add-clips-unary-word
             (?symbol)
             (add-word ?symbol FALSE FALSE (unary-operation ?symbol)))

(deffunction MAIN::add-clips-no-arg-word
             (?symbol)
             (add-word ?symbol FALSE FALSE (no-arg-operation ?symbol)))

(deffacts MAIN::initial-dictionary
          (words clips-no-arg-word 
                 ?*no-arg-ops*)
          (words clips-binary-word
                 ?*binary-ops*)
          (words clips-unary-word
                 ?*unary-ops*)
          (word random:range FALSE FALSE binary-operation random)
          (word drop FALSE FALSE invoke-operation drop-top)
          (word swap FALSE FALSE invoke-operation swap-top-two)
          (word dup FALSE FALSE invoke-operation duplicate-top)
          (word .  FALSE FALSE invoke-operation print-top)
          (word quit FALSE FALSE invoke-operation terminate-execution)
          (word ?*symbol-end-function* FALSE TRUE invoke-operation compile-or-end-function)
          (word ?*symbol-begin-function* FALSE FALSE invoke-operation new-compile-target)
          (word ?*comment-symbol-begin* FALSE TRUE invoke-operation handle-input-ignore-mode)
          (word CR FALSE FALSE invoke-operation print-newline)
          (word @ FALSE FALSE invoke-operation load-word-onto-stack)
          (word if FALSE TRUE invoke-operation if-condition)
          (word then FALSE TRUE invoke-operation then-condition)
          (word else FALSE TRUE invoke-operation else-condition)
          (word store FALSE FALSE binary-operation mem-store drop)
          (word load FALSE FALSE unary-operation mem-load)
          (word words FALSE FALSE invoke-operation print-words)
          (word stack FALSE FALSE invoke-operation stack-contents)
          (word literal FALSE TRUE invoke-operation add-literal-from-stack-into-definition)
          (word depth FALSE FALSE no-arg-operation get-stack-depth)
          (word rot FALSE FALSE invoke-operation rot)
          )

(defrule MAIN::construct-call-operations
         (declare (salience 10))
         (order (current setup))
         ?f <- (word ?title
                     ?fake
                     ?compile-time-invoke
                     $?a 
                     ?fn&:(not (neq ?fn
                                    binary-operation
                                    unary-operation
                                    invoke-operation
                                    no-arg-operation)) ?op $?b)
         =>
         (retract ?f)
         (assert (word ?title
                       ?fake
                       ?compile-time-invoke
                       $?a (funcall ?fn
                                    ?op) $?b)))

(defrule MAIN::add-word-to-initial-dictionary
         (order (current setup))
         ?f <- (word ?title 
                     ?fake 
                     ?compile-time-invoke 
                     $?operations)
         =>
         (retract ?f)
         (add-word ?title
                   ?fake
                   ?compile-time-invoke
                   ?operations))

(defrule MAIN::slice-off-of-massive-list
         (order (current setup))
         ?f <- (words ?op
                      ?title $?rest)
         =>
         (retract ?f)
         (assert (word ?op ?title)
                 (words ?op $?rest)))

(defrule MAIN::add-clips-short-form-word
         (order (current setup))
         ?f <- (word ?op&:(not (neq ?op
                                    clips-no-arg-word
                                    clips-binary-word
                                    clips-unary-word))
                     ?title)
         =>
         (retract ?f)
         (funcall (sym-cat add- ?op)
                  ?title))



(deffunction MAIN::add-literal-from-stack-into-definition
             ()
             (if (not ?*compiling*) then 
               (raise-error "literal can only be used during compilation!")
               (return))
             (send ?*current-compilation-target*
                   add-component
                   (send [parameter]
                         pop)))

(deffunction MAIN::print-title
             (?instance)
             (if ?instance then
               ; start with the oldest first and then go to newest
               (print-title (send ?instance get-next))
               (if (not (send ?instance get-fake)) then
                 (printout t "- " (send ?instance get-title) crlf))))

(deffunction MAIN::print-words
             ()
             (print-title ?*dictionary-pointer*))
(deffunction MAIN::mem-store
             (?address ?value)
             (bind ?*memory*
                   (replace$ ?*memory* 
                             (+ ?address 
                                1)
                             ?value
                             ?value))
             TRUE)
(deffunction MAIN::mem-load
             (?address)
             (nth$ (+ ?address
                      1)
                   ?*memory*))

(deffunction MAIN::stack-contents
             ()
             (progn$ (?a (send [parameter] get-contents))
                     (printout t "- " ?a crlf)))


(deffunction MAIN::handle-input-ignore-mode
             ()
             (bind ?*ignore-input*
                   (not ?*ignore-input*)))
(deffunction MAIN::terminate-execution
             ()
             (bind ?*keep-executing*
                   FALSE))
(deffunction MAIN::next-word
             ()
             (if (>= ?*current-index*
                     ?*current-length*) then
               (bind ?*current-input*
                     (explode$ (readline)))
               (bind ?*current-index* 1)
               (bind ?*current-length* 
                     (length$ ?*current-input*))
               else
               (bind ?*current-index*
                     (+ ?*current-index*
                        1)))
             (nth$ ?*current-index*
                   ?*current-input*))
(deffunction MAIN::new-compile-target
             ()
             ; goto the next input set
             (bind ?name 
                   (next-word))
             (send [subroutine] 
                   push
                   ?*current-compilation-target*)
             (bind ?*current-compilation-target*
                   (make-instance of dictionary-entry
                                  (title ?name)
                                  (fake FALSE)
                                  (compile-time-invoke FALSE)
                                  (contents)))
             (bind ?*compiling*
                   TRUE))
(deffunction MAIN::load-word-onto-stack
             ()
             (bind ?name
                   (next-word))
             (bind ?sym
                   (lookup-word ?name))
             (if ?sym then
               (send [parameter]
                     push
                     ?sym)
               else
               (raise-error (sym-cat ?name "?"))))
(defclass MAIN::if-statement
  (is-a USER)
  (slot on-true
        (type INSTANCE))
  (slot on-false
        (type INSTANCE))
  (message-handler invoke primary))
(defmessage-handler if-statement invoke primary
                    ()
                    (bind ?top
                          (send [parameter] pop))
                    (send (if (or (not ?top)
                                  (eq ?top
                                      0)) then
                            ?self:on-false
                            else
                            ?self:on-true) invoke))
(deffunction MAIN::if-condition
             ()
             (if (not ?*compiling*) then
               (raise-error "If outside of compilation mode!")
               (return FALSE))
             (send [subroutine]
                   push
                   ?*current-compilation-target*)
             (bind ?i 
                   (make-instance of if-statement
                                  (on-true (make-instance of dictionary-entry
                                                          (title "")
                                                          (fake TRUE)
                                                          (compile-time-invoke FALSE)
                                                          (contents)))
                                  (on-false (make-instance of dictionary-entry
                                                           (title "")
                                                           (fake TRUE)
                                                           (compile-time-invoke FALSE)
                                                           (contents)))))
             (send [subroutine]
                   push
                   ?i)
             (bind ?*current-compilation-target*
                   (send ?i
                         get-on-true)))
(deffunction MAIN::else-condition
             ()
             (if (not ?*compiling*) then
               (raise-error "Else outside of compilation mode!")
               (return FALSE))
             (bind ?i
                   (send [subroutine]
                         pop))
             (send ?*current-compilation-target*
                   install)
             (bind ?*current-compilation-target*
                   (send ?i
                         get-on-false))
             (send [subroutine]
                   push
                   ?i))
(deffunction MAIN::then-condition
             ()
             (if (not ?*compiling*) then
               (raise-error "then outside of compilation mode!")
               (return FALSE))
             (send ?*current-compilation-target*
                   install)
             (bind ?i
                   (send [subroutine]
                         pop))
             (if (not (send (send ?i 
                                  get-on-false) 
                            get-compiled)) then
               (send (send ?i 
                           get-on-false) 
                     install))
             (bind ?*current-compilation-target*
                   (send [subroutine]
                         pop))
             (send ?*current-compilation-target*
                   add-component
                   ?i))



(deffunction MAIN::compile-or-end-function
             ()
             (if ?*compiling* then
               (send ?*current-compilation-target*
                     add-component
                     ?*symbol-end-function*)
               (send ?*current-compilation-target*
                     install)
               (bind ?*current-compilation-target*
                     (send [subroutine] pop))
               (bind ?*compiling*
                     (instancep ?*current-compilation-target*))))
(deffunction MAIN::handle-compilation
             (?word ?entry)
             (if ?entry then
               (if (send ?entry
                         get-compile-time-invoke) then
                 (send ?entry
                       invoke)
                 else
                 (send ?*current-compilation-target*
                       add-component
                       ?entry))
               else
               (send ?*current-compilation-target*
                     add-component
                     ?word)))
(deffunction MAIN::push-to-stack
             (?value)
             (or (numberp ?value)
                 (stringp ?value)
                 (eq ?value
                     TRUE)
                 (eq ?value
                     FALSE)))
(deffunction MAIN::invoke-or-push
             (?word ?entry)
             (if ?entry then
               (send ?entry
                     invoke)
               else
               (if (push-to-stack ?word) then
                 (send [parameter]
                       push
                       ?word)
                 else
                 (raise-error (str-cat ?word "?")))))






(deffunction MAIN::continue
             ()
             (bind ?*keep-executing* 
                   TRUE)
             (assert (order (current control-loop)))
             (run))

(defrule MAIN::setup-memory
         (order (current setup))
         =>
         (loop-for-count (?i 1 ?*memory-cell-count*) do
                         (bind ?*memory*
                               ?*memory*
                               0)))

(defrule MAIN::stop-executing
         (declare (salience 10000))
         ?f <- (order (current determination))
         (test (not ?*keep-executing*))
         =>
         (retract ?f))

(defrule MAIN::continue-execution
         ?f <- (order (current determination))
         (test (eq ?*keep-executing*
                   TRUE))
         =>
         (modify ?f
                 (current get-input)
                 (rest control-loop
                       handle-output
                       determination)))

(defrule MAIN::setup-input
         (order (current get-input))
         (test (or (= ?*current-length* 0)
                   (>= ?*current-index*
                       ?*current-length*)))
         =>
         (bind ?*current-input* 
               (explode$ (readline)))
         (bind ?*current-index*
               1)
         (bind ?*current-length*
               (length$ ?*current-input*)))

(defrule MAIN::check-output
         (order (current handle-output))
         =>
         (if ?*error-happened* then
           (send [parameter] put-contents (create$))
           (send [subroutine] put-contents (create$))
           (printout werror ?*error-message* crlf)
           (bind ?*error-happened* FALSE)
           (bind ?*error-message* FALSE)
           else
           (if (and (not ?*ignore-input*)
                    (not ?*compiling*)) then
             (printout t " ok" crlf))))
(defrule MAIN::invoke-control-loop
         (order (current control-loop))
         =>
         (while (and (> ?*current-length* 0)
                     (<= ?*current-index*
                         ?*current-length*)) do
                (bind ?word
                      (nth$ ?*current-index*
                            ?*current-input*))
                (if ?*ignore-input* then
                  (if (eq ?word 
                          ?*comment-symbol-end*) then
                    (bind ?*ignore-input* FALSE))
                  else
                  (bind ?entry (lookup-word ?word))
                  (if ?*compiling* then
                    (handle-compilation ?word
                                        ?entry)
                    else
                    (invoke-or-push ?word
                                    ?entry)))
                (bind ?*current-index*
                      (+ ?*current-index* 1))))

(deffacts MAIN::control
          (order (current setup)
                 (rest determination)))
(definstances MAIN::stacks
              (parameter of stack)
              (subroutine of stack))
