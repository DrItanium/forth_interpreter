; A quick implementation of a forth interpreter in CLIPS
; some changes have to be made, the end function comment will use }
;
; must use my maya spin for this to work correctly
(defgeneric MAIN::make-entry)
(defgeneric MAIN::lookup-word)
(defgeneric MAIN::read-symbol)
(defglobal MAIN
           ?*print-ok* = FALSE
           ?*dictionary-pointer* = FALSE
           ?*keep-executing* = TRUE
           ?*ignore-input* = FALSE
           ?*compiling* = FALSE
           ?*current-compilation-target* = FALSE
           ?*error-happened* = FALSE
           ?*error-message* = FALSE
           ?*symbol-end-function* = (sym-cat ";")
           ?*symbol-begin-function* = :
           ?*comment-symbol-begin* = (sym-cat "(")
           ?*comment-symbol-end* = (sym-cat ")")
           ?*string-begin-symbol* = (sym-cat "\"")
           ?*square-bracket* = (sym-cat "[")
           ; bootstrapping default contents
           ?*memory-cell-count* = 4096
           ?*memory* = (create$)
           ?*no-arg-ops* = (create$ random 
                                    pi 
                                    time
                                    operating-system
                                    gensym
                                    gensym*
                                    new-uuid)
           ?*binary-ops* = (create$ + - * / mod ** div str-cat str-index max min
                                    eq neq = <> > < >= <= and or rename
                                    gcd lcm
                                    has-prefix
                                    has-suffix)
           ?*unary-ops* = (create$ abs integer float upcase lowcase str-length
                                   grad-deg geg-grad deg-rad rad-deg sqrt exp log
                                   log10 round seed length not remove
                                   numberp floatp integerp lexemep stringp symbolp evenp
                                   oddp multifieldp pointerp
                                   cos sin tan sec csc cot atan asin asec acsc acot acos
                                   cosh sinh tanh sech csch coth atanh asinh asech acsch acoth acosh
                                   string-trim string-trim-front string-trim-back
                                   path-exists directoryp regular-filep)
           ?*current-output-router* = stdout
           ?*current-input-router* = stdin)

(deffunction MAIN::save-current-compilation-target
             ()
             (send [subroutine]
                   push
                   ?*current-compilation-target*))
(deffunction MAIN::restore-current-compilation-target
             ()
             (bind ?*current-compilation-target*
                   (send [subroutine]
                         pop)))
(deffunction MAIN::raise-error
             (?message)
             (bind ?*error-happened*
                   TRUE)
             (bind ?*error-message*
                   ?message))

(deffunction MAIN::is-white-space
             (?value)
             (if (eq ?value 10) then
               (bind ?*print-ok*
                     TRUE))
             (not (neq ?value 32 10)))
(defclass global-constant
  (is-a USER)
  (slot var
        (type LEXEME)
        (default ?NONE))
  (message-handler invoke primary))

(defmessage-handler global-constant invoke primary
                    ()
                    (send [parameter]
                          push
                          (eval ?self:var)))

(defclass stack
  (is-a USER)
  (multislot contents)
  (message-handler clear primary)
  (message-handler rotate-top-three primary)
  (message-handler push primary)
  (message-handler pop primary)
  (message-handler duplicate-top primary)
  (message-handler swap-top-two primary))

(defmessage-handler stack clear primary
                    ()
                    (bind ?self:contents
                          (create$)))

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
(deffunction get-stack-depth () (length$ (send [parameter] get-contents)))

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
                    (bind ?top
                          (send [parameter] pop))
                    (bind ?lower
                          (send [parameter] pop))
                    (send [parameter]
                          push
                          (funcall (dynamic-get operation)
                                   ?lower 
                                   ?top)))
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

(defclass MAIN::if-statement
  (is-a USER)
  (slot on-true
        (type INSTANCE))
  (slot on-false
        (type INSTANCE))
  (message-handler invoke primary))
(defmessage-handler if-statement invoke primary
                    ()
                    (send (if (or (not (bind ?top
                                             (send [parameter]
                                                   pop)))
                                  (eq ?top
                                      0)) then
                            ?self:on-false
                            else
                            ?self:on-true) 
                          invoke))

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

(defclass MAIN::stack-message
  (is-a USER)
  (slot target
        (type INSTANCE)
        (default ?NONE))
  (slot message
        (type SYMBOL)
        (default ?NONE))
  (message-handler invoke primary))
(defmessage-handler stack-message invoke primary
                    ()
                    (send ?self:target
                          ?self:message))
(deffunction MAIN::parameter-stack-message 
             (?message) 
             (make-instance of stack-message
                            (target [parameter])
                            (message ?message)))
(deffunction MAIN::subroutine-stack-message 
             (?message) 
             (make-instance of stack-message
                            (target [subroutine])
                            (message ?message)))
(deffunction MAIN::print-top (?router ?value) (printout ?router ?value) FALSE)

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

(deffunction MAIN::begin-statement
             ()
             (if (not ?*compiling*) then
               (raise-error "Not Compiling!")
               (return))
             (save-current-compilation-target)
             (bind ?*current-compilation-target*
                   (make-instance of dictionary-entry
                                  (fake TRUE)
                                  (title "")
                                  (compile-time-invoke FALSE)
                                  (contents))))

(deffunction MAIN::end-statement
             ()
             (if (not ?*compiling*) then
               (raise-error "Not Compiling!")
               (return))
             (send ?*current-compilation-target* install)
             (bind ?i
                   (make-instance of dictionary-entry
                                  (fake TRUE)
                                  (title "")
                                  (compile-time-invoke FALSE)
                                  (contents ?*current-compilation-target*)))
             (send ?i
                   add-component
                   (make-instance of if-statement
                                  (on-true (make-instance of dictionary-entry
                                                          (fake TRUE)
                                                          (title "")
                                                          (compile-time-invoke FALSE)
                                                          (compiled TRUE)
                                                          (contents)))
                                  (on-false ?i))) ; recursively call self for now, can cause call chain problems later on though!
             (restore-current-compilation-target)
             (send ?i install)
             (send ?*current-compilation-target*
                   add-component
                   ?i))

(defrule MAIN::construct-call-operations
         (declare (salience 10))
         (order (current setup))
         ?f <- (word ?title
                     ?fake
                     ?compile-time-invoke
                     $?a 
                     ?fn&:(not (neq ?fn
                                    parameter-stack-message
                                    subroutine-stack-message
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
             (bind ?pos
                   (+ ?address 
                      1))
             (bind ?*memory*
                   (replace$ ?*memory* 
                             ?pos
                             ?pos
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
             (read-symbol ?*current-input-router*))
(deffunction MAIN::new-compile-target
             ()
             ; goto the next input set
             (bind ?name 
                   (next-word))
             (save-current-compilation-target)
             (bind ?*current-compilation-target*
                   (make-instance of dictionary-entry
                                  (title ?name)
                                  (fake FALSE)
                                  (compile-time-invoke FALSE)
                                  (contents)))
             (bind ?*compiling*
                   TRUE))

(deffunction MAIN::if-condition
             ()
             (if (not ?*compiling*) then
               (raise-error "If outside of compilation mode!")
               (return FALSE))
             (save-current-compilation-target)
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
             (restore-current-compilation-target)
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
               (restore-current-compilation-target)
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
             (assert (order (current determination)))
             (run))

(defrule MAIN::setup-memory
         (declare (salience 10000))
         (order (current setup))
         =>
         (loop-for-count (?i 1 ?*memory-cell-count*)
                         do
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
                 (current control-loop)
                 (rest handle-output
                       determination)))

(defrule MAIN::check-output
         (order (current handle-output))
         =>
         (if ?*error-happened* then
           (send [parameter] 
                 clear)
           (send [subroutine] 
                 clear)
           (printout werror 
                     ?*error-message* 
                     crlf)
           (bind ?*error-happened* 
                 FALSE)
           (bind ?*error-message* 
                 FALSE)
           else
           (if ?*print-ok* then
             (if (and (not ?*ignore-input*)
                      (not ?*compiling*)) then
               (printout ?*current-output-router* 
                         " ok" crlf))
             (bind ?*print-ok* 
                   FALSE))))

(defrule MAIN::invoke-control-loop
         (order (current control-loop))
         =>
         (bind ?word
               (next-word))
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
                             ?entry))))

(deffacts MAIN::control
          (order (current setup)
                 (rest determination)))
(definstances MAIN::stacks
              (parameter of stack)
              (subroutine of stack))
(deffunction construct-string
             ()
             ; keep consuming words until we end with a string 
             (bind ?string
                   (create$))
             (while TRUE do
                    (bind ?curr
                          (next-word))
                    (if (not ?curr) then
                      (break))
                    (if (has-suffix (str-cat ?curr)
                                    "\"") then
                      (bind ?string
                            ?string
                            (sym-cat (sub-string 1 (- (str-length ?curr) 1)
                                        ?curr)))
                      (break)
                      else
                      (bind ?string
                            ?string
                            ?curr)))
             (implode$ ?string))

(deffacts MAIN::initial-dictionary
          (words clips-no-arg-word
                 ?*no-arg-ops*)
          (words clips-binary-word
                 ?*binary-ops*)
          (words clips-unary-word
                 ?*unary-ops*)
          (word *current-output-router* FALSE FALSE [current-output-router])
          (word *current-input-router* FALSE FALSE [current-input-router])
          (word random:range FALSE FALSE binary-operation random)
          (word drop FALSE FALSE parameter-stack-message pop)
          (word swap FALSE FALSE parameter-stack-message swap-top-two)
          (word dup FALSE FALSE parameter-stack-message duplicate-top)
          (word rot FALSE FALSE parameter-stack-message rotate-top-three)
          (word . FALSE FALSE [current-output-router] swap binary-operation print-top drop)
          (word quit FALSE FALSE invoke-operation terminate-execution)
          (word ?*symbol-end-function* FALSE TRUE invoke-operation compile-or-end-function)
          (word ?*symbol-begin-function* FALSE FALSE invoke-operation new-compile-target)
          (word ?*comment-symbol-begin* FALSE TRUE invoke-operation handle-input-ignore-mode)
          (word if FALSE TRUE invoke-operation if-condition)
          (word then FALSE TRUE invoke-operation then-condition)
          (word else FALSE TRUE invoke-operation else-condition)
          (word mload FALSE FALSE unary-operation mem-load)
          (word mstore FALSE FALSE binary-operation mem-store drop)
          (word words FALSE FALSE invoke-operation print-words)
          (word stack FALSE FALSE invoke-operation stack-contents)
          (word literal FALSE TRUE invoke-operation add-literal-from-stack-into-definition)
          (word depth FALSE FALSE no-arg-operation get-stack-depth)
          (word begin FALSE TRUE invoke-operation begin-statement)
          (word end FALSE TRUE invoke-operation end-statement)
          (word ?*string-begin-symbol* FALSE TRUE no-arg-operation construct-string)
          (word emit FALSE FALSE [current-output-router] swap binary-operation emit-operation drop)
          (word read-char FALSE FALSE [current-input-router] unary-operation get-char)
          (word load FALSE FALSE ; ( path -- )
                unary-operation load-routine)
          (word close FALSE FALSE
                [current-input-router] unary-operation close-op)
          (word readline FALSE FALSE
                [current-input-router] unary-operation readline)
          )
            
(deffunction MAIN::emit-operation
             (?router ?value)
             (put-char ?router
                       ?value)
             FALSE)
(deffunction MAIN::close-op
             (?operation)
             (if (eq ?operation 
                     stdout) then
                FALSE
                else
                (bind ?*current-input-router*
                      (send [subroutine]
                            pop))
                (close ?operation)))
(definstances MAIN::router-constants
              (current-output-router of global-constant 
                                     (var "?*current-output-router*"))
              (current-input-router of global-constant 
                                    (var "?*current-input-router*")))
(deffunction MAIN::load-routine
             (?path)
             (send [subroutine] 
                   push
                   ?*current-input-router*)
             (bind ?*current-input-router*
                   (gensym*))
             (open ?path
                   ?*current-input-router*))
; make this always last as the formatter goes nuts otherwise
(defmethod MAIN::read-symbol
  (?router)
  (bind ?contents
        (create$))
  ; strip away white space
  (while (is-white-space (bind ?char
                               (get-char ?router))) do
         (if (eq ?char -1) then
           (return FALSE)))
  ; now that we are not at white space we have to use this data
  (while TRUE do
         (if (eq ?char -1) then
           (return FALSE))
         (bind ?contents
               ?contents
               (format nil
                       "%c"
                       ?char))
         (if (is-white-space (bind ?char
                                   (get-char ?router))) then
           (break)))
  (if (= (length$ ?contents) 0) then
    (raise-error "REACHED END OF INPUT!!")
    (return FALSE))
  (bind ?first
        (nth$ 1
              ?contents))
  (if (and (= (length$ ?contents) 1)
           (not (neq ?first
                     "(" ")" ";" "\""))) then
    (sym-cat ?first)
    else
    (if (or (eq ?first
                ?*square-bracket*)
            (eq (nth$ (length$ ?contents)
                      ?contents)
                "\"")) then
      (sym-cat (str-cat (expand$ ?contents)))
      else
      (string-to-field (str-cat (expand$ ?contents))))))
