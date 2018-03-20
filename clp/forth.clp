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
           ?*has-setup-initial-dictionary* = FALSE
           ?*error-happened* = FALSE
           ?*error-message* = FALSE
           ?*symbol-end-function* = }
           ?*symbol-begin-function* = {
           ?*current-input* = FALSE
           ?*current-index* = 0
           ?*current-length* = 0)
(deffunction MAIN::raise-error
             (?message)
             (bind ?*error-happened*
                   TRUE)
             (bind ?*error-message*
                   ?message))
(defclass stack
  (is-a USER)
  (multislot contents)
  (message-handler empty primary)
  (message-handler push primary)
  (message-handler pop primary))

(defmessage-handler stack empty primary
                    ()
                    (= (length$ ?self:contents) 0))
(defmessage-handler stack push primary
                    (?value)
                    (bind ?self:contents
                          ?value
                          ?self:contents))
(defmessage-handler stack pop primary
                    ()
                    (if (send ?self 
                              empty) then
                      (raise-error "STACK EMPTY!!")
                      FALSE
                      else
                      (bind ?front
                            (first$ ?self:contents))
                      (bind ?self:contents
                            (rest$ ?self:contents))
                      (nth$ 1 ?front)))

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
(deffunction MAIN::binary-operation
             (?symbol)
             (make-instance of wrapped-binary-operation
                            (operation ?symbol)))
(deffunction MAIN::unary-operation
             (?symbol)
             (make-instance of wrapped-unary-operation
                            (operation ?symbol)))
(deffunction MAIN::invoke-operation
             (?symbol)
             (make-instance of generic-operation
                            (operation ?symbol)))

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
  (message-handler has-next primary)
  (message-handler invoke primary))

(defmessage-handler dictionary-entry has-next primary
                    ()
                    ?self:next)

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
(definstances MAIN::stacks
              (parameter of stack)
              (subroutine of stack))
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
                  get-title)) then ?target else
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

(deffunction MAIN::drop-top 
             () 
             (send [parameter] pop)) 
(deffunction MAIN::swap-top-two
             () 
             (bind ?a 
                   (send [parameter] pop))
             (bind ?b
                   (send [parameter] pop))
             (send [parameter]
                   push ?b)
             (send [parameter]
                   push ?a))
(deffunction MAIN::duplicate-top
             ()
             (bind ?a
                   (send [parameter] pop))
             (send [parameter] push ?a)
             (send [parameter] push ?a))
(deffunction MAIN::print-top
             ()
             (printout t 
                       (send [parameter]
                             pop)))
(deffunction MAIN::print-newline
             ()
             (printout t
                       crlf))
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
(deffunction MAIN::setup-dictionary
             ()
             (if (not ?*has-setup-initial-dictionary*) then
               (add-word drop
                         FALSE
                         FALSE
                         (invoke-operation drop-top))
               (add-word swap
                         FALSE
                         FALSE
                         (invoke-operation swap))
               (add-word dup
                         FALSE
                         FALSE
                         (invoke-operation duplicate-top))
               (add-word 2dup
                         FALSE
                         FALSE
                         dup
                         dup)
               (add-word 2drop
                         FALSE
                         FALSE
                         drop drop)
               (add-word +
                         FALSE
                         FALSE
                         (binary-operation +))
               (add-word -
                         FALSE
                         FALSE
                         (binary-operation -))
               (add-word *
                         FALSE
                         FALSE
                         (binary-operation *))
               (add-word /
                         FALSE
                         FALSE
                         (binary-operation /))
               (add-word %
                         FALSE
                         FALSE
                         (binary-operation mod))
               (add-word pow
                         FALSE
                         FALSE
                         (binary-operation **))
               (add-word /u
                         FALSE
                         FALSE
                         (binary-operation div))
               (add-word .
                         FALSE
                         FALSE
                         (invoke-operation print-top))
               (add-word quit
                         FALSE
                         FALSE
                         (invoke-operation terminate-execution))
               (add-word ?*symbol-end-function*
                         FALSE
                         TRUE
                         (invoke-operation compile-or-end-function))
               (add-word ?*symbol-begin-function*
                         FALSE
                         TRUE
                         (invoke-operation new-compile-target))
               (bind ?*has-setup-initial-dictionary*
                     TRUE)))
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
                     (length$ ?*current-input*)))
             (bind ?output
                   (nth$ ?*current-index*
                         ?*current-input*))
             (bind ?*current-index*
                   (+ ?*current-index* 1))
             ?output)
(deffunction MAIN::new-compile-target
             ()
             ; goto the next input set
             (bind ?name 
                   (next-word))
             )
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
             (bind ?finished-compiling
                   (eq ?word `))
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
                     ?entry)))
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


(deffunction MAIN::control-loop
             ()
             (setup-dictionary)
             (while ?*keep-executing* do
                    (bind ?*current-input* 
                          (explode$ (readline)))
                    (bind ?*current-index*
                          1)
                    (bind ?*current-length*
                          (length$ ?*current-input*))
                    (while (and (> ?*current-length* 0)
                                (<= ?*current-index*
                                    ?*current-length*)) do
                           (bind ?word
                                 (nth$ ?*current-index*
                                       ?*current-input*))
                           (if ?*ignore-input* then
                             (if (eq ?word ")") then
                               (bind ?*ignore-input* FALSE))
                             else
                             (if (eq ?word "(") then
                               (bind ?*ignore-input* TRUE)
                               else
                               (bind ?entry (lookup-word ?word))
                               (if ?*compiling* then
                                 (handle-compilation ?word
                                                     ?entry)
                                 (if ?*error-happened* then (break))
                                 else
                                 (invoke-or-push ?word
                                                 ?entry)
                                 (if ?*error-happened* then (break)))))
                           (bind ?*current-index*
                                 (+ ?*current-index* 1)))
                    (if ?*error-happened* then
                      (send [parameter] put-contents (create$))
                      (send [subroutine] put-contents (create$))
                      (printout werror ?*error-message* crlf)
                      (bind ?*error-happened* FALSE)
                      (bind ?*error-message* FALSE)
                      else
                      (if (and (not ?*ignore-input*)
                               (not ?*compiling*)) then
                        (printout t " ok" crlf)))))






