; A quick implementation of a forth interpreter in CLIPS
(defgeneric MAIN::report-error)
(defgeneric MAIN::make-entry)
(defgeneric MAIN::lookup-word)
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
                      (report-error ?self
                                    "Stack Empty!")
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

(defclass dictionary-entry
  (is-a USER)
  (slot title
        (type SYMBOL)
        (visibility public)
        (storage local)
        (default ?NONE))
  (slot next
        (type INSTANCE
              SYMBOL)
        (visibility public)
        (storage local)
        (allowed-symbols FALSE)
        (default ?NONE))
  (slot fake
        (type SYMBOL)
        (visibility public)
        (storage local)
        (allowed-symbols FALSE
                         TRUE))
  (slot compile-time-invoke
        (type SYMBOL)
        (visibility public)
        (storage local)
        (allowed-symbols FALSE
                         TRUE))
  (multislot contents)
  (message-handler has-next primary)
  (message-handler invoke primary))

(defmessage-handler dictionary-entry has-next primary
                    ()
                    ?self:next)

(defmessage-handler dictionary-entry invoke primary
                    ()
                    (progn$ (?c ?self:contents)
                            (send ?c invoke))) 
(defglobal MAIN
           ?*dictionary-pointer* = [unfound-word]
           )
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
              (unfound-word of dictionary-entry
                            (title FALSE)
                            (next FALSE)
                            (fake TRUE)
                            (compile-time-invoke FALSE)
                            (native-funcall FALSE)))
(defmethod lookup-word
  ((?name SYMBOL)
   (?target SYMBOL))
  (handle-error ?name "?")
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


(defmethod make-entry
  ((?word SYMBOL))
  (lookup-word ?word))
(defmethod make-entry
  ((?value NUMBER
           STRING))
  (make-instance of constant
                 (value ?number)))
(defmethod make-entry
  ((?inst INSTANCE))
  ?inst)
(deffunction clear-parameter-stack
             ()
             (send [parameter
(defmethod handle-error
  (?a ?b)
  (printout werror 
            ?a 
            ?b crlf)
  (halt))

(defclass MAIN::wrapped-unary-operation
  (is-a USER)
  (slot operation
        (type SYMBOL)
        (default ?NONE))
  (message-handler invoke primary))
(defmessage-handler wrapped-unary-operation invoke primary
                    ()
                    (send [parameter]
                          push
                          (funcall operation
                                   (send [parameter] pop))))

(defclass MAIN::wrapped-binary-operation
  (is-a USER)
  (slot operation
        (type SYMBOL)
        (default ?NONE))
  (message-handler invoke primary))

(defmessage-handler wrapped-binary-operation invoke primary
                    ()
                    (send [parameter]
                          push
                          (funcall operation
                                   (send [parameter] pop)
                                   (send [parameter] pop))))

(defrule MAIN::make-word
         (stage (current construct-word))
         ?f <- (make word
                     ?name
                     ?fake
                     ?compile-time
                     $?operations)
         =>
         (retract ?f)
         (bind ?contents
               (create$))
         (progn$ (?a ?operations)
                 (bind ?contents
                       ?contents
                       (make-entry ?a)))
         (bind ?*dictionary-pointer*
               (make-instance of dictionary-entry
                              (next ?*dictionary-pointer*)
                              (fake ?fake)
                              (compile-time-invoke ?compile-time-invoke)
                              (title ?title)
                              (contents ?contents))))

