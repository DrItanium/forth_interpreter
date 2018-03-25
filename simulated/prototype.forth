: storeOne 1 swap = ;
: storeZero 0 swap = ;
: memoryCellTrue @ 0 > ;
: shouldIgnoreInput $IgnoreInput memoryCellTrue ;
: shouldKeepExecuting $KeepExecuting memoryCellTrue ;
: inCompileMode $CompileMode memoryCellTrue ;
: noIgnoreInput $IgnoreInput storeZero ;
: yesIgnoreInput $IgnoreInput storeOne ;
: 2dup dup dup ;
: 2drop drop drop ;
: catchError ( word cond prefix -- )
             swap not if handleError else 2drop then ;
: handleComment ( word -- continue? ) dup shouldIgnoreInput if " )" eq.str if noIgnoreInput then 0 else " (" eq.str if yesIgnoreInput 0 else 1 then then ;


(: controlLoop noIgnoreInput shouldKeepExecuting if do
                readWord 2dup 
                handleComment if 
                lookupWord inCompileMode if compileRoutine else
                else 2drop then shouldKeepExecuting continue ;
                    lookupWord inCompileMode if
                        compileRoutine
                        else
                            invokeWord not if numberRoutine " ?" catchError then
                        then
                   then
                then
                shouldKeepExecuting
                continue ; )
