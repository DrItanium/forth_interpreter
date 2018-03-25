( setup the basic forth system )

(
    // : store-then-next = 1 + dup ;
    // : init Constants::systemVariablesStart dup
    //        Constants::parameterStackFull store-then-next
    //        Constants::parameterStackEmpty store-then-next
    //        Constants::subroutineStackEmpty store-then-next
    //        Constants::subroutineStackFull store-then-next
    //        Constants::dictionaryStart store-then-next
    //        Constants::dictionary
namespace Constants {
    constexpr forth::Address systemVariablesStart = 0xFE0000;
    constexpr forth::Address systemVariablesEnd = forth::Machine::largestAddress;
    constexpr forth::Address subroutineStackEmpty = 0xFE0000;
    constexpr forth::Address subroutineStackFull = 0xFD0000;
    constexpr forth::Address parameterStackEmpty = 0xFD0000;
    constexpr forth::Address parameterStackFull = 0xFC0000;
    constexpr forth::Address dictionaryFull = 0xFC0000;
    constexpr forth::Address dictionaryStart = 0xF00000;
} // end namespace Constants
)
: syscon::64KWEntry 10000# ;
: syscon::prev-page 64KWEntry swap - ;
: syscon::next-page 64KWEntry + ;
: syscon::systemVariableStart FE0000#
: syscon::systemVariablesEnd syscon::systemVariablesStart syscon::next-page ;
: syscon::subroutineStackEmpty syscon::systemVariableStart ;
: syscon::subroutineStackFull syscon::subroutineStackEmpty syscon::prev-page ;
: syscon::parameterStackEmpty syscon::subroutineStackFull ;
: syscon::parameterStackFull syscon::parameterStackEmpty syscon::prev-page ;
: syscon::dictionaryFull syscon::parameteterStackFull ;
: syscon::dictionaryStart F00000# ;


: set-variable syscon:systemVariableStart + = ;
: get-variable syscon:systemVariableStart + @ ;
: init 0 get-variable not if
                          true 0 set-variable
                          syscon::systemVariableStart 1 set-variable ( parameter stack empty )
                          1 get-variable prev-page 2 set-variable ( parameter stack full )
                          2 get-variable 3 set-variable ( subroutine stack empty )
                          3 get-variable prev-page 4 set-variable ( subroutine stack full )
                          4 get-variable prev-page 5 set-variable ( dictionary full )
                          F00000# 6 set-variable ( dictionary empty / start ) ;


