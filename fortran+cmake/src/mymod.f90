module mymod
    implicit none
    
    public :: mysub
    contains
    subroutine mysub
        print * , 'From myMod'
    endsubroutine mysub  
endmodule