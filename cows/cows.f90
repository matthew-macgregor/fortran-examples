! This is an old-style function and return
! Why is it called "cows"? Idk.
FUNCTION func_name(a, b)
   IMPLICIT NONE
   INTEGER :: func_name
   INTEGER :: a
   REAL    :: b
   func_name = INT((2 * a) + b)
END FUNCTION
    
PROGRAM cows
   IMPLICIT NONE
   INTEGER :: func_name
   PRINT *, func_name(2, 1.3)
END PROGRAM