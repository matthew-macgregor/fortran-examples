LOGICAL FUNCTION fIs_Leap_Year(Year)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: Year
    LOGICAL :: Y4, Y100, Y400, ILY
    ! The Gregorian calendar stipulates that a year that is evenly divisible 
    ! by 100 (for example, 1900) is a leap year only if it is also evenly divisible by 400.
    ! 1. If the year is evenly divisible by 4, go to step 2. Otherwise, go to step 5.
    ! 2. If the year is evenly divisible by 100, go to step 3. Otherwise, go to step 5.
    ! 3. If the year is evenly divisible by 400, go to step 4. Otherwise, go to step 5.
    ! 4. The year is a leap year (it has 366 days).
    ! 5. The year is not a leap year (it has 365 days).

    Y4 = MOD(Year, 4) == 0
    Y100 = MOD(Year, 100) == 0
    Y400 = MOD(Year, 400) == 0

    IF (.NOT. Y4) THEN
        ILY = .False.
    ELSE IF (.NOT. Y100) THEN
        ILY = .True.
    ELSE IF (.NOT. Y400) THEN
        ILY = .False.
    ELSE
        ILY = .True.
    END IF

    fIs_Leap_Year = ILY
    RETURN
END FUNCTION fIs_Leap_Year

SUBROUTINE Print_Summary(Year, Days_in_Year, Is_Leap_Year)
    INTEGER, INTENT(IN) :: Year
    INTEGER, INTENT(IN) :: Days_in_Year
    LOGICAL, INTENT(IN) :: Is_Leap_Year
    CHARACTER(len=5) :: LY = 'true'
    IF (Is_Leap_Year) THEN
        LY = 'true'
    ELSE
        LY = 'false'
    END IF
    WRITE(*,'(A, I4, A, A, A, I3, A)') '{ "Year": ', &
        Year, ', "IsLeapYear": ', LY, ', "Days": ', Days_in_Year, ' }'
END SUBROUTINE Print_Summary

SUBROUTINE Get_Year_Arg(Year)
    INTEGER, INTENT(OUT) :: Year
    CHARACTER(len=4) :: arg
  
    IF (command_argument_count() > 0) THEN
        CALL get_command_argument(1, arg)
        READ(arg, '(I4)', iostat=ios) Year
    END IF
END SUBROUTINE

PROGRAM Calculate_Leap_Year
    IMPLICIT NONE
    LOGICAL :: fIs_Leap_Year
    INTEGER :: Year = huge(1)
    INTEGER, PARAMETER :: Num_Months = 12
    INTEGER, DIMENSION(Num_Months) :: Num_Days = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
    INTEGER :: Days_in_Year = 0
    LOGICAL :: Is_Leap_Year

    CALL Get_Year_Arg(Year)
    IF (Year == huge(1)) THEN
        ! WRITE(*,*) Year
        WRITE(*,'(A)') 'Enter a year to check if it is a leap year => '
        READ(*,*) Year
    END IF
    
    Is_Leap_Year = fIs_Leap_Year(Year)
    Days_in_Year = SUM(Num_Days)
    IF (Is_Leap_Year .EQV. .TRUE.) THEN
        ! WRITE(*,'(A, L1)') 'Is Leap Year: ', Is_Leap_Year
        Days_in_Year = Days_in_Year + 1
    END IF

    CALL Print_Summary(Year, Days_in_Year, Is_Leap_Year)
END PROGRAM Calculate_Leap_Year