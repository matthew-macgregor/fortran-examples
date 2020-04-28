program ReadCSV
    integer, parameter :: fh = 18
    integer, parameter :: max_rows = 1000
    integer :: i, ios

    TYPE :: City
        INTEGER :: LatD
        INTEGER :: LatM
        INTEGER :: LatS
        CHARACTER(len=1) :: NS
        INTEGER :: LonD
        INTEGER :: LonM
        INTEGER :: LonS
        CHARACTER(len=1) :: EW
        CHARACTER(len=32) :: City
        CHARACTER(len=2) :: State
    END TYPE City

    TYPE(City), DIMENSION(max_rows) :: cities


    open(unit=fh, file='../data/cities.csv', status='OLD', action='read')
    read(fh,*)
    do i = 1, max_rows
        read(fh,*,iostat=ios) cities(i)
        ! print *, cities(i)
        write(*,'(A20, A)') cities(i)%City, cities(i)%EW
        if (ios < 0) then
            print *, 'Exiting loop'
            exit
        endif
    end do
    close(fh)
end program ReadCSV
