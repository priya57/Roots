! read the controldata from the inputfile
!
logical function readInput(name,x0,prec,slope,h,s,itmax,icode)
    implicit none

    !         | dimension defined elsewhere
    character(*)::name      ! file name

    ! controling parameters
    real(8)::x0             ! starting value
    real(8)::prec           ! precision
    real(8)::slope          ! slope quality
    real(8)::h              ! for numerical slope calculation
    real(8)::s              ! step aside if bad slope
    integer::itmax          ! maximum number of iterations
    integer::icode          ! return code

    integer::ierr           ! internal error code

    readInput = .false.     ! default return -> no success

    ! open the input
    !    | input channel: we should not use 5 or 6
    !    |
    open(10,file=name,status='old',iostat=ierr)
    if (ierr /= 0) then
        icode = -1          ! i.e. open without success
        return
    end if

    ! read the data
    !    | input channel
    !    |  | free format, it's blank seperated
    read(10,*,iostat=ierr) x0,itmax,prec,slope,h,s
    ! format error
    if (ierr /= 0) then
        icode = -2

    ! correct format
    else
        if (itmax < 1) then
            icode = 1

        ! success
        else
            icode = 0
            readInput = .true.
        end if

    end if

    ! close the input file
    close(10)
end function
