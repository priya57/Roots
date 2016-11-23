! testing environment for the Newton algorithm
program mainNewton
    implicit none

    ! controling parameters
    real(8)::x0             ! starting value
    real(8)::prec           ! precision
    real(8)::slope          ! slope quality
    real(8)::h              ! for numerical slope calculation
    real(8)::s              ! step aside if bad slope
    integer::itmax          ! maximum number of iterations
    integer::icode          ! return code

    ! path information
    real(8),allocatable,dimension(:,:)::p
    integer::it             ! iteration counter
    integer::ierr           ! memory allocation code
    integer::i

    ! declare external functions
    real(8),external::myF       ! function to analyse
    logical,external::newton    ! function of newton's algorithm
    logical,external::readInput ! function to read the input

    ! read input information
    !                   file name
    if (.not.readInput("newton.inp",x0,prec,slope,h,s,itmax,icode)) then
        write(*,'(a,i5,a)') "*** error: input data not available (",icode,")"
        stop
    end if

    ! check the input
    !write(*,*)x0,prec,slope,h,s,itmax,icode

    ! allocate the memory for the path
    allocate(p(2,itmax),stat=ierr)
    if (ierr /= 0) then
        write(*,*) "*** error: memory not allocatable!"
        stop
    end if

    ! call the newton
    !                | function
    !                |  --- ctrl para. -------- path iterations
    if (newton(myF,x0,prec,slope,h,s,itmax,   p,it)) then
        write (*,*) "solution found!"
    else
        write (*,*) "no solution found after ",itmax," iterations!"
    end if

    ! print the iteration path
    do i=1,it
        write(*,'(a,i3,a,e12.3,a,e12.3)') "i = ",i, " x = ",p(1,i)," f(x) = ",p(2,i)
    end do

    ! clean up
    deallocate(p)

end

! function to be analysed
real(8) function myF(x)
    real(8)::x

   !myF = ((x**2 - 2*sin(3*x))/(3-2*cos(x/2)))*tan(x/2)
   !myF = 6*x**5-7*x**4-2*x**3+6*x**2-x-4
    myF =  (exp(-x/2))   *    (2-5*sin(x/2)+3*cos(x/3))


end function

