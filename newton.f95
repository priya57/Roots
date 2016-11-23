! implementation of Newton's algorithm

logical function newton(f,x0,prec,slope,h,s,itmax, p,it)
    implicit none

    ! controling parameters
    real(8)::x0             ! starting value
    real(8)::prec           ! precision
    real(8)::slope          ! slope quality
    real(8)::h              ! for numerical slope calculation
    real(8)::s              ! step aside if bad slope
    integer::itmax          ! maximum number of iterations

    ! path information
    real(8),dimension(2,itmax)::p
    integer::it             ! iteration counter
    real(8)::x,fx,fsx       ! current position, function- and slope value

    ! declare external functions
    real(8),external::f     ! function to analyse
    real(8),external::fs    ! function's derivative    it = 1
    x  = x0
    it = 1

    ! iteration loop
    do while(it <= itmax)

        fx      = f(x)
        p(1,it) = x
        p(2,it) = fx

        ! root found
        if (dabs(fx) < prec) then
            newton = .true.
            return
        end if

        ! check the slope
        fsx = fs(f,x,h)

        ! bad slope?
        if (dabs(fsx) < slope) then
            x = x + s

        ! slope ok => newton step
        else
            x = x - fx/fsx

        end if

        it = it +1
    end do
    newton = .false.

end function

! function to calculate the derívate of a function
real(8) function fs(f,x,h)
    real(8),external::f     ! function
    real(8)::x              ! position
    real(8)::h              ! step width

    fs = (f(x+h/2.d0)  - f(x-h/2.d0))/h
end function
