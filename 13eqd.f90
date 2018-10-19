!-----------------------------------------------------------------------------------------------------------------------------------
program eqd
  call test_euler()
end program
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_euler()
  implicit none
  real(8) :: x0, x, t, dt, del, euler
  real(8), external :: func
  open(unit=13,file="euler.dat",status="unknown")

  del = 1.d-3 ! o delta da derivada
  dt = 1.d-2  ! passo para a variável independente
  x0 = 0.d0
  do
    x = euler(func,x0,t,del)
    write(13,*) t, x
    x0 = x
    t = t + dt
    if ( t > 3.d0 ) exit
  enddo
  close(13)
  open(unit=14,file="euler.gnu",status="unknown")
  write(14,*) "reset"
  write(14,*) "set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*) "set output 'euler.eps'"
  write(14,*) "plot 'euler.dat' u 1:2 w lp"
  close(14)
  call system("gnuplot euler.gnu")
  !DEC$ IF DEFINED(_WIN32)
    write(*,*) "no ruindows"
  !DEC$ ELSEIF DEFINED(__linux)
    call system("evince euler.eps&")
  !DEC$ ELSE
    call system("open -a skim euler.eps&")
  !DEC$ ENDIF

end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
function euler(f,xtkm1,tkm1,del)
  implicit none
  real(8) :: euler,xtkm1,tkm1,del
  real(8), external :: f

  euler = xtkm1 + del*f(xtkm1,tkm1) ! = xtk

end function
!-----------------------------------------------------------------------------------------------------------------------------------
function func(x,t)
  implicit none
  real(8) :: func, x, t
  real(8) :: v0, a

  a = t
  !func = a ! = F/m=cte
  v0 = 0.d0
  func = v0 + a*t! v=v0+at => x'=x0+v0*t+a*t**2/2

end function
!-----------------------------------------------------------------------------------------------------------------------------------
