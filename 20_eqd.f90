!-----------------------------------------------------------------------------------------------------------------------------------
program eqd
  call test_eq1()
  !call test_sed()
  !call projetil()
end program
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_eq1()
  implicit none
  real(8) :: x0, x, t, del, euler1
  real(8), external :: func
  open(unit=13,file="euler.dat",status="unknown")
  del = 1.d-3
  t = 0;  x0 = 0
  do
    x = euler1(func,x0,t,del);  write(13,*) t, x
    x0 = x;  t = t + del;  if ( t > 3.d0 ) exit
  enddo
  close(13)
  open(unit=14,file="euler.gnu",status="unknown")
  write(14,*) "reset"
  write(14,*) "set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*) "set output 'euler.eps'"
  !write(14,*) "plot 'euler.dat' u 1:2 w p pt 1, x w p pt 2"
  write(14,*) "plot 'euler.dat' u 1:2 w p pt 1, 0.5*x**2 w p pt 2"
  close(14)
  call system("gnuplot euler.gnu")
  !call system("evince euler.eps&")
  call system("open -a skim euler.eps&")
end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
function euler1(f,xt,t,del) ! ED de ordem 1 e 1 variável
  implicit none
  real(8) :: euler1
  real(8) :: xt,t,del
  real(8), external :: f

  euler1 = xt + del*f(xt,t) ! = x(t+del)

end function
!-----------------------------------------------------------------------------------------------------------------------------------
function func(x,t)
  implicit none
  real(8) :: func, x, t
  real(8) :: v0, a

  a = 1
  v0 = 0
  func = v0 + a*t! v=v0+at => x'=x0+v0*t+a*t**2/2

end function
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_sed()
  implicit none
  integer, parameter :: d = 2
  real(8) :: xt(d), xtp1(d), t, del
  open(unit=13,file="sed.dat",status="unknown")
  del = 1.d-3;  xt = 0;  t = 0
  do
    !call rungekutta1(d,xt,t,del,xtp1)
    !call rungekutta2(d,xt,t,del,xtp1)
    call rungekutta4(d,xt,t,del,xtp1)
    write(13,*) t, xtp1(1), xtp1(2)
    xt = xtp1;  t = t + del;  if ( t > 4.d0 ) exit
  enddo
  close(13)
  open(unit=14,file="sed.gnu",status="unknown")
  write(14,*) "reset"
  write(14,*) "set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*) "set output 'sed.eps'"
  write(14,*) "plot 'sed.dat' u 1:2 w p, '' u 1:3 w l"
  close(14)
  call system("gnuplot sed.gnu")
  !call system("evince sed.eps&")
  call system("open -a skim sed.eps&")
end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine rungekutta1(d,xt,t,del,xtp1)  ! = euler
  use modf
  implicit none
  integer :: d
  real(8) :: xt(d),t,del,xtp1(d), f(d)

  call fvec(d,xt,t,f);  xtp1 = xt + del*f

end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine rungekutta2(d,xt,t,del,xtp1)
  use modf
  implicit none
  integer :: d
  real(8) :: xt(d),t,del,xtp1(d), f1(d), f2(d), h

  h = del/2.d0
  call fvec(d,xt,t,f1);  call fvec(d,xt+h*f1,t+h,f2);  xtp1 = xt + del*f2

end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine rungekutta4(d,xt,t,del,xtp1)
  use modf
  implicit none
  integer :: d
  real(8) :: xt(d), t, del, xtp1(d), f1(d), f2(d), f3(d), f4(d), h

  h = del/2.d0
  call fvec(d,xt,t,f1);  call fvec(d,xt+h*f1,t+h,f2);  call fvec(d,xt+h*f2,t+h,f3);  call fvec(d,xt+del*f3,t+del,f4)
  xtp1 = xt + (del/6.d0)*(f1+2*f2+2*f3+f4)

end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine projetil()
  implicit none
  integer, parameter :: d = 2
  real(8) :: xt(d), xtp1(d), vtp1(d), vt(d), g(d), t, del, k
  open(unit=13,file="projetilk2.dat",status="unknown")
  del = 1.d-3;  xt = 0;  t = 0;  g(1) = 0;  g(2) = -10;  vt(1) = 5;  vt(2) = 20;  k = 2
  do
    vtp1 = vt + del*(g-k*vt)
    xtp1 = xt + del*vt
    write(13,*) t, xtp1(1), xtp1(2)
    xt = xtp1;  vt = vtp1;  t = t + del;  if ( t > 4.d0 ) exit
  enddo
  close(13)
  open(unit=14,file="projetil.gnu",status="unknown")
  write(14,*) "reset"
  write(14,*) "set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*) "set output 'projetil.eps'"
  write(14,*) "set title 'Projetil com forca proporcional a velocidade'"
  write(14,*) "set xrange [0:20]"
  write(14,*) "set yrange [0:]"
  write(14,*) "set xlabel 'x'"
  write(14,*) "set ylabel 'y'"
  write(14,*) "plot 'projetilk0.dat' u 2:3 w l t 'k=0', 'projetilk01.dat' u 2:3 w l t 'k=0.1', \"
  write(14,*) "'projetilk025.dat' u 2:3 w l t 'k=0.25', 'projetilk05.dat' u 2:3 w l t 'k=0.5', \"
  write(14,*) "'projetilk1.dat' u 2:3 w l t 'k=1', 'projetilk2.dat' u 2:3 w l t 'k=2'"
  close(14)
  call system("gnuplot projetil.gnu")
  !call system("evince projetil.eps&")
  call system("open -a skim projetil.eps&")
end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
