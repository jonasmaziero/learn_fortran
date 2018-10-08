!------------------------------------------------------------------------------------------------------------------------------------
program taylorr

  !call subtaylor()
  call taylor_decomposition()

end program taylorr
!------------------------------------------------------------------------------------------------------------------------------------
subroutine subtaylor()
  implicit none
  real(8), parameter :: pi = 4*atan(1.0)
  real(8) :: x, dx, xmax, x0, taylor
  real(8), external :: sen
  open(unit=13,file="taylor.dat",status="unknown")

  xmax = 2.d0*pi
  dx = pi/100.d0
  x0 = pi/2.d0
  x = -dx
  do
    x = x + dx
    write(13,*) x,sen(x),taylor(sen,x,x0,1),taylor(sen,x,x0,2),taylor(sen,x,x0,3),taylor(sen,x,x0,4),taylor(sen,x,x0,5)
    if(x>xmax)exit
  enddo

  ! Para compilar:
  ! gfortran 04fatorial.f90 09derivada.f90 10taylor.f90
  ! Para o gráfico:
  close(13)
  open(unit=14,file="taylor.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'taylor.eps'"
  write(14,*)"plot [0:2*pi][-1.01:1.01] 'taylor.dat' u 1:2 w l,'' u 1:3 w p pt 1,'' u 1:4 w p pt 1,'' u 1:5 w p pt 1,&
            '' u 1:6 w p pt 1"
  close(14)
  call system("gnuplot taylor.gnu")
  call system("evince taylor.eps&")

end subroutine subtaylor
!------------------------------------------------------------------------------------------------------------------------------------
function taylor(f,x,x0,order)
  implicit none
  real(8) :: taylor, x, x0, diffn
  integer :: order, j, fat
  real(8), external :: f
  real(8), parameter :: h = 1.d-2

  taylor = f(x0)
  do j = 1, order
    taylor = taylor + (diffn(f,x,h,j)*(x-x0)**j)/fat(j)
  end do

end function
!------------------------------------------------------------------------------------------------------------------------------------
subroutine taylor_decomposition()
  implicit none
  real(8), parameter :: pi = 4.d0*datan(1.d0)
  real(8) :: dx, x, f, ft1, ft2, ft3, ft4, ft5, ft6, ft7
  integer :: fat
  open(unit=13,file='taylor_decomp.dat',status='unknown')

  dx = pi/100.d0
  x = -2.d0*pi - dx
  do
    x = x + dx
    ft1 = x
    ft2 = x - (x**3.d0)/fat(3)
    ft3 = x - (x**3.d0)/fat(3) + (x**5.d0)/fat(5)
    ft4 = x - (x**3.d0)/fat(3) + (x**5.d0)/fat(5) - (x**7.d0)/fat(7)
    ft5 = x - (x**3.d0)/fat(3) + (x**5.d0)/fat(5) - (x**7.d0)/fat(7) + (x**9.d0)/fat(9)
    ft6 = x - (x**3.d0)/fat(3) + (x**5.d0)/fat(5) - (x**7.d0)/fat(7) + (x**9.d0)/fat(9) - (x**11.d0)/fat(11)
    ft7 = x - (x**3.d0)/fat(3) + (x**5.d0)/fat(5) - (x**7.d0)/fat(7) + (x**9.d0)/fat(9) - (x**11.d0)/fat(11) + (x**13.d0)/fat(13)
    write(13,*) x, dsin(x), ft1, ft2, ft3, ft4, ft5, ft6, ft7
    if (x > 2.d0*pi) exit
  enddo
  close(13)
  open(unit=14,file="taylor_decomp.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'taylor_decomp.eps'"
  write(14,*)"plot [-2*pi:2*pi][-1.1:1.1] 'taylor_decomp.dat' u 1:2 w l,'' u 1:3 w p pt 1,'' u 1:4 w p pt 1,'' u 1:5 w p pt 1,&
            '' u 1:6 w p pt 1,'' u 1:7 w p pt 1,'' u 1:8 w p pt 1,'' u 1:9 w p pt 1"
  close(14)
  call system("gnuplot taylor_decomp.gnu")
  call system("evince taylor_decomp.eps&")

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
