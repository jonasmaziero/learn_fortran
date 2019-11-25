include '16_modules.f90'
!include '08_fatorial.f90' ! já está incluso no arquivo das derivadas
include '15_derivada.f90'
!-----------------------------------------------------------------------------------------
program taylorr

  call subtaylor()
  !call taylor_decomposition()

end program taylorr
!-----------------------------------------------------------------------------------------
subroutine subtaylor()
  ! grafica uma função e expanções de Taylor de várias ordens dessa
  use cte
  implicit none
  real(8) :: x, dx, xmax, x0, taylor
  real(8) :: hd ! o delta_x para as derivadas
  real(8), external :: f
  open(unit=13,file="taylor.dat",status="unknown")

  hd = 0.01d0
  xmax = 2.d0*pi
  dx = pi/100.d0
  x0 = 0 ! ponto em torno do qual expandimos f
  write(*,*) 'x0 = ', x0
  x = -2*pi -dx
  do
    x = x + dx
    write(13,*) x, f(x), taylor(f,x,x0,1,hd), taylor(f,x,x0,2,hd),&
    			taylor(f,x,x0,3,hd), taylor(f,x,x0,4,hd), taylor(f,x,x0,5,hd)
    if(x > xmax)exit
  enddo
  
  close(13)
  open(unit=14,file="taylor.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'taylor.eps'"
  write(14,*)"plot [-2*pi:2*pi][-1.01:1.01] 'taylor.dat' u 1:2 w l t 'f',&
  			 '' u 1:3 w p pt 1 t 'ordem 1', '' u 1:4 w p pt 1 t '2',&
  			 '' u 1:5 w p pt 1 t '3'"
  close(14)
  call system("gnuplot taylor.gnu")
  !call system("evince taylor.eps&")
  call system("open -a skim taylor.eps&")

end subroutine
!-------------------------------------------------------------------------------
function f(x)
implicit none
real(8) :: x, f

f = dsin(x)

end function
!-------------------------------------------------------------------------------
function taylor(f, x, x0, order, h)
  implicit none
  real(8) :: taylor, x, x0, h, diffn
  integer :: order, j, fat
  real(8), external :: f

  taylor = f(x0)
  do j = 1, order
    taylor = taylor + (diffn(f,x,h,j)*(x-x0)**j)/fat(j)
  end do

end function
!-----------------------------------------------------------------------------------------
subroutine taylor_decomposition()
  ! Gera dados que indicam equivalência da série de Taylor e de f em qualquer ponto
  use cte
  implicit none
  real(8) :: dx, x, ft1, ft2, ft3, ft4, ft5, ft6, ft7
  real(8), external :: f
  integer :: fat
  open(unit=13,file='taylor_decomp.dat',status='unknown')

  dx = pi/100.d0
  x = -2.d0*pi - dx
  do  ! faz variar o x0 pra fazer o gráfico comparativo com a função
    x = x + dx
    ft1 = x
    ft2 = x - (x**3.d0)/fat(3)
    ft3 = x - (x**3.d0)/fat(3) + (x**5.d0)/fat(5)
    ft4 = x - (x**3.d0)/fat(3) + (x**5.d0)/fat(5) - (x**7.d0)/fat(7)
    ft5 = x - (x**3.d0)/fat(3) + (x**5.d0)/fat(5) - (x**7.d0)/fat(7) + (x**9.d0)/fat(9)
    ft6 = x - (x**3.d0)/fat(3) + (x**5.d0)/fat(5) - (x**7.d0)/fat(7) + (x**9.d0)/fat(9) - (x**11.d0)/fat(11)
    ft7 = x - (x**3.d0)/fat(3) + (x**5.d0)/fat(5) - (x**7.d0)/fat(7) + (x**9.d0)/fat(9) - (x**11.d0)/fat(11) + (x**13.d0)/fat(13)
    write(13,*) x, f(x), ft1, ft2, ft3, ft4, ft5, ft6, ft7
    if (x > 2.d0*pi) exit
  enddo
  
  close(13)
  open(unit=14,file="taylor_decomp.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'taylor_decomp.eps'"
  write(14,*)"plot [-2*pi:2*pi][-1.1:1.1] 'taylor_decomp.dat' u 1:2 w l t 'f',& 
  			 '' u 1:3 w p pt 1 t 'ordem 1', '' u 1:4 w p pt 1 t '2',&
  			 '' u 1:5 w p pt 1 t '3', '' u 1:6 w p pt 1 t '4',& 
  			 '' u 1:7 w p pt 1 t '5', '' u 1:8 w p pt 1 t '6',& 
  			 '' u 1:9 w p pt 1 t '7'"
  close(14)
  call system("gnuplot taylor_decomp.gnu")
  !call system("evince taylor_decomp.eps&")
  call system("open -a skim taylor_decomp.eps&")

end subroutine
!-----------------------------------------------------------------------------------------
