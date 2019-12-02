!-----------------------------------------------------------------------------------------
!program opt
!  call test_der_par()
!end
!-----------------------------------------------------------------------------------------
subroutine test_der_par()
  implicit none
  real(8), external :: ff ! função que vamos derivar
  integer, parameter :: d = 2 ! número de variáveis
  real(8) :: x(d), xmin(d), xmax(d), dx(d) ! vetores para as coordenadas
  real(8) :: h ! delta para as derivadas
  real(8) :: der_par, der
  open(unit=13,file="der_par.dat",status='unknown')

  xmin(1) = -1; xmax(1) = 1; xmin(2) = -1; xmax(2) = 1
  dx(1) = 0.1; dx(2) = 0.1; h = 0.001

  x(1) = xmin(1)
  do1: do
    x(1) = x(1) + dx(1)
    if (x(1) > xmax(1)) exit do1
    x(2) = xmin(2)
    do2: do
      x(2) = x(2) + dx(2)
      if (x(2) > xmax(2)) exit do2
      der = der_par(ff,d,x,2,h); write(13,*) x(1), x(2), der, 6*x(2)
      !der = der_par(ff,d,x,1,h); write(13,*) x(1), x(2), der, 4*x(1)+1
    end do do2
  end do do1
  close(13)

  open(unit=14,file="der_par.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set xlabel 'x'"
  write(14,*)"set ylabel 'y'"
  write(14,*)"set zrange [-6:6]"
  write(14,*)"set output 'der_par.eps'"
  write(14,*)"splot 'der_par.dat' u 1:2:3, '' u 1:2:4"
  close(14)
  call system("gnuplot der_par.gnu")
  !call system("evince der_par.eps&")
  call system("open -a skim der_par.eps&")
  close(14)

end subroutine
!-----------------------------------------------------------------------------------------
function der_par(f,d,x,j,h)  ! derivada parcial em relação a x_j
  implicit none
  real(8) :: der_par ! derivada da função f no ponto x, em relação a x_j
  real(8), external :: f ! função a ser derivada
  integer :: d ! número de componentes do vetor x
  real(8) :: x(d) ! ponto no qual calculamos a derivada
  integer :: j ! componente na qual aplicamos a derivada
  !real(8) :: xpd(d) ! variável auxiliar
  real(8) :: h ! delta para a derivada

  !xpd = x
  !xpd(j) = x(j) + h
  !der_par = (f(d,xpd) - f(d,x))/h
  
  x(j) = x(j) + h
  der_par = f(d,x)
  x(j) = x(j) - h
  der_par = der_par - f(d,x)
  der_par = der_par/h

end function
!-----------------------------------------------------------------------------------------
function ff(d,x)
  implicit none
  real(8) :: ff
  integer :: d
  real(8) :: x(d)

  ff = 2*x(1)**2 + x(1) + 3*x(2)**2
  !ff = (1.d0-x(1))**2 + 1.d2*(x(2)-x(1)**2)**2
  !ff = dsin((x(1)**2)/2.d0 - (x(2)**2)/4.d0 + 3.d0)*cos(2.d0*x(1)+1.d0-dexp(x(2)))

end function
!-----------------------------------------------------------------------------------------