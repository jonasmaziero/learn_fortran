!-----------------------------------------------------------------------------------------------------------------------------------
program integrais ! gfortran 16_modules.o 18_integral2D.f90
  call integral_tests()
end program
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine integral_tests()
  use cte  ! para usar o módulo cte
  implicit none
  real(8) :: integral, xi, xf, yi, yf, x, y, delta, int2d, ff, erro
  real(8), external :: funcao
  integer :: Nx, Ny
  open(unit=13,file="integral.dat",status="unknown")

  Nx = 100; Ny = 100; xi = 0.d0; xf = 4*pi; yi = 0.d0; yf = pi
  delta = 0.2
  x = xi
  dox: do
    x = x + delta
    if (x > xf) exit
    y = yi
    doy: do
      y = y + delta
      if (y > yf) exit
      int2d = integral(funcao,xi,x,Nx,yi,y,Ny)
      !ff = dsin(x)*dsin(y)
      ff = dsin(x)*(1.d0-dcos(y))
      erro = abs(ff-int2d)
      if (erro > 1.d-3) write(*,*) erro
      write(13,*) x, y, int2d, ff
    enddo doy
  enddo dox
  close(13)
  open(unit=14,file="integral.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'integral.eps'"
  write(14,*)"splot 'integral.dat' u 1:2:3 w p pt 5 ps 0.5, sin(x)*sin(y)"
  close(14)
  call system("gnuplot integral.gnu")
  !call system("evince integral.eps&")
  call system("open -a skim integral.eps&")

end subroutine
!-------------------------------------------------------------------------------
function integral(f,xi,xf,Nx,yi,yf,Ny)
  implicit none
  real(8) :: integral, xi, xf, yi, yf, dx, dy, med
  integer :: Nx, Ny, j, k
  real(8), external :: f

  dx = (xf-xi)/dble(Nx)
  dy = (yf-yi)/dble(Ny)
  integral = 0.d0
  do j = 1, Nx
    do k = 1, Ny
      !integral = integral + f(xi+j*dx,yi+k*dy) ! definição
      integral = integral + (f(xi+j*dx,yi+k*dy) + f(xi+j*dx,yi+(k+1)*dy) & ! trapezio
                          + f(xi+(j+1)*dx,yi+k*dy) + f(xi+(j+1)*dx,yi+(k+1)*dy))/4.d0
    enddo
  enddo
  integral = integral*dx*dy

end function
!-------------------------------------------------------------------------------
function funcao(x,y)
  implicit none
  real(8) :: funcao, x, y

  !funcao = dcos(x)*dcos(y)
  funcao = dcos(x)*dsin(y)

end function
!-------------------------------------------------------------------------------
! Exercício: Fazer o código para integral 3D
