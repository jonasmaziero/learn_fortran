!-------------------------------------------------------------------------------
program integrais ! gfortran 16_modules.o 18_integral2D.f90
  call integral_tests()
end program
!-------------------------------------------------------------------------------
subroutine integral_tests()
  use cte  ! para usar o módulo cte
  implicit none
  real(8) :: integral, xi, xf, yi, yf, x, y, delta, int2d, ff, erro, intmc, integral_mc
  real(8), external :: funcao
  integer :: Nx, Ny, N_mc
  open(unit=13,file="integral.dat",status="unknown")

  Nx = 100; Ny = 100; N_mc = 10000
  xi = 0.d0; xf = 4*pi; yi = 0.d0; yf = pi
  delta = 0.2
  x = xi
  dox: do
    x = x + delta ! limite superior em x
    if (x > xf) exit
    y = yi
    doy: do
      y = y + delta ! limite superior em y
      if (y > yf) exit
      int2d = integral(funcao,xi,x,Nx,yi,y,Ny)
      intmc = integral_mc(funcao,xi,x,yi,y,N_mc)
      !ff = dsin(x)*dsin(y)
      !ff = dsin(x)*(1.d0-dcos(y))
      !erro = abs(ff-int2d); if (erro > 1.d-3) write(*,*) erro
      write(13,*) x, y, int2d, intmc
    enddo doy
  enddo dox
  close(13)
  open(unit=14,file="integral.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'integral.eps'"
  write(14,*)"splot 'integral.dat' u 1:2:4 w p pt 5 ps 0.5, sin(x)*(1-cos(y))"
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
      integral = integral + f(xi+j*dx,yi+k*dy) ! definição
      !integral = integral + (f(xi+j*dx,yi+k*dy) + f(xi+j*dx,yi+(k+1)*dy) & ! trapezio
      !                    + f(xi+(j+1)*dx,yi+k*dy) + f(xi+(j+1)*dx,yi+(k+1)*dy))/4.d0
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
!-------------------------------------------------------------------------------
real(8) function rand_ab(a,b)
  implicit none
  real(8) :: a, b, rn

  call random_number(rn)
  rand_ab = a + (b-a)*rn

end function
!-------------------------------------------------------------------------------
function integral_mc(f,xi,xf,yi,yf,N_mc)
  implicit none
  real(8) :: xi, xf, yi, yf, rand_ab, rn1, rn2, integral_mc
  integer :: N_mc, j
  real(8), external :: f

  integral_mc = 0.d0
  do j = 1, N_mc
    rn1 = rand_ab(xi,xf); rn2 = rand_ab(yi,yf)
    integral_mc = integral_mc + f(rn1,rn2)
  end do
  integral_mc = integral_mc/N_mc
  integral_mc = integral_mc*(xf-xi)*(yf-yi)

end function
!-------------------------------------------------------------------------------
