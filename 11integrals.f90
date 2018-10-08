!-----------------------------------------------------------------------------------------------------------------------------------
program integrais ! gfortran 09modules.f90 09derivada.f90 11integrals.f90
  call integral_tests()
end program
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine integral_tests()
  use cte  ! para usar o módulo cte
  implicit none
  real(8) :: integral, x0, xN, delta, dx
  real(8), external :: sen, coss
  integer :: N
  open(unit=13,file="integral.dat",status="unknown")

  !write(*,*) pi
  N = 100 ! delta = (xN-x0)/N=(N*delta-)
  x0 = 0.d0!;  xN = 2.d0*pi
  dx = pi/dble(N)
  xN = x0
  do
    xN = xN + dx
    delta = (xN-x0)/dble(N)!=(x0*(N-1))/dble(N)
    !write(13,*) xN, 1.d0-integral(sen,x0,N,delta), dcos(xN)
    write(13,*) xN, integral(coss,x0,N,delta), dsin(xN)
    if (xN > 4.d0*pi) exit
  enddo
  close(13)
  open(unit=14,file="integral.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'integral.eps'"
  write(14,*)"plot [0:4*pi][-1.01:1.01] 'integral.dat' u 1:2 w p, '' u 1:3 w l"
  close(14)
  call system("gnuplot integral.gnu")
  call system("evince integral.eps&")

end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
function integral(f,x0,N,delta)
  implicit none
  real(8) :: integral, x0, delta
  integer :: N, j
  real(8), external :: f

  integral = 0.d0
  do j = 1, N
    !integral = integral + f(x0+j*delta) ! definição
    integral = integral + (f(x0+(j-1)*delta)+f(x0+j*delta))/2.d0 ! trapezio
  end do
  integral = integral*delta

end function
!-----------------------------------------------------------------------------------------------------------------------------------
function coss(x)
  implicit none
  real(8) :: coss, x
  coss = dcos(x)
end function
!-----------------------------------------------------------------------------------------------------------------------------------
