!-----------------------------------------------------------------------------------------------------------------------------------
program integrais ! gfortran 16_modules.o 17_integrals.f90
  call integral_tests()
end program
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine integral_tests()
  use cte  ! para usar o módulo cte
  ! Antes compile com: gfortran -c 16_modules.f90
  ! Depois, compile com: gfortran 17_integrals.f90 16_modules.o
  ! name.o são 'objetos', já compilados (em assembly)
  implicit none
  real(8) :: integral, a, b, delta, dx
  real(8), external :: funcao, funcao2
  integer :: N, M
  open(unit=13,file="integral.dat",status="unknown")

  M = 1000
  !write(*,*) pi
  N = 50 ! delta = (xN-x0)/N=(N*delta-)
  a = 0.d0!;  b = 2.d0*pi
  dx = pi/dble(N)
  b = a
  do
    b = b + dx
    !write(13,*) b, integral(funcao,a,b,N), dsin(b)
    write(13,*) b, integral(funcao2,a,b,M), b**3.d0/6.d0
    if (b > 4.d0*pi) exit
  enddo
  close(13)
  open(unit=14,file="integral.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'integral.eps'"
  write(14,*)"plot [:][:] 'integral.dat' u 1:2 w p, '' u 1:3 w l"
  close(14)
  call system("gnuplot integral.gnu")
  !call system("evince integral.eps&")
  call system("open -a skim integral.eps&")

end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
function integral(f,x0,xN,N)
  implicit none
  real(8) :: integral, x0, xN, delta
  integer :: N, j
  real(8), external :: f

  delta = (xN-x0)/dble(N)
  integral = 0.d0
  do j = 1, N
    integral = integral + f(x0+(j-1)*delta)  ! retangulo (definição)
    !integral = integral + f((x0+(j-1)*delta+x0+j*delta)/2) ! com ponto médio
    !integral = integral + (f(x0+(j-1)*delta)+f(x0+j*delta))/2.d0 ! trapezio
    !integral = integral + (f(x0+(j-1)*delta)+4.d0*f((2.d0*x0+delta*(2*j-1))/2.d0)+f(x0+j*delta))/6.d0  ! simpson
  end do
  integral = integral*delta

end function
!-----------------------------------------------------------------------------------------------------------------------------------
function funcao(x)
  implicit none
  real(8) :: funcao, x
  funcao = dcos(x)
end function
!-----------------------------------------------------------------------------------------------------------------------------------
function funcao2(x)
  implicit none
  real(8) :: funcao2, x
  funcao2 = x**2.d0/2.d0
end function
!-----------------------------------------------------------------------------------------------------------------------------------
