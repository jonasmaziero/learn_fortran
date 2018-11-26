!-----------------------------------------------------------------------------------------------------------------------------------
program interpol
!  call init_gnu()
  call test_interpol()
end program
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_interpol()
  implicit none
  integer, parameter :: n = 5
  integer :: j
  real(8) :: x(6), f(6)
  real(8) :: x0, dx0, lagrange_poly, linear_interpol
  open(unit=12,file='dado.dat',status='unknown')
  open(unit=13, file='interpol.dat', status='unknown')

  do j = 1, n+1
    read(12,*) x(j), f(j)
    !write(*,*) x(j), f(j)
  enddo
  close(12)

  dx0 = 0.01
  x0 = 0 - dx0
  do
    x0 = x0 + dx0
    write(13,*) x0, lagrange_poly(n,x,f,x0), linear_interpol(n,x,f,x0)
    if (x0 > 7.d0) exit
  end do

  open(unit=14,file="interpol.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'interpol.eps'"
  write(14,*)"plot [:][0:1] 'dado.dat' u 1:2 w p ps 2 pt 5, 'interpol.dat' u 1:2 w l lt 3, \"
  write(14,*)"              'interpol.dat' u 1:3 w l lt 0"
  close(14)
  call system("gnuplot interpol.gnu")
  !call system("evince interpol.eps&")
  call system("open -a skim interpol.eps&")

end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
function lagrange_poly(n,x,f,x0)  ! Retorna o polinômio de Lagrange calculado no ponto x0
  implicit none
  real(8) :: lagrange_poly
  integer :: n, m, j, k, l
  real(8) :: x(n+1), f(n+1), x0
  real(8) :: pn, pd

  m = n + 1
  lagrange_poly = 0
  do1: do j = 1, m
    pn = 1
    do2: do k = 1, m
      if (k /= j) pn = pn*(x0-x(k))
    end do do2
    pd = 1
    do3: do l = 1, m
      if (l /= j) pd = pd*(x(j)-x(l))
    end do do3
    lagrange_poly = lagrange_poly + (pn/pd)*f(j)
  end do do1

end function
!-----------------------------------------------------------------------------------------------------------------------------------
function linear_interpol(n,x,f,x0)
  implicit none
  real(8) :: linear_interpol
  integer :: n, j
  real(8) :: x(n+1), f(n+1), x0

  do j = 1, n
    if (x0 >= x(j) .and. x0 < x(j+1)) then
      linear_interpol = f(j) + ((f(j+1)-f(j))/(x(j+1)-x(j)))*(x0-x(j))
    end if
  end do

end function
!-----------------------------------------------------------------------------------------------------------------------------------
! spline cubico
