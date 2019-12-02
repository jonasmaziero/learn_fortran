!-----------------------------------------------------------------------------------------
program fit
  call test_fit()
end program
!-----------------------------------------------------------------------------------------
subroutine test_fit()
  use par
  use ffs
  implicit none
  integer :: j
  real(8) :: x0, dx0, a, b
  integer, parameter :: M = 2
  real(8) :: q0(M), q(M)
  real(8), external :: chi2

  open(unit=12,file='dado.dat',status='unknown')
  do j = 1, n
    read(12,*) x(j), f(j)
  enddo
  close(12)
  call linear_fit(n,x,f,a,b)
  q0(1) = 0.03;  q0(2) = 0.02!;  q0(3) = 0.01;  q0(4) = 0.01
  call general_fit(chi2,M,q0,q)

  open(unit=13, file='fit.dat', status='unknown')
  dx0 = 0.01
  x0 = 0.5 - dx0
  do
    x0 = x0 + dx0
    write(13,*) x0, a+b*x0, y(M,q,x0)
    if (x0 > 6.5d0) exit
  end do
  close(13)

  open(unit=14,file="fit.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'fit.eps'"
  write(14,*)"plot [:][0:1] 'dado.dat' u 1:2 w p ps 2 pt 5, 'fit.dat' u 1:2 w l lt 3 lw 3, \"
  write(14,*)"              'fit.dat' u 1:3 w l lt 5 lw 3"
  close(14)
  call system("gnuplot fit.gnu")
  !call system("evince fit.eps&")
  call system("open -a skim fit.eps&")

end subroutine
!-----------------------------------------------------------------------------------------
subroutine linear_fit(n,x,f,a,b)
  implicit none
  real(8) :: linear_interpol
  integer :: n, j
  real(8) :: x(n), f(n), a, b, Xs, Fs, Xss, XFs

  Xs = 0; Fs = 0; Xss = 0; XFs = 0
  do j = 1, n
    Xs = Xs + x(j);  Fs = Fs + f(j);  Xss = Xss + x(j)**2;  XFs = XFs + x(j)*F(j)
  enddo
  a = (Fs*Xss-Xs*XFs)/(n*Xss-Xs**2)
  b = (n*XFs-Xs*Fs)/(n*Xss-Xs**2)

end subroutine
!-----------------------------------------------------------------------------------------
subroutine general_fit(chi2,M,q0,q)
  use par
  implicit none
  integer :: M
  real(8) :: q0(M), q(M)
  real(8), external :: chi2

  call grad_desc(chi2,M,q0,err,Nmax,del,q)

end subroutine
!-----------------------------------------------------------------------------------------
function chi2(M,q)
  use par
  use ffs
  implicit none
  real(8) :: chi2
  integer :: M
  real(8) :: q(M)
  integer :: j

  chi2 = 0
  do j = 1, N
    chi2 = chi2 + (y(M,q,x(j))-f(j))**2
  end do

end function
!-----------------------------------------------------------------------------------------
