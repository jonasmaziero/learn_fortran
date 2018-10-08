!-----------------------------------------------------------------------------------------------------------------------------------
program roots
  !use cte
  !write(*,*) pi, c, e, eps0, mu0, G, h, hb, kB, alpha; stop
  call roots_test()
end program
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine roots_test()
  implicit none
  real(8) :: err, xi, xf, xr, dx, x
  integer :: Nm
  real(8), external :: func
  real(8) :: xe, xd
  integer :: Er
  open(unit=13,file="roots.dat",status="unknown")

  err = 1.d-5;  Nm = 10**3  ! erro e no. máximo de iterações
  xi = -3.d0;  xf = 3.d0 ! intervalo para a procura da raíz
  dx = 1.d-2;  x = xi-dx
  do
    x = x + dx;  write(13,*) x, func(x)
    if ( x > xf ) exit
  end do
  close(13)
  open(unit=14,file="roots.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'roots.eps'"
  write(14,*)"plot 'roots.dat' u 1:2 w lp, 0"
  close(14)
  call system("gnuplot roots.gnu")
  call system("evince roots.eps&")

  write(*,*) "Obtendo uma ou nenhuma raiz"
  xe = xi;  xd = xf
  call bissection(func,xe,xd,err,Nm,xr,Er)
  write(*,*) "xr = ",xr,"f(xr) = ", func(xr)

  write(*,*) "Obtendo várias raízes"
  xe = xi - dx
  do
    xe = xe + dx;  xd = xe + dx
    call bissection(func,xe,xd,err,Nm,xr,Er)
    if (Er == 1) write(*,*) "xr = ",xr,"f(xr) = ", func(xr)
    if ( xd >= xf ) exit
  end do

end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine bissection(f,xe,xd,err,Nm,xr,Er)
  implicit none
  real(8), external :: f
  real(8) :: xe, xd, err, xm, xr, fxm, dx
  integer :: Nm, Ni
  integer :: Er

  if1: if ( f(xe)*f(xd) > 0 ) then
    !write(*,*) "Não há raíz nesse intervalo"
    Er = 0
  else
    Ni = 0
    do1: do
      dx = xd-xe;  if ( dx < err ) exit do1
      xm = (xe+xd)/2.d0;  fxm = f(xm)
      if2: if ( fxm == 0.d0 ) then
        exit do1
      else if ( f(xe)*f(xm) < 0.d0 ) then
        xd = xm
      else
        xe = xm
      end if if2
      Ni = Ni + 1;  if (Ni > Nm) exit do1
    end do do1
    xr = xm;  Er = 1
  end if if1

end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
function func(x)
  implicit none
  real(8) :: x, func

  func = x**3.d0 + 3.d0*x**2.d0 - x - 4.d0

end function
!-----------------------------------------------------------------------------------------------------------------------------------
