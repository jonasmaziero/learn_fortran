include "15_derivada.f90"
!-------------------------------------------------------------------------------
program roots !gfortran 20_roots.f90 19_modf.o
  call roots_test()
end program
!-------------------------------------------------------------------------------
subroutine roots_test()
  use modf
  implicit none
  real(8) :: err, xi, xf, xrb, xrn, dx, xm, x, xk, del
  integer :: Nm
  !real(8), external :: func ! não declara pois  está no módulo
  real(8) :: xe, xd
  integer :: Er ! diz se há (=1) ou não (=0) raíz em um determinado intervalo
  open(unit=13,file="roots.dat",status="unknown")

  xi = -3.d0;  xf = 2.5d0 ! intervalo para a procura da raíz
  dx = 1.d-2;  x = xi-dx
  do
    x = x + dx
    write(13,*) x, func(x)
    !write(13,*) x, func2(x)	
    if ( x > xf ) exit
  end do
  close(13)
  open(unit=14,file="roots.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'roots.eps'"
  write(14,*)"plot 'roots.dat' u 1:2 w l lw 2, 0"
  close(14)
  call system("gnuplot roots.gnu")
  !call system("evince roots.eps&")
  call system("open -a skim roots.eps&")
 ! stop 
!________________________________________
  write(*,*) "Obtendo uma ou nenhuma raiz"
  xe = xi;  xd = xf ! intervalor no qual procuraremos as raízes
  err = 1.d-5;  Nm = 10**2  ! erro máximo e no. máximo de iterações
  !call bissection(func, xe, xd, err, Nm, xrb, Er)
  !write(*,*) "xrb = ", xrb, "f(xrb) = ", func(xrb)
  xk = -1.5d0
  call newton(func, xk, err, Nm, dx, xrn)
  write(*,*) "xrn = ", xrn, "f(xrn) = ", func(xrn)
  !stop
!___________________________________
  write(*,*) "Obtendo várias raízes"
  del = 1.d-1
  xe = xi - del
  do
    xe = xe + del
    xd = xe + del
    call bissection(func, xe, xd, err, Nm, xrb, Er)
    if (Er == 1) write(*,*) "xrb = ", xrb, "f(xrb) = ", func(xrb)
    if ( xd > xf ) exit
  end do

end subroutine
!-------------------------------------------------------------------------------
subroutine bissection(f, xe, xd, err, Nm, xr, Er)
  implicit none
  real(8), external :: f
  real(8) :: xe, xd ! limites "esquerdo" e "direito" para procurar a raíz
  real(8) :: xm, fxm ! ponto médio e valor da função nesse ponto
  real(8) :: xr ! valor da raíz
  real(8) :: err ! largura máxima tolerada do intervalo no qual procuramos a raíz
  real(8) :: dx ! largura atual do intervalo no qual procuramos a raíz
  integer :: Nm ! número máximo de iterações
  integer :: Ni ! número atual de iterações
  integer :: Er ! se = 0 => não há raíz; se = 1 => há pelo menos uma raíz

  if1: if ( f(xe)*f(xd) > 0 ) then
    Er = 0 !write(*,*) "Não há raíz nesse intervalo"
  else
    Ni = 0
    do1: do
      dx = xd-xe
      if ( dx < err ) exit do1 ! primeiro critério de parada
      xm = (xe+xd)/2.d0
      fxm = f(xm)
      if2: if ( dabs(fxm) < 1.d-5 ) then ! segundo critério de parada
        ! dabs é o valor absoluto com dupla precisão
        exit do1
      else if ( f(xe)*f(xm) < 0.d0 ) then
        xd = xm
      else
        xe = xm
      end if if2
      !write(*,*) 'xe = ', xe, 'xd = ', xd
      Ni = Ni + 1
      if (Ni > Nm) exit do1 ! terceiro critério de parada
    end do do1
    xr = xm
    Er = 1
  end if if1

end subroutine
!-------------------------------------------------------------------------------
subroutine newton(f, xk, err, Nm, h, xr)
  implicit none
  real(8), external :: f
  real(8) :: xr ! a raíz
  real(8) :: xk ! chute inicial para a raíz
  real(8) :: diffn, h ! função pra calcular derivadas e o "delta" associado
  real(8) :: err ! valor máximo tolerado para o valor absoluto da função em xr
  integer :: Ni, Nm ! No. atual e máximo de iterações

  Ni = 0
  do1: do
    if1: if (dabs(f(xk)) < err) then ! primeira condição de parada
      exit do1
    else
      xk = xk - f(xk)/diffn(f,xk,h,1)
      !write(*,*) 'xk = ', xk, 'f(xk) = ', f(xk)
    end if if1
    Ni = Ni + 1
    if (Ni > Nm) exit do1 ! segunda condição de parada
  end do do1
  xr = xk

end subroutine
!-------------------------------------------------------------------------------
