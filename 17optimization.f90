!-----------------------------------------------------------------------------------------------------------------------------------
!program opt
!  call test_opt()
  !call test_der_par()
!end
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_opt()
  implicit none
  integer, parameter :: d = 2
  real(8), external :: ff
  real(8) :: xi(d), xf(d)
  integer, parameter :: Nmax = 100
  real(8), parameter :: del = 0.001
  real(8), parameter :: err = 0.00001
  real(8) :: xmin(d), xmax(d), dx(d), x(d)
  open(unit=13,file='opt_f.dat',status='unknown')

  dx(1) = 0.05;  dx(2) = 0.05
  xmin(1) = -10;  xmin(2) = -10
  xmax(1) = 10;  xmax(2) = 10
  x(1) = xmin(1) - dx(1)
  do1: do
    x(1) = x(1) + dx(1)
    if (x(1) > xmax(1)) exit do1
    x(2) = xmin(2) - dx(2)
    do2: do
      x(2) = x(2) + dx(2)
      if (x(2) > xmax(2)) exit do2
      write(13,*) x(1), x(2), ff(d,x)
    end do do2
  end do do1
  close(13)

  xi(1) = -9; xi(2) = 9
  call grad_desc(ff,d,xi,err,Nmax,del,xf)
  write(*,*) 'xf = ', xf, ', f(d,xf) =', ff(d,xf)
  ! os gráficos são feitos com o Jupyter: https://github.com/jonasmaziero/jupyterQ, no arquivo opt.ipynb

end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine grad_desc(f,d,xjm1,err,Nmax,del,xj)  ! algorithm from wikipedia
  implicit none
  real(8), external :: f
  integer :: d  ! número de parâmetros
  real(8) :: err ! precisão (menor norma do gradiente)
  real(8) :: del ! o delta para as derivadas
  real(8) :: grad(d)
  integer :: Nmax, Nint ! número máximo e atual de interações
  real(8) :: norm, inner, gj
  real(8) :: dx(d), xj(d), xjm1(d), dg(d), gradj(d), gradjm1(d)
  !open(unit=13,file='opt_x.dat',status='unknown')

  call gradiente(f,d,xjm1,del,gradjm1)
  xj = xjm1 - 0.00001*gradjm1
  write(13,*) xj(1), xj(2)
  call gradiente(f,d,xj,del,gradj)
  dx = xj - xjm1;  dg = gradj - gradjm1
  gj = inner(d,dx,dg)/(norm(d,dg)**2)
  Nint = 0
  do
    Nint = Nint + 1
    xjm1 = xj;  gradjm1 = gradj
    xj = xjm1 - gj*gradjm1
    !write(13,*) xj(1), xj(2)
    call gradiente(f,d,xj,del,gradj)
    dx = xj - xjm1;  dg = gradj - gradjm1
    gj = inner(d,dx,dg)/(norm(d,dg)**2)
    !write(*,*) 'Nint = ', Nint, ', xj = ', xj
    if (norm(d,gradj) < err .or. Nint > Nmax) exit
  end do

end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
function inner(d,x,y)
  implicit none
  real(8) :: inner
  integer :: d, j
  real(8) :: x(d), y(d)

  inner = 0
  do j = 1, d
    inner = inner + x(j)*y(j)
  end do

end function
!-----------------------------------------------------------------------------------------------------------------------------------
function norm(d,x)
  implicit none
  real(8) :: norm, inner
  integer :: d
  real(8) :: x(d)

  norm = dsqrt(inner(d,x,x))

end function
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine gradiente(f,d,x,del,grad)
  implicit none
  real(8), external :: f
  integer :: d, j
  real(8) :: x(d), grad(d), del, der_par

  do j = 1, d
    grad(j) = der_par(f,d,x,j,del)
  end do

end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
function der_par(f,d,x,j,del)  ! derivada parcial em relação a x_j
  implicit none
  real(8) :: der_par
  real(8), external :: f
  integer :: d ! número de componentes do vetor x
  real(8) :: x(d)
  integer :: j ! componente na qual aplicamos a derivada
  real(8) :: xpd(d) ! variável auxiliar
  real(8) :: del

  xpd = x
  xpd(j) = x(j) + del
  der_par = (f(d,xpd) - f(d,x))/del

end function
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_der_par()
  implicit none
  real(8), external :: ff
  integer, parameter :: d = 2
  real(8) :: x(d), xmin(d), xmax(d), dx(d)
  real(8) :: del, der_par, der
  open(unit=13,file="der_par.dat",status='unknown')

  xmin(1) = -1; xmin(2) = -1
  xmax(1) = 1; xmax(2) = 1
  dx(1) = 0.1; dx(2) = 0.1
  del = 0.001

  x(1) = xmin(1)
  do1: do
    x(1) = x(1) + dx(1)
    if (x(1) > xmax(1)) exit do1
    x(2) = xmin(2)
    do2: do
      x(2) = x(2) + dx(2)
      if (x(2) > xmax(2)) exit do2
      der = der_par(ff,d,x,2,del)
      write(13,*) x(1), x(2), der, 6*x(2)
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
!-----------------------------------------------------------------------------------------------------------------------------------
function ff(d,x)
  implicit none
  real(8) :: ff
  integer :: d
  real(8) :: x(d)

  ff = 2*x(1)**2 + x(1) + 3*x(2)**2

end function
!-----------------------------------------------------------------------------------------------------------------------------------
