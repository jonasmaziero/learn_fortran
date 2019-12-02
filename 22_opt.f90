include '21_der_parcial.f90'
!-----------------------------------------------------------------------------------------
program opt
  call test_opt()
end
!-----------------------------------------------------------------------------------------
subroutine test_opt()
  implicit none
  integer, parameter :: d = 2 ! número de variáveis
  real(8), external :: ff ! funções cujo mínimo ou máximo queremos calcular
  real(8) :: xi(d), xf(d) !
  integer, parameter :: Nmax = 100 ! número máximo de iterações do gradiente descendente
  real(8), parameter :: h = 0.001
  real(8), parameter :: err = 1.d-8 ! precisão (menor norma do gradiente)
  real(8) :: xmin(d), xmax(d), dx(d), x(d)
  open(unit=13,file='opt_f.dat',status='unknown')

  xmin(1) = -20; xmax(1) = 20; xmin(2) = -20; xmax(2) = 20
  dx(1) = 0.05;  dx(2) = 0.05
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

  xi(1) = -19; xi(2) = 19 ! ponto inicial para o GD
  call grad_desc(ff, d, xi, err, Nmax, h, xf)
  write(*,*) 'xf = ', xf, ', f(d,xf) =', ff(d,xf)
  call system("python3 23_opt.py &")
  call system("open -a skim opt.eps&")
  !call system("evince opt.eps&")

end subroutine
!-----------------------------------------------------------------------------------------
subroutine grad_desc(f, d, xjm1, err, Nmax, h, xj) ! GD
  ! https://en.wikipedia.org/wiki/Gradient_descent
  implicit none
  real(8), external :: f
  integer :: d  ! número de parâmetros
  real(8) :: err ! precisão (menor norma do gradiente)
  real(8) :: h ! o delta para as derivadas
  real(8) :: grad(d)
  integer :: Nmax, Nit ! número máximo e atual de iterações
  real(8) :: norm, inner, gj
  real(8) :: dx(d), xj(d), xjm1(d), dg(d), gradj(d), gradjm1(d)
  ! armazenamos os pontos passados durante a decida
  open(unit=13, file='opt_x.dat', status='unknown')

  !call gradiente(f, d, xjm1, h, gradjm1)
  !xj = xjm1 - 0.00001*gradjm1 ! usamos uma valor inicial pequeno para o passo
  !write(13,*) xj(1), xj(2)
  !call gradiente(f, d, xj, h, gradj)
  !dx = xj - xjm1
  !dg = gradj - gradjm1
  !gj = inner(d, dx, dg)/(norm(d, dg)**2) ! tamanho do passo
  xj = xjm1
  gj = 1.d-2
  Nit = 0
  do
    Nit = Nit + 1
    !xjm1 = xj
    !gradjm1 = gradj
    !xj = xjm1 - gj*gradjm1 ! novo ponto
    call gradiente(f, d, xj, h, gradj)
    xj = xj - gj*gradj
    !dx = xj - xjm1
    !dg = gradj - gradjm1
    !gj = inner(d, dx, dg)/(norm(d, dg)**2) ! passo
    !gj = 1.d-3
    write(*,*) 'Nit = ', Nit, ', xj = ', xj, 'f(xj) = ', f(d,xj)
    write(13,*) xj(1), xj(2)
    if (norm(d, gradj) < err .or. Nit > Nmax) exit
  end do

end subroutine
!-----------------------------------------------------------------------------------------
subroutine gradiente(f, d, x, h, grad)
  ! retorna o gradiente da função f no ponto x
  implicit none
  real(8), external :: f
  integer :: d, j
  real(8) :: x(d), grad(d), h, der_par

  do j = 1, d
    grad(j) = der_par(f, d, x, j, h)
  end do

end subroutine
!-----------------------------------------------------------------------------------------
function inner(d, x, y)
  ! retorna o produto escalar entre os vetores x e y
  implicit none
  real(8) :: inner
  integer :: d, j
  real(8) :: x(d), y(d)

  inner = 0
  do j = 1, d
    inner = inner + x(j)*y(j)
  end do

end function
!-----------------------------------------------------------------------------------------
function norm(d, x)
  ! retorna a norma do vetor x
  implicit none
  real(8) :: norm, inner
  integer :: d
  real(8) :: x(d)

  norm = dsqrt(inner(d,x,x))

end function
!-----------------------------------------------------------------------------------------
