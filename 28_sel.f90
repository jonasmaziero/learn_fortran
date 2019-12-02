include '09_arrays.f90'
include '13_random.f90'
!-----------------------------------------------------------------------------------------
program interpol
  call test_interpol()
  !call sel3d_test()
end program
!-----------------------------------------------------------------------------------------
subroutine test_interpol()
  implicit none
  integer, parameter :: n = 5
  integer :: j
  real(8) :: xj(6), f(6)
  real(8) :: x, dx, lagrange_poly, linear_interpol
  real(8) :: a(5), b(5), c(5), d(5), sc
  real :: dp(6)

  call init_gnu()
  call simula_dado(50,dp)
  open(unit=12,file='dado.dat',status='unknown') ! dist. prob. do dado aleatório
  do j = 1, n+1
    read(12,*) xj(j), f(j)
  enddo
  close(12)
  
  
  open(unit=13, file='interpol.dat', status='unknown')
  call spline_cubico(5,xj,f,a,b,c,d)
  dx = 0.01
  x = 1 - dx
  dox: do
    x = x + dx
    if (x > 6) exit dox
    doj: do j = 1, 5
      if ( x >= xj(j) .and. x < xj(j+1) ) then
        sc = a(j)*(x-xj(j))**3 + b(j)*(x-xj(j))**2 + c(j)*(x-xj(j)) + d(j)
      endif
    enddo doj
    write(13,*) x, lagrange_poly(n,xj,f,x), linear_interpol(n,xj,f,x), sc
    !write(*,*) x, lagrange_poly(n,xj,f,x), linear_interpol(n,xj,f,x), sc
  enddo dox
  close(13)

  open(unit=14,file="interpol.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'interpol.eps'"
  write(14,*)"plot [0.9:6.1][0:0.5] 'dado.dat' u 1:2 w p ps 2 pt 5 t 'dp dado',\"
  write(14,*)"'interpol.dat' u 1:2 w l lt 3 t 'lagrange',\"
  write(14,*)"'interpol.dat' u 1:3 w l lt 0 t 'linear',\"
  write(14,*)"'interpol.dat' u 1:4 w l lt 7 t 'cubic spline'"
  close(14)
  call system("gnuplot interpol.gnu")
  !call system("evince interpol.eps&")
  call system("open -a skim interpol.eps&")

end subroutine
!-----------------------------------------------------------------------------------------
function lagrange_poly(n,x,f,x0)  
  ! Retorna o polinômio de Lagrange calculado no ponto x0
  ! ref: P. L. DeVries, A first course in Computational Physics, Wiley, 1984. 
  implicit none
  real(8) :: lagrange_poly
  integer :: n, m, j, k, l
  real(8) :: x(n+1), f(n+1), x0
  real(8) :: pn, pd

  m = n + 1
  lagrange_poly = 0
  do1: do j = 1, m
    pn = 1 ! calcula o numerador
    do2: do k = 1, m 
      if (k /= j) pn = pn*(x0-x(k))
    end do do2
    pd = 1 ! calcula o denominador
    do3: do l = 1, m
      if (l /= j) pd = pd*(x(j)-x(l))
    end do do3
    lagrange_poly = lagrange_poly + (pn/pd)*f(j) ! cada termo
  end do do1

end function
!-----------------------------------------------------------------------------------------
function linear_interpol(n,x,f,x0) 
  ! Retorna interpolação linear calculada no ponto x0
  ! ref: P. L. DeVries, A first course in Computational Physics, Wiley, 1984.
  implicit none
  real(8) :: linear_interpol
  integer :: n, j
  real(8) :: x(n+1), f(n+1), x0

  do j = 1, n
    if (x0 >= x(j) .and. x0 < x(j+1)) then
      linear_interpol = f(j) + ((f(j+1)-f(j))/(x(j+1)-x(j)))*(x0-x(j))
      exit
    end if
  end do

end function
!-----------------------------------------------------------------------------------------
subroutine spline_cubico(n,x,G,a,b,c,d)
  ! Retorna os coeficientes para o spline cúbico
  ! ref: P. L. DeVries, A first course in Computational Physics, Wiley, 1984.
  implicit none
  integer :: n ! Número de intervalos
  real(8) :: x(n+1), G(n+1) ! dados a serem interpolados
  real(8) :: h(n) ! intervalos: h(j)=x(j+1)-x(j)
  real(8) :: di(n-2), ds(n-2), dp(n-1), nh(n-1) ! diagonais inferior, superior, principal
  											! e vetor de não homogeneidade
  real(8) :: a(n), b(n), c(n), d(n) ! coeficientes
  real(8) :: Gll(n+1) ! derivadas segundas
  integer :: j
  real(8) :: Dll(n-1)
  
  do j = 1, n
  	h(j) = x(j+1) - x(j)
  	if ( h(j) == 0 ) then
  	  write(*,*) 'intervalo nulo'; stop
  	endif
  enddo
  
  do j = 1, n-2
  	  di(j) = h(j+1)
  	  ds(j) = h(j+1)
  	  dp(j) = 2*(h(j)+h(j+1))
  	  nh(j) = 6*((G(j+2)-G(j+1))/h(j+1)	- (G(j+1)-G(j))/h(j))
  enddo
  dp(n-1) = 2*(h(n-1)+h(n))
  nh(n-1) = 6*((G(n+1)-G(n))/h(n)	- (G(n)-G(n-1))/h(n-1))
    
  call sel_3diag(n-1,di,dp,ds,nh,Dll)
  Gll(2:n) = Dll(1:n-1); Gll(1) = 0; Gll(n+1) = 0
  
  do j = 1, n
  	d(j) = G(j)
  	b(j) = Gll(j)/2
  	a(j) = (Gll(j+1)-Gll(j))/(6*h(j))
  	c(j) = (G(j+1)-G(j))/h(j) - (Gll(j+1)+2*Gll(j))*(h(j)/6)
  enddo
  
end subroutine
!-----------------------------------------------------------------------------------------
subroutine sel_3diag(n,a,b,c,r,x)
  ! retorna a solução de um sistema de eqs. lineares tri-diagonal
  ! ref: P. L. DeVries, A first course in Computational Physics, Wiley, 1984.
  implicit none
  integer :: n ! dimensão da matriz de coeficientes
  real(8) :: a(n-1) ! diagonal inferior da matriz de coeficientes
  real(8) :: b(n) ! diagonal principal da matriz de coeficientes
  real(8) :: c(n-1) ! diagonal superior da matriz de coeficientes
  real(8) :: r(n) ! vetor de não homogeneidade
  real(8) :: x(n) ! vetor solução do sel
  integer :: j
  real(8) beta(n), rho(n) ! variáveis auxiliares
  real(8) :: razao
  
  beta(1) = b(1)
  rho(1) = r(1)
  do j = 2, n
    if ( beta(j-1) == 0 ) then
    	write(*,*) 'elemento nulo no sel tridiagonal'
    	stop
    endif
  	razao = a(j-1)/beta(j-1)
  	beta(j) = b(j) - razao*c(j-1)
  	rho(j) = r(j) - razao*rho(j-1)
  enddo
  
  x(n) = rho(n)/beta(n)
  do j = n-1, 1, -1
  	x(j) = (rho(j)-c(j)*x(j+1))/beta(j)
  enddo
  
end subroutine
!-----------------------------------------------------------------------------------------
subroutine sel3d_test() ! ok
  ! Teste para sel_3diag(n,a,b,c,r,x)
  implicit none
  integer, parameter :: n = 5
  real(8) :: a(n-1), b(n), c(n-1), r(n), x(n), M(n,n)
  integer :: j,k 
	
  r = (/ 0, 1, 2, 3, 4 /); write(*,*) 'r = ', real(r)
  a = (/ -1, -1, -1, -1 /)
  b = (/ 2, 2, 2, 2, 2 /)
  c = (/ -1, -1, -1, -1 /)
  
  call sel_3diag(n,a,b,c,r,x); write(*,*) 'x = ', real(x)
  
  write(*,*) 'verificação'
  do j = 1, n ! monta a matriz de coeficientes
  	do k = 1, n ! pode usar tb para montar os vetores da a matriz
  	  if1: if ( j == k) then
  	    M(j,k) = b(j)
  	  else
  	    if2: if ( (k-j) == 1 ) then
  	       M(j,k) = c(j)
  	    else if ( (k-j) == -1 ) then
  	       M(j,k) = a(j-1)
  	    else
  	       M(j,k) = 0.d0
  	    endif if2
  	  endif if1
    enddo
  enddo
  write(*,*) 'M = '; call display_array(real(M), n, n)
  write(*,*) 'r = ', real(matmul(M,x))
  
end subroutine
!-----------------------------------------------------------------------------------------
