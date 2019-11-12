!-------------------------------------------------------------------------------
include '08_fatorial.f90'
!-------------------------------------------------------------------------------
!program derivada
!  call derivadas()
!end
!-------------------------------------------------------------------------------
subroutine derivadas()
  implicit none
  real(8), parameter :: pi = 4*atan(1.0) ! dupla precisão (15 casas após a vírgula)
  real(8) :: x, dx, xmax, del!, delm1
  real(8), external :: funcao
  real(8) :: der1, der2, der3, der4, dern, derr, diffn
  open(unit=13,file="derivada.dat",status="unknown")

  del = 1.d-3 ! delta_x para a derivada
  !delm1 = 1.d4
  xmax = 2.d0*pi
  dx = pi/40.d0
  x = -dx
  do
    x = x + dx
    write(13,*) x, funcao(x),der1(funcao,x,del),dcos(x),der2(funcao,x,del),-dsin(x),der3(funcao,x,del),-dcos(x),der4(funcao,x,del)
    if (x > xmax) exit
  enddo
  close(13)
  !stop

  ! Para o gráfico:
  open(unit=14,file="derivada.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'derivada.eps'"
  write(14,*)"plot [0:2*pi][-1.01:1.01] 'derivada.dat' u 1:2 w l,&
             '' u 1:3 w p pt 1,'' u 1:4 w l,'' u 1:5 w p pt 2,'' u 1:6 w l,&
              '' u 1:7 w p pt 3,'' u 1:8 w l,'' u 1:9 w p pt 4"
  close(14)
  call system("gnuplot derivada.gnu")
  !call system("evince derivada.eps&")
  call system("open -a skim derivada.eps")

end subroutine
!-------------------------------------------------------------------------------
function funcao(x)
  implicit none
  real(8) :: funcao
  real(8) :: x

  funcao = dsin(x)

end function
!------------------------------------------------------------------------------------------------------------------------------------
function der1(f,x,h)
  implicit none
  real(8) :: der1
  real(8) :: x, h
  real(8), external :: f

  der1 = (f(x+h)-f(x))/h  ! erro ~ h**2
  !der1 = (f(x+h)-f(x-h))/(2.0*h)  ! erro ~ h**3
  !der1 = (-f(x+2*h)+8*f(x+h)-8*f(x-h)+f(x-2*h))/(12*h)  ! erro ~ h**4

end function der1
!-------------------------------------------------------------------------------
function der2(f,x,h)
  implicit none
  real(8) :: der2, der1
  real(8) :: x, h
  real(8), external :: f ! maneira de passar função como argumento
  real(8) :: der

  !der2 = (f(x+2.d0*h)-2.d0*f(x+h)+f(x))/(h**2.0)
  der2 = (der1(f,x+h,h)-der1(f,x,h))/h

end function der2
!-------------------------------------------------------------------------------
function der3(f,x,h)
  implicit none
  real(8) :: der3, der2, x, h
  real(8), external :: f

  !der3 = (f(x+3.d0*h)-3.d0*f(x+2.d0*h)+3.d0*f(x+h)-f(x))/(h**3.d0)
  der3 = (der2(f,x+h,h)-der2(f,x,h))/h

end function der3
!-------------------------------------------------------------------------------
function der4(f,x,h)
  implicit none
  real(8) :: der4, der3, x, h
  real(8), external :: f

  !der4 = (f(x+4.d0*h)-4.d0*f(x+3.d0*h)+6.d0*f(x+2.d0*h)-4.d0*f(x+h)+f(x))/(h**4.d0)
  der4 = (der3(f,x+h,h)-der3(f,x,h))/h

end function der4
!-------------------------------------------------------------------------------
function dern(f,x,h,n)
  implicit none
  real(8) :: dern, x, h
  real(8), external :: f
  integer :: n, j
  integer(8) :: newtonb
  !real(8) :: ha

  !ha = 1.d0/h

  dern = 0.d0
    do j = 0, n
      if2: if (mod(n-j,2) == 0) then
        dern = dern + newtonb(n,j)*f(x+(n-j)*h)
        !dern = dern + newtonb(n,j)*f(x+(n-j)*ha)
      else
        dern = dern - newtonb(n,j)*f(x+(n-j)*h)
        !dern = dern - newtonb(n,j)*f(x+(n-j)*ha)
      endif if2
      !if(j==3)write(*,*)dern
    enddo
  dern = dern/h**n
  !dern = dern*h**n

end function dern
!-------------------------------------------------------------------------------
function newtonb(n,j)
  implicit none
  integer(8) :: newtonb
  integer :: n, j
  integer(4) :: fat

  newtonb = fat(n)/(fat(j)*fat(n-j))

end function
!-------------------------------------------------------------------------------
recursive function derr(f,x,h,order) result(dn) ! recursiva
  implicit none
  real(8) :: dn
  real(8) :: x, h
  real(8), external :: f
  integer :: order

  if (order == 1) then
    dn = (f(x+h)-f(x))/h  ! erro ~ h^2
    !dn = (f(x+h)-f(x-h))/(2.d0*h)  ! erro ~ h^3
    !dn = (-f(x+2.d0*h)+8.d0*f(x+h)-8.d0*f(x-h)+f(x-2.d0*h))/(12.d0*h)  ! erro ~ h^4
  else
    dn = (derr(f,x+h,h,order-1)-derr(f,x,h,order-1))/h
  end if

end function derr
!-------------------------------------------------------------------------------
function diffn(f,x,h,n)
  implicit none
  real(8) :: diffn
  real(8), external :: f
  real(8) :: x, h
  integer :: n
  integer :: j,k,l,nj
  real(8), allocatable :: yd(:), y(:)

  allocate(y(1:n+1))
  dol: do l = 1, n+1
    y(l) = f(x+(l-1)*h)
  enddo dol
  doj: do j = 1, n-1
    nj = n+1-j
    allocate(yd(1:nj))
    dok: do k = 1, nj
      yd(k) = (y(k+1)-y(k))/h
    enddo dok
    deallocate(y)
    allocate(y(1:nj))
    y = yd
    deallocate(yd)
  enddo doj
  diffn = (y(2)-y(1))/h
  deallocate(y)

end function diffn
!-------------------------------------------------------------------------------
