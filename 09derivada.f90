!------------------------------------------------------------------------------------------------------------------------------------
program derivada  ! gfortran 09derivada.f90 04fatorial.f90
  call derivadas()
end program
!------------------------------------------------------------------------------------------------------------------------------------
subroutine derivadas()
  implicit none
  real(8), parameter :: pi = 4*atan(1.0)
  real(8) :: x, dx, xmax, del
  real(8), external :: sen
  real(8) :: der, der2, der3, der4, dern, derr
  open(unit=13,file="derivada.dat",status="unknown")

  del = 1.d-3
  xmax = 2.d0*pi
  dx = pi/40.d0
  x = -dx
  do
    x = x + dx
    !write(13,*) x, sen(x),der(sen,x,del),dcos(x),der2(sen,x,del),-dsin(x),der3(sen,x,del),-dcos(x),der4(sen,x,del)
    write(13,*) x, sen(x),dern(sen,x,del,1),dcos(x),dern(sen,x,del,2),-dsin(x),dern(sen,x,del,3),-dcos(x),dern(sen,x,del,4)
    !write(*,*) x, sen(x),dern(sen,x,del,1),dcos(x),dern(sen,x,del,2),-dsin(x),dern(sen,x,del,3),-dcos(x),dern(sen,x,del,4)
    !write(13,*) x,sen(x),derr(sen,x,del,1),dcos(x),derr(sen,x,del,2),-dsin(x),derr(sen,x,del,3),-dcos(x),derr(sen,x,del,4)
    if (x > xmax) exit
  enddo
  close(13)
  !stop

  ! Para o gráfico:
  open(unit=14,file="derivada.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'derivada.eps'"
  write(14,*)"plot [0:2*pi][-1.01:1.01] 'derivada.dat' u 1:2 w l,'' u 1:3 w p pt 1,'' u 1:4 w l,'' u 1:5 w p pt 2,'' u 1:6 w l,&
              '' u 1:7 w p pt 3,'' u 1:8 w l,'' u 1:9 w p pt 4"
  close(14)
  call system("gnuplot derivada.gnu")
  !call system("evince derivada.eps&")
  call system("open -a skim derivada.eps")

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
function sen(x)
  implicit none
  real(8) :: sen
  real(8) :: x
  sen = dsin(x)
end function sen
!------------------------------------------------------------------------------------------------------------------------------------
function der(f,x,h)
  implicit none
  real(8) :: der
  real(8) :: x, h
  real(8), external :: f

  der = (f(x+h)-f(x))/h  ! erro ~ h**2
  !der = (f(x+h)-f(x-h))/(2.0*h)  ! erro ~ h**3
  !der = (-f(x+2*h)+8*f(x+h)-8*f(x-h)+f(x-2*h))/(12*h)  ! erro ~ h**4

end function der
!------------------------------------------------------------------------------------------------------------------------------------
function der2(f,x,h)
  implicit none
  real(8) :: der2
  real(8) :: x, h
  real(8), external :: f
  real(8) :: der

  der2 = (f(x+2.d0*h)-2.d0*f(x+h)+f(x))/(h**2.0)
  !der2 = (der(f,x+h,h)-der(f,x,h))/h

end function der2
!------------------------------------------------------------------------------------------------------------------------------------
function der3(f,x,h)
  implicit none
  real(8) :: der3
  real(8) :: x, h
  real(8), external :: f
  real(8) :: der2

  der3 = (f(x+3.d0*h)-3.d0*f(x+2.d0*h)+3.d0*f(x+h)-f(x))/(h**3.d0)
  !der3 = (der2(f,x+h,h)-der2(f,x,h))/h

end function der3
!------------------------------------------------------------------------------------------------------------------------------------
function der4(f,x,h)
  implicit none
  real(8) :: der4
  real(8) :: x, h
  real(8), external :: f
  real(8) :: der3

  der3 = (f(x+4.d0*h)-4.d0*f(x+3.d0*h)+6.d0*f(x+2.d0*h)-4.d0*f(x+h)+f(x))/(h**4.d0)
  !der4 = (der3(f,x+h,h)-der3(f,x,h))/h

end function der4
!------------------------------------------------------------------------------------------------------------------------------------
function dern(f,x,h,n)
  implicit none
  real(8) :: dern, x, h
  real(8), external :: f
  integer :: n, j
  integer(8) :: newtonb

  dern = f(x+n*h) - n*f(x+(n-1)*h)
  if1: if (n > 1) then
    do j = 2, n
      if2: if (mod(n-j,2) == 0) then
        dern = dern + newtonb(n,j)*f(x+(n-j)*h)
      else
        dern = dern - newtonb(n,j)*f(x+(n-j)*h)
      endif if2
    enddo
  endif if1
  dern = dern/h**n

end function dern
!------------------------------------------------------------------------------------------------------------------------------------
function newtonb(n,j)
  implicit none
  integer(8) :: newtonb
  integer :: n, j
  integer(8) :: fat

  newtonb = fat(n)/(fat(j)*fat(n-j))

end function
!------------------------------------------------------------------------------------------------------------------------------------
recursive function derr(f,x,h,order) result(dn)
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
!------------------------------------------------------------------------------------------------------------------------------------
