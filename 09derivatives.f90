!------------------------------------------------------------------------------------------------------------------------------------
program derivadas
  call derivada()
end program
!------------------------------------------------------------------------------------------------------------------------------------
subroutine derivada()
  implicit none
  real, parameter :: pi = 4*atan(1.0)
  real :: x, dx, xmax, del
  real, external :: sen
  real :: der, der2
  open(unit=13,file="derivada.dat",status="unknown")

  del = 1.e-3  ! a derivada segunda está muito sensível a mudanças de del
  xmax = 2.0*pi
  dx = pi/40.0
  x = -dx
  do
    x = x + dx
    write(13,*) x, sen(x), der(sen,x,del), cos(x), der2(sen,x,del), -sen(x)
    if(x>xmax)exit
  enddo

  ! para fazer o gráfico use:
  ! plot [0:2*pi][-1.01:1.01] "derivada.dat" u 1:2 w l, "" u 1:3 w p pt 1, "" u 1:4 w l, "" u 1:5 w p pt 2, "" u 1:6 w l
end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
function sen(x)
  implicit none
  real sen
  real :: x
  sen = sin(x)
end function sen
!------------------------------------------------------------------------------------------------------------------------------------
function der(f,x,h)
  implicit none
  real :: der
  real :: x, h
  real, external :: f

  der = (f(x+h)-f(x))/h  ! erro ~ h**2
  !der = (f(x+h)-f(x-h))/(2.0*h)  ! erro ~ h**2
  !der = (-f(x+2*h)+8*f(x+h)-8*f(x-h)+f(x-2*h))/(12*h)  ! erro ~ h**4

end function der
!------------------------------------------------------------------------------------------------------------------------------------
function der2(f,x,h)
  implicit none
  real :: der2
  real :: x, h
  real, external :: f
  real :: der

  !der2 = (f(x+2.0*h)-2.0*f(x+h)+f(x))/(h**2.0)
  der2 = (der(f,x+h,h)-der(f,x,h))/h

end function der2
!------------------------------------------------------------------------------------------------------------------------------------
