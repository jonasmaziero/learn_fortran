!------------------------------------------------------------------------------------------------------------------------------------
program derivadas
  call derivada()
end program
!------------------------------------------------------------------------------------------------------------------------------------
subroutine derivada()
  implicit none
  real(8), parameter :: pi = 4*atan(1.0)
  real(8) :: x, dx, xmax, del
  real(8), external :: sen
  real(8) :: der, der2, der3
  open(unit=13,file="derivadaDP.dat",status="unknown")

  del = 1.d-5  ! a derivada segunda está muito sensível a mudanças de del
  xmax = 2.d0*pi
  dx = pi/40.d0
  x = -dx
  do
    x = x + dx
    write(13,*) x, sen(x), der(sen,x,del), dcos(x), der2(sen,x,del), -dsin(x), der3(sen,x,del), -dcos(x)
    if(x>xmax)exit
  enddo

  ! para fazer o gráfico use:
  ! plot [0:2*pi][-1.01:1.01] "derivadaDP.dat" u 1:2 w l,"" u 1:3 w p pt 1,"" u 1:4 w l,"" u 1:5 w p pt 2,"" u 1:6 w l,"" u 1:7 w p pt 3,"" u 1:8 w l
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
  !der = (f(x+h)-f(x-h))/(2.0*h)  ! erro ~ h**2
  !der = (-f(x+2*h)+8*f(x+h)-8*f(x-h)+f(x-2*h))/(12*h)  ! erro ~ h**4

end function der
!------------------------------------------------------------------------------------------------------------------------------------
function der2(f,x,h)
  implicit none
  real(8) :: der2
  real(8) :: x, h
  real(8), external :: f
  real(8) :: der

  !der2 = (f(x+2.0*h)-2.0*f(x+h)+f(x))/(h**2.0)
  der2 = (der(f,x+h,h)-der(f,x,h))/h

end function der2
!------------------------------------------------------------------------------------------------------------------------------------
function der3(f,x,h)
  implicit none
  real(8) :: der3
  real(8) :: x, h
  real(8), external :: f
  real(8) :: der2

  der3 = (der2(f,x+h,h)-der2(f,x,h))/h

end function der3
!------------------------------------------------------------------------------------------------------------------------------------
