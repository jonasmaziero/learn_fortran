!------------------------------------------------------------------------------------------------------------------------------------
!program derivada
!  call derivadas()
!end program
!------------------------------------------------------------------------------------------------------------------------------------
subroutine derivadas()
  implicit none
  real(8), parameter :: pi = 4*atan(1.0)
  real(8) :: x, dx, xmax, del
  real(8), external :: sen
  real(8) :: der, der2, der3
  real(8) :: diffn
  open(unit=13,file="derivada.dat",status="unknown")

  del = 1.d-3
  xmax = 2.d0*pi
  dx = pi/40.d0
  x = -dx
  do
    x = x + dx
    !write(13,*) x, sen(x), der(sen,x,del), dcos(x), der2(sen,x,del), -dsin(x), der3(sen,x,del), -dcos(x)
    write(13,*) x,sen(x),diffn(sen,x,del,1),dcos(x),diffn(sen,x,del,2),-dsin(x),diffn(sen,x,del,3),-dcos(x),diffn(sen,x,del,4)
    if(x>xmax)exit
  enddo
  close(13)

  ! Para o gráfico:
  close(13)
  open(unit=14,file="derivada.gnu",status="unknown")
  write(14,*)"reset"
  write(14,*)"set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*)"set output 'derivada.eps'"
  write(14,*)"plot [0:2*pi][-1.01:1.01] 'derivada.dat' u 1:2 w l,'' u 1:3 w p pt 1,'' u 1:4 w l,'' u 1:5 w p pt 2,'' u 1:6 w l,&
              '' u 1:7 w p pt 3,'' u 1:8 w l,'' u 1:9 w p"
  close(14)
  call system("gnuplot derivada.gnu")
  !DEC$ IF DEFINED(_WIN32)
    write(*,*)"no ruindows"
  !DEC$ ELSEIF DEFINED(__linux)
    call system("evince derivada.eps&")
  !DEC$ ELSE
    call system("open -a skim derivda.eps&")
  !DEC$ ENDIF

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
recursive function diffn(f,x,h,order) result(dn)
  implicit none
  real(8) :: dn
  real(8) :: x, h
  real(8), external :: f
  integer :: order

  if (order == 1) then
    !dn = (f(x+h)-f(x))/h
    !dn = (f(x+h)-f(x-h))/(2.d0*h)
    dn = (-f(x+2.d0*h)+8.d0*f(x+h)-8.d0*f(x-h)+f(x-2.d0*h))/(12.d0*h)
  else
    dn = (diffn(f,x+h,h,order-1)-diffn(f,x,h,order-1))/h
  end if

end function diffn
!------------------------------------------------------------------------------------------------------------------------------------
