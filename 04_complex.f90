program complex
implicit none
real :: x,y
complex :: z

x = 1.0
y = 2.0
write(*,*) "x = ", x
write(*,*) "y = ", y
z = x + (0.0,1.0)*y
!z = (x,0) + (0,y) ! não funciona
!z = (x,y) ! tb não funciona
write(*,*) "z = x+iy = ", z
write(*,*) "Re(z) = ", dble(z), "Im(z) = ", imag(z)
write(*,*) "z* = ", conjg(z) 
write(*,*) "|z| = ", abs(z)
write(*,*) "sqrt(x^2+y^2) = ", sqrt(x**2+y**2)

end program
