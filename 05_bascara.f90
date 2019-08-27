!-------------------------------------------------------------------------------
! O compilador começa a traduzir as instruções a partir do programa principal (PP)
! Quando compilarmos vários arquivos de uma vez, devemos ter um único PP
program bascaraPP
  call bascara()
end program
!-------------------------------------------------------------------------------
! Tente sempre organizar seus programas em subprogramas, um para cada tarefa
! Isso facilita os testes e reutilização de código
! Os subprogramas mais comuns são: subroutine, function, module
subroutine bascara()
 implicit none
 real :: a, b, c, Delta
 complex :: x1, x2

 write(*,*)"Forneça os coeficientes de ax^2+bx+c=0"
 write(*,*)"Digite a"
 read(*,*) a
 if ( a==0 ) write(*,*)"a não pode ser nulo"
 write(*,*)"Digite b"
 read(*,*) b
 write(*,*)"Digite c"
 read(*,*) c
 Delta = b**2.0 - 4.0*a*c
 write(*,*) "Delta =", Delta
 write(*,*) "Raízes"
 if (Delta == 0) then
   x1 = -b/(2*a)
   x2 = x1
   write(*,*) "x1 = ", real(x1), "  x2 = ", real(x2)
 else if (Delta > 0) then
   x1 = (-b + sqrt(Delta))/(2.0*a)
   x2 = (-b - sqrt(Delta))/(2.0*a)
   write(*,*) "x1 = ", real(x1), "  x2 = ", real(x2)
 else if (Delta < 0.) then
   x1 = (-b/(2.0*a)) + (0.0,1.0)*(sqrt(abs(Delta))/(2.0*a))
   x2 = (-b/(2.0*a)) - (0.0,1.0)*(sqrt(abs(Delta))/(2.0*a))
   write(*,*) "Re(x1) = ", real(x1), "Im(x1) = ", aimag(x1)
   write(*,*) "Re(x2) = ", real(x2), "Im(x2) = ", aimag(x2)
 endif

end
!-------------------------------------------------------------------------------
