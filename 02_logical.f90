program logicals
  implicit none  ! esse comando nos obriga a declarar todas as variáveis (use)
  logical :: A, B, C
  integer :: j,k

  A = .true.
  B = .false.
  write(*,*) "A = ",A, ", B = ",B
  write(*,*) ".not.A = ", .not.A

  C = A.or.B
  write(*,*) "A.or.B = ", C

  C = A.and.B
  write(*,*) "A.and.B = ", C

  C = A.eqv.B
  write(*,*) "A.eqv.B = ", C

  C = A.neqv.B
  write(*,*) "A.neqv.B = ", C
  
  j = 2; k = 2
  C = j >= k
  write(*,*) j,">=",k,"?", C

  ! Exercício:
  ! Faça o análogo do feito acima usando os operadores lógicos:
  ! <, ==, /=, >=, <=

end program
