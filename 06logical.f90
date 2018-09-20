program logicals
  implicit none
  logical :: A, B, C

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
  C = 1 > 2
  write(*,*) "1>2 = ", C

  ! Exercício:
  ! Faça o análogo do feito acima usando os operadores lógicos:
  ! <, ==, /=, >=, <=

end program
