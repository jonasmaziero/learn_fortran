program conditionals
implicit none
real :: N1, N2, Np, E, Nf
character(20) :: nome

write(*,*) "Digite o nome do aluno"
read(*,*) nome
write(*,*) "Digite a nota da primeira avaliação"
read(*,*) N1
write(*,*) "Digite a nota da segunda avaliação"
read(*,*) N2
Np = (N1+N2)/2.0
write(*,*) "Np = ", Np
if (Np >= 7.0) then
  write(*,*) "O aluno ", nome, "está aprovado"
else if (Np < 7) then
  write(*,*) "Digite a nota do exame"
  read(*,*) E
  Nf = (Np+E)/2.0
  write(*,*) "Nf = ", Nf
  if (Nf >= 5.0) then
    write(*,*) "O aluno ",nome, "está aprovado"
  else
    write(*,*) "O aluno ",nome, "está reprovado"
  endif
end if


end program

! Algumas extruturas condicionais

! if (EB) comando

! if (EB == T) then
!    comandos
! end if

! if (EB1 == T) then
!   comandos1
! else if (EB2 == T) then
!   comandos2
!  else
!     comandos3
!end if
