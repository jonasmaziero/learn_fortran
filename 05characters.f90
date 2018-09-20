!------------------------------------------------------------------------------------------------------------------------------------
program char
  !call caracteres_lista()
  call strings()
end program
!------------------------------------------------------------------------------------------------------------------------------------
subroutine caracteres_lista()
  implicit none
  character:: ch
  integer:: j, k

  do j = 33, 126
    ch = achar(j)  ! transforma o inteiro no caracter correspondente
    write(*,*) j, ch
    k = ichar(ch)  ! transforma o caracter no inteiro correspondente
    write(*,*) ch, k
  end do

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
subroutine strings()
  implicit none
  character(len=10) :: nome, sobrenome  ! strings são sequências de caracteres, como vetores
  character(20) :: nome_sobrenome
  integer :: j

  nome = "Jonas"
  sobrenome = "Maziero"
!  write(*,*) nome, sobrenome
  nome_sobrenome = nome//sobrenome  ! concatenação de characteres
!  write(*,*) nome_sobrenome
!  write(*,*) len(nome), len(nome_sobrenome)
!  do j = 1, 10
!    write(*,*) nome(j:j)
!  end do
!  write(*,*)"---------------------------------------"
!  do j = 1, 20
!    write(*,*) nome_sobrenome(j:j)
!  end do
  write(*,*) len("Jonas"), len("Maziero")
  write(*,*) nome(1:6), sobrenome(1:7)
end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
