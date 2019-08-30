!-------------------------------------------------------------------------------
program char
  write(*,*) achar(74)//achar(79)//achar(78)//achar(65)//achar(83)
  !call caracteres()
  !call strings()
end program
!-------------------------------------------------------------------------------
subroutine caracteres()
  implicit none
  character :: ch
  integer :: j, k

  !do j = 50, 60
  !  ch = achar(j)  ! transforma o inteiro no caracter correspondente
  !  write(*,*) j, ch
  !  k = ichar(ch)  ! transforma o caracter no inteiro correspondente
  !  write(*,*) ch, k
  !end do

  write(*,*) (k, k=65,90)
  write(*,*) (achar(k), k=65,90)  ! implied do

end subroutine
!-------------------------------------------------------------------------------
subroutine strings()
  implicit none
  character(len=10) :: nome, sobrenome, temp  ! strings são sequências de caracteres
  character(20) :: nome_sobrenome
  integer :: j

  write(*,*) "comprimentos = ",len(nome), len(nome_sobrenome)
  ! para saber quantos caractéres tem cada string

  nome = "Jonas"
  sobrenome = "Maziero"
  write(*,*) "Nome = ",nome, "Sobrenome = ",sobrenome
  nome_sobrenome = nome//sobrenome
  write(*,*) "Nome e Sobrenome = ",nome_sobrenome
  nome_sobrenome = trim(nome)//" "//trim(sobrenome)
  ! concatenação de characteres
  ! trim(var) remove os caracters em branco de var
  write(*,*) "Nome e Sobrenome = ",nome_sobrenome
  write(*,*) "len(nome) = ",len(nome), ",","len(trim(nome)) = ", len(trim(nome))
  !len_trim(var) retorna o comprimento de nome depois que removemos os 'brancos'

  write(*,*) nome(1:6), sobrenome(4:7)  ! para acessar parte da string
  do j = 1, 10
    write(*,*) nome(j:j) ! para acessar cada elemento da string
  end do

end subroutine
!-------------------------------------------------------------------------------
! Alocação dinâmica de memória para strings
!character(len=:),allocatable :: str(:)
!	integer :: clen, nbLines
!	allocate(character(clen) :: str(nbLines))
!------------------------------------------------------------------------------------------------------------------------------------
