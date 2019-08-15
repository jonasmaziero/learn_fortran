!-------------------------------------------------------------------------------
! Os primeiros comandos a serem executados são os do programa principal
program principal
  implicit none  ! te obriga declarar todas as variaveis
  real :: x,y, ssen, scos, fsen, ffsen

  x = 3.1415/2.0
  y = 3.1415
  call subsen(x,ssen,scos) ! assim "chamamos subrotinas"
  !ffsen = fsen(x) ! assim "chamamos" funções
  write(*,*)  ssen, scos, fsen(x), fsen(y)

end program

!-------------------------------------------------------------------------------
! Subrotinas podem receber e retornar vários argumentos
subroutine subsen(x,ssen,scos)
  real :: x, ssen, scos

  ssen = sin(x)
  scos = cos(x)

end subroutine
!-------------------------------------------------------------------------------
! Funções podem receber vários argumentos mas retornam um único valor
function fsen(xx)
  real :: xx, fsen

  fsen = sin(xx)

end function
!-------------------------------------------------------------------------------
