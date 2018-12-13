!------------------------------------------------------------------------------------------------------------------------------------
function fat(n)  ! Usado junto com o programa 04files.f90, como exemplo de compilação simultânea de vários programas
implicit none
integer(8) :: fat
integer :: n, j

  if (n == 0 .or. n == 1) then
    fat = 1
  else if (n > 1) then
    fat = 1
    do j = 2, n
      fat = fat*j
    end do
  end if

end function fat
!------------------------------------------------------------------------------------------------------------------------------------
