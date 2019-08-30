!-------------------------------------------------------------------------------
function fat(n)
implicit none
integer :: fat
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
!-------------------------------------------------------------------------------
