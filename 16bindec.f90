!-----------------------------------------------------------------------------------------------------------------------------------
program binarydecimal
  implicit none
  integer, parameter :: nd = 3
  integer :: bin(3), dec

!  bin = (/0,0,1/); write(*,*) bin
!  call bin2dec(bin, nd, dec); write(*,*) dec
!  call dec2bin(dec, nd, bin); write(*,*) bin

!end program
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine bin2dec(bin, nd, dec)
  implicit none
  integer :: nd  ! No. de digitos
  integer :: bin(1:nd)
  integer :: dec
  integer :: i

  dec = 0
  do i = 1, nd
    !dec = dec + bin(i)*2**(nd-i)
    if (bin(i) == 1) dec = dec + 2**(nd-i) ! mais eficiente
  enddo

end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine dec2bin(dec, nd, bin)
  implicit none
  integer :: dec
  integer :: nd  ! No. de digitos a serem usados
  integer :: bin(1:nd)
  integer :: i

  bin = 0
  do i = 1, nd
    if (2**(nd-i) <= dec) then ;   bin(i) = 1 ;   dec = dec - 2**(nd-i) ;   end if
  end do

end subroutine
!-----------------------------------------------------------------------------------------------------------------------------------
