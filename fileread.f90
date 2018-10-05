program ler
implicit none
integer :: j, k
integer :: a(729)
real :: b(729)
open(unit=13,file="medidaetanol.dat",status="old")

do j = 1, 729
  read(13,*) a(j), b(j)
enddo

do j = 1, 10
  write(*,*) a(j), b(j)
enddo

end program
