!-----------------------------------------------------------------------------------------
program p2
  !call sf()
  call alcance() ! gfortran P2.f90 P2mod.o 12roots.f90 13modf.o 09derivada.f90 04fatorial.f90 12modfunc.o
end program
!-----------------------------------------------------------------------------------------
subroutine sf() ! para estudar a função f
  use p2mod
  implicit none
  real(8) :: f, T
  real(8), parameter :: del = 0.05
  open(unit=13,file='p2fk4.dat',status='unknown')

  ! gráficos da função
  k = 4  ! k não pode ser nulo
  T = -del
  do
    T = T + del
    write(13,*) T, f(T)
    if (T > 5.d0) exit
  enddo
  close(13)
  !stop
  open(unit=14,file="p2f.gnu",status="unknown")
  write(14,*) "reset"
  write(14,*) "set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*) "set output 'p2f.eps'"
  write(14,*) "set xlabel 'T'"
  write(14,*) "plot 'p2fk000001.dat' u 1:2 w l t 'k=0.00001', 'p2fk01.dat' u 1:2 w l t 'k=0.1', \"
  write(14,*) "'p2fk05.dat' u 1:2 w l t 'k=0.5', 'p2fk1.dat' u 1:2 w l t 'k=1', \"
  write(14,*) "'p2fk2.dat' u 1:2 w l t 'k=2', 'p2fk4.dat' u 1:2 w l t 'k=4'"
  close(14)
  call system("gnuplot p2f.gnu")
  !call system("evince p2f.eps&")
  call system("open -a skim p2f.eps&");  read(*,*);  call system("sh quit.sh")

end subroutine
!-----------------------------------------------------------------------------------------
subroutine alcance()
  use p2mod
  implicit none
  integer :: Er, Nm
  real(8), external :: f
  real(8) :: T, R, Te, Td, err
  real(8), parameter :: del = 0.05
  open(unit=13,file='p2r.dat',status='unknown')

  Te = 0.1;  Td = 6;  err = 0.001;  Nm = 10**3
  k = 0.01-del
  do
    k = k + del
    call bissection(f,Te,Td,err,Nm,T,Er)
    R = (v0x/k)*(1.d0-dexp(-k*T))
    write(13,*) k, R
    write(*,*) k, R
    if (k > 10.d0) exit
  end do
  close(13)
  open(unit=14,file="p2r.gnu",status="unknown")
  write(14,*) "reset"
  write(14,*) "set terminal postscript color enhanced 'Helvetica' 24"
  write(14,*) "set output 'p2r.eps'"
  write(14,*) "set xlabel 'k'"
  write(14,*) "set ylabel 'R'"
  write(14,*) "plot 'p2r.dat' u 1:2 w l notitle"
  close(14)
  call system("gnuplot p2r.gnu")
  !call system("evince p2r.eps&")
  call system("open -a skim p2r.eps&");  read(*,*);  call system("sh quit.sh")

end subroutine
!-----------------------------------------------------------------------------------------
function f(T)
  use p2mod
  implicit none
  real(8) :: f, T
  f = (1.d0-dexp(-k*T))*((k*v0y+g)/(g*k)) - T
end function
!-----------------------------------------------------------------------------------------
