!-------------------------------------------------------------------------------
program prova
implicit none
real :: E,kT, maxwell_boltzmann, bose_einstein, fermi_dirac
open(unit=13, file='prova.dat',status='unknown')

call impares(20)
stop

kT = 1.0
E = 0.00001
do while(E < 5.0)
  write(13,*) E, maxwell_boltzmann(E,kT), bose_einstein(E,kT), fermi_dirac(E,kT)
  !write(*,*) E, maxwell_boltzmann(E,kT), bose_einstein(E,kT), fermi_dirac(E,kT)
  E = E + 0.01
enddo

open(unit=14, file='prova.gnu')
write(14,*) "reset"
write(14,*) "set terminal postscript enhanced 'Helvetica' 24"
write(14,*) "set output 'prova.eps'"
write(14,*) "set yrange [0:1]"
write(14,*) "plot 'prova.dat' u 1:2 w l t 'MB', '' u 1:3 w l t 'BE', '' u 1:4 w l t 'FD'"
close(14)
call system("gnuplot prova.gnu")
!call system("evince plot2d.eps &")
call system("open -a skim prova.eps &")

end
!-------------------------------------------------------------------------------
real function maxwell_boltzmann(E,kT)
  implicit none
  real :: E,kT

  maxwell_boltzmann = 1.0/exp(E/kT)

end
!-------------------------------------------------------------------------------
real function bose_einstein(E,kT)
  implicit none
  real :: E,kT

  bose_einstein = 1.0/(exp(E/kT)-1.0)

end
!-------------------------------------------------------------------------------
real function fermi_dirac(E,kT)
  implicit none
  real :: E,kT

  fermi_dirac = 1.0/(exp(E/kT)+1.0)

end
!-------------------------------------------------------------------------------
subroutine impares(N)
  implicit none
  integer :: N,j

  write(*,*) 1
  do j = 2, N
    if (mod(j,2) /= 0) write(*,*) j
  enddo

end
!-------------------------------------------------------------------------------
