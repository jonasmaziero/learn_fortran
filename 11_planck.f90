!-------------------------------------------------------------------------------
program planck
  implicit none
  real(8) :: L, Lm, T1, T2, T3, B
  real(16) :: Bn
  open(unit=13, file='planck.dat',status='unknown')

  T1 = 3.d3; T2 = 4.d3; T3 = 5.d3

  L = 0.d0
  do while (L < 5.d0)
    L = L + 0.05d0
    Lm = L*1.d3
    !write(13,*)  L, B(Lm,T1), B(Lm,T2), B(Lm,T3)
  end do

  open(unit=14, file='planck.gnu')
  write(14,*) "reset"
  write(14,*) "set terminal postscript enhanced 'Helvetica' 24"
  write(14,*) "set output 'planck.eps'"
  write(14,*) "plot 'planck.dat' u 1:2 w l, '' u 1:3 w l, '' u 1:4 w l"
  close(14)
  call system("gnuplot planck.gnu")
  !call system("evince plot2d.eps &")
  call system("open -a skim planck.eps &")

end

!-------------------------------------------------------------------------------
real(8) function B(L,T)
  implicit none
  ! L == comprimento de onda em nm; T == temperatora em graus Kelvin
  !real(8), parameter :: c = 3.d0*1.d8, h = 6.62d0*1.d-34, k = 1.38d0*1.d-23
  real(8) :: L, T

  !B = ((2.d0*h*c**2.d0)/(L**5.d0))*(1.d0/(exp((h*c)/(k*T*L))-1.d0))
  ! expressão simplificada (para evitar problemas de divisão por zero)
  B = ((2.d0*6.626d0*9*1.d15)/(L**5.d0))*(1.d0/(exp((6.626d0*3.d0*1.d6)/(1.38d0*L*T))-1.d0))

end
!-------------------------------------------------------------------------------
