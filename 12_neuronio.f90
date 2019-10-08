!-------------------------------------------------------------------------------
program neuronio
  implicit none
  real :: z, sigmoid, relu, lrelu
  open(unit=13, file='neuronio.dat', status='unknown')

  z = -6.0
  do while (z < 6.0)
    z = z + 0.01
    write(13,*) z, sigmoid(z), relu(z)
  enddo

  open(unit=14, file='neuronio.gnu')
  write(14,*) "reset"
  write(14,*) "set terminal postscript enhanced 'Helvetica' 24"
  write(14,*) "set output 'neuronio.eps'"
  write(14,*) "set xlabel 'z'"
  write(14,*) "plot 'neuronio.dat' u 1:2 w l, '' u 1:3 w l"
  close(14)
  call system("gnuplot neuronio.gnu")
  !call system("evince neuronio.eps &")
  call system("open -a skim neuronio.eps &")

end
!-------------------------------------------------------------------------------
real function sigmoid(z)
  implicit none
  real :: z

  sigmoid = 1.0/(1.0+exp(-z))

end
!-------------------------------------------------------------------------------
real function relu(z)
  implicit none
  real :: z

  relu = 0.0
  if (z > 0.0) relu = z

end
!-------------------------------------------------------------------------------
