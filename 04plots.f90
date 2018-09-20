!------------------------------------------------------------------------------------------------------------------------------------
program plots
  implicit none
  call plot1D()
end
!------------------------------------------------------------------------------------------------------------------------------------
subroutine plot1D()  ! Basic 1D plot with gnuplot from a data file
  implicit none
  open(unit=13, file="1Dplot.gnu")
  write(13,*) "reset"
  write(13,*) "set terminal postscript enhanced 'Helvetica' 24"
  write(13,*) "set output '1Dplot.eps'"
  write(13,*) "set xrange [0:2*pi]"
  write(13,*) "set yrange [-1.01:1.01]"
  write(13,*) "plot '1Dplot.dat' w l"
  close(13)
  call system("gnuplot 1Dplot.gnu")
  call system("evince 1Dplot.eps &")
end
!------------------------------------------------------------------------------------------------------------------------------------


!------------------------------------------------------------------------------------------------------------------------------------
