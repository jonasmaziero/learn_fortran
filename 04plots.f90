!------------------------------------------------------------------------------------------------------------------------------------
program plots
  implicit none
  character(len=10) :: arq
  real :: th, dth, om, dom
  real, parameter :: pi = 4.0*atan(1.0)
  real :: ti,tf

  !call plot1D()

  arq = "'sen.dat'"
  dth = pi/100.0
  dom = 0.1
  om = -dom
  doo: do
    open(unit=13,file="sen.dat",status="unknown")
    om = om + dom
    th = -dth
    dot: do
      th = th + dth
      write(13,*) th, sin(om*th)
      if (th > 4.0*pi) exit dot
    end do dot
    call plot2d(arq)
    if (om > 2) exit doo
    close(13)
    call cpu_time(ti)
    dott: do
      call cpu_time(tf)
      if ((tf - ti) > 1) exit dott
    end do dott
  end do doo

end program
!------------------------------------------------------------------------------------------------------------------------------------
subroutine plot1D()  ! Basic 1D plot with gnuplot from a data file
  implicit none
  open(unit=13, file="1Dplot.gnu")
  write(13,*) "reset"
  write(13,*) "set terminal postscript enhanced 'Helvetica' 24"
  write(13,*) "set output '1Dplot.eps'"
  !write(13,*) "set xrange [0:2*pi]"
  !write(13,*) "set yrange [-1.01:1.01]"
  write(13,*) "plot 'sen.dat' w l"
  close(13)
  call system("gnuplot 1Dplot.gnu")
  call system("evince 1Dplot.eps &")
end
!------------------------------------------------------------------------------------------------------------------------------------
subroutine plot2d(arq)
  implicit none
  character(len=10) :: arq  ! nome do arquivo. Ex: 'fat.dat'
  open(unit=13, file="plot2d.gnu", status="unknown")
  write(13,*) "reset"
  write(13,*) "set terminal postscript enhanced 'Helvetica' 24"
  write(13,*) "set output 'plot2d.eps'"
  write(13,*) "plot "//arq
  close(13)
  call system("gnuplot plot2d.gnu")
  !call system("evince plot2d.eps &")
  call system("open -a skim plot2d.eps &")
end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
