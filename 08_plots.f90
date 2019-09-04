!-------------------------------------------------------------------------------
program plots
  implicit none
  character(len=20) :: arq
  real :: th, dth, om, dom = 0.1
  real, parameter :: pi = 4.0*atan(1.0) ! não pode mudar valor
  real :: ti,tf

  arq = "'sen.dat'" ! nome do arquivo de dados
  dth = pi/100.0
  !dom = 0.1
  om = 0.1-dom
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
    call cpu_time(ti) ! retorna o tempo de vida da cpu
    dott: do
      call cpu_time(tf)
      if ((tf - ti) > 1) exit dott
    end do dott
  end do doo

  !call plot2D(arq)

  ! Exercício: Faça um exercício desse tipo usando o problema do projétil
  !            e fazendo um gráfico 3D variando v0

end program
!-------------------------------------------------------------------------------
subroutine plot2D(arq)  ! Basic 2D plot with gnuplot from a data file
  implicit none
  character(len=10) :: arq  ! nome do arquivo. Ex: 'fat.dat'
  open(unit=13, file="plot2d.gnu")

  write(13,*) "reset"
  write(13,*) "set terminal postscript enhanced 'Helvetica' 24"
  !write(13,*) "set terminal postscript portrait"
  write(13,*) "set output 'plot2d.eps'"
  !write(13,*) "set xrange [0:2*pi]"
  !write(13,*) "set yrange [-1.01:1.01]"
  write(13,*) "plot "//arq
  close(13)
  call system("gnuplot plot2d.gnu")
  !call system("evince plot2d.eps &")
  call system("open -a skim plot2d.eps &")
end
!-------------------------------------------------------------------------------
subroutine plot3d(arq)
  implicit none
  character(len=10) :: arq
  open(unit=13, file="plot3d.gnu", status="unknown")
  write(13,*) "reset"
  write(13,*) "set terminal postscript enhanced 'Helvetica' 24"
  write(13,*) "set output 'plot3d.eps'"
  write(13,*) "plot "//arq
  close(13)
  call system("gnuplot plot3d.gnu")
  !call system("evince plot2d.eps &")
  call system("open -a skim plot3d.eps &")
end subroutine
!-------------------------------------------------------------------------------
