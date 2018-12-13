!------------------------------------------------------------------------------------------------------------------------------------
program files
  !call arquivo()
  call plot1d()
  !call plot_sencos()
  !call plot3d()
  !call projetil()
  !call append_data()
end
!------------------------------------------------------------------------------------------------------------------------------------
subroutine arquivo()
  implicit none
  integer :: j, fat
  integer :: k, l
  real :: x
  open(unit = 13, file = "fat.dat", status = "unknown") ! Abre ou cria um arquivo chamado fat.dat na pasta atual
  open(unit = 14, file = "fat_log.dat", status = "unknown")

  do j = 1, 10
    write(13,*) j, fat(j), exp(real(j))
    write(14,*) j, log(real(fat(j))), log(exp(real(j))) ! escala logarítmica
  enddo
  close(13)
  close(14)
  ! Digite "gnuplot" no terminal. Use
  ! plot "fat.dat" u 1:2 w lp, "" u 1:3 w lp
  ! plot "fat_log.dat" u 1:2 w lp, "" u 1:3 w lp
  ! Para sair do gnuplot, digite exit e tecle Enter.

  ! Vamos ler dados do arquivo criado e escrever na tela
  open(unit = 13, file = "fat.dat", status = "unknown")
  do j = 1, 10
    read(13,*) k, l, x
    write(*,*) k, l, x
  enddo
  close(13)

end
!------------------------------------------------------------------------------------------------------------------------------------
subroutine plot1d()
  implicit none
  real :: th, dth, pi
  open(unit=13,file="sen.dat",status="unknown")

  pi = 4.0*atan(1.0)
  dth = pi/100.0
  th = -dth
  do
    th = th + dth
    write(13,*) th, sin(th)
    if (th > 2*pi) exit
  end do

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
subroutine plot_sencos()
  implicit none
  real :: pi, th, dth
  real :: x, y, z
  open(unit=13,file="sen_cos.dat",status="unknown")

  pi = 4.0*atan(1.0)
  dth = pi/100.0
  th = -2.0*pi
  do
    th = th + dth
    write(13,*) th, sin(th), cos(th)
    if (th > 2.0*pi) exit
  end do

  close(13)
  open(unit=13,file="sen_cos.dat",status="unknown")
  th = -2.0*pi
  do
    th = th + dth
    read(13,*) x, y, z
    write(*,*) z, y, z
    if (th > 2.0*pi) exit
  end do
  close(13)

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
subroutine plot3d()
  implicit none
  real :: th, dth, ph, dph, pi
  open(unit=13,file="sencos.dat",status="unknown")

  pi = 4.0*atan(1.0)
  dth = pi/100.0
  dph = dth
  th = -dth
  do1: do
    th = th + dth
    ph = -dph
    do2: do
      ph = ph + dph
      write(13,*) th, ph, sin(th)*cos(ph)
      if (ph > 2*pi) exit do2
    enddo do2
    if (th > 2*pi) exit do1
  end do do1

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
subroutine projetil()
  implicit none
  real :: x, dx, th0, dth0, pi, v0, trajetoria
  open(unit=13,file="trajetoria.dat",status="unknown")

  v0 = 10.0
  pi = 4.0*atan(1.0)
  dth0 = pi/50.0
  dx = 0.01
  th0 = 0.1*pi/8.0
  do1: do
    th0 = th0 + dth0
    x = 0.01
    do2: do
      x = x + dx
      write(13,*) th0, x, trajetoria(x, th0, v0)
      if (x > 10.0) exit do2
    enddo do2
    if (th0 > (3.5*pi/8.0)) exit do1
  end do do1
  close(13)
  write(*,*) pi/4.0
  !set ticslevel 0
  !set xlabel 'th0'
  !set ylabel 'x'
  !set zlabel 'y'
  !splot [0:][0:][0:] "trajetoria.dat" w p ps 0.1

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
real function trajetoria(x, th0, v0)
  implicit none
  real :: x, th0, v0, g

  g = 10.0
  trajetoria = x*tan(th0) - (g*x**2.0)/(2.0*(v0**2.0)*(cos(th0)**2.0))

end function
!------------------------------------------------------------------------------------------------------------------------------------
subroutine append_data()  ! Adicionando dados em um arquivo existente
  implicit none
  integer :: j
  open(unit=13, file='append.dat', status='unknown')

  do j = 1, 5
    write(13,"(I3)") j
  enddo
  close(13)

  open(unit=13, access='append', file='append.dat', status='old')
  do j = 0, 5
    if(j==0)then
      write(13,"(I3)")
      cycle
    endif
    write(13,"(I3)") j
  enddo
  close(13)

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
