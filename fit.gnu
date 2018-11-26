 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'fit.eps'
 plot [:][0:1] 'dado.dat' u 1:2 w p ps 2 pt 5, 'fit.dat' u 1:2 w l lt 3 lw 3, \
               'fit.dat' u 1:3 w l lt 5 lw 3
