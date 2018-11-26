 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'interpol.eps'
 plot [:][0:1] 'dado.dat' u 1:2 w p ps 2 pt 5, 'interpol.dat' u 1:2 w l lt 3, \
               'interpol.dat' u 1:3 w l lt 0
