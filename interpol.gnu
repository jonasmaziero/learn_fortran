 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'interpol.eps'
 plot [0.9:6.1][0:0.5] 'dado.dat' u 1:2 w p ps 2 pt 5 t 'dp dado',\
 'interpol.dat' u 1:2 w l lt 3 t 'lagrange',\
 'interpol.dat' u 1:3 w l lt 0 t 'linear',\
 'interpol.dat' u 1:4 w l lt 7 t 'cubic spline'
