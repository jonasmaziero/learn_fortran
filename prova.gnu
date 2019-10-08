 reset
 set terminal postscript enhanced 'Helvetica' 24
 set output 'prova.eps'
 set yrange [0:1]
 plot 'prova.dat' u 1:2 w l t 'MB', '' u 1:3 w l t 'BE', '' u 1:4 w l t 'FD'
