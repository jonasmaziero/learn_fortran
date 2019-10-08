 reset
 set terminal postscript enhanced 'Helvetica' 24
 set output 'neuronio.eps'
 set xlabel 'z'
 plot 'neuronio.dat' u 1:2 w l, '' u 1:3 w l, '' u 1:4 w l
