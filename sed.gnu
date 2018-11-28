 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'sed.eps'
 plot 'sed.dat' u 1:2 w p, '' u 1:3 w l
