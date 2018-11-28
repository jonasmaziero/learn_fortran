 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'sed_euler.eps'
 plot 'sed_euler.dat' u 1:2 w p, '' u 1:3 w l
