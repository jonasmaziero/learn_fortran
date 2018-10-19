 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'euler.eps'
 plot 'euler.dat' u 1:2 w lp
