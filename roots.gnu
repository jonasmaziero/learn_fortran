 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'roots.eps'
 plot 'roots.dat' u 1:2 w lp, 0
