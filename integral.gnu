 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'integral.eps'
 plot [:][:] 'integral.dat' u 1:2 w p, '' u 1:3 w l
