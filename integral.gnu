 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'integral.eps'
 plot [0:4*pi][-1.01:1.01] 'integral.dat' u 1:2 w p, '' u 1:3 w l
