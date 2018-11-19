 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set xlabel 'x'
 set ylabel 'y'
 set zrange [-6:6]
 set output 'der_par.eps'
 splot 'der_par.dat' u 1:2:3, '' u 1:2:4
