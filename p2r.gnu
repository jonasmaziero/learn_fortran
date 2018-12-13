 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'p2r.eps'
 set xlabel 'k'
 set ylabel 'R'
 plot 'p2r.dat' u 1:2 w l notitle
