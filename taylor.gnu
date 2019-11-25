 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'taylor.eps'
 plot [-2*pi:2*pi][-1.01:1.01] 'taylor.dat' u 1:2 w l t 'f','' u 1:3 w p pt 1 t 'ordem 1', '' u 1:4 w p pt 1 t '2','' u 1:5 w p pt 1 t '3'
