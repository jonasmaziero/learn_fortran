 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'integral.eps'
 splot 'integral.dat' u 1:2:4 w p pt 5 ps 0.5, sin(x)*(1-cos(y))
