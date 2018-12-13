 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'euler.eps'
 plot 'euler.dat' u 1:2 w p pt 1, 0.5*x**2 w p pt 2
