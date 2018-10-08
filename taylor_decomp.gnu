 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'taylor_decomp.eps'
 plot [-2*pi:2*pi][-1.1:1.1] 'taylor_decomp.dat' u 1:2 w l,'' u 1:3 w p pt 1,'' u 1:4 w p pt 1,'' u 1:5 w p pt 1,'' u 1:6 w p pt 1,'' u 1:7 w p pt 1,'' u 1:8 w p pt 1,'' u 1:9 w p pt 1
