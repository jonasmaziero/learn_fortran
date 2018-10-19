 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'derivada.eps'
 plot [0:2*pi][-1.01:1.01] 'derivada.dat' u 1:2 w l,'' u 1:3 w p pt 1,'' u 1:4 w l,'' u 1:5 w p pt 2,'' u 1:6 w l,'' u 1:7 w p pt 3,'' u 1:8 w l,'' u 1:9 w p pt 4
