 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'p2f.eps'
 set xlabel 'T'
 plot 'p2fk000001.dat' u 1:2 w l t 'k=0.00001', 'p2fk01.dat' u 1:2 w l t 'k=0.1', \
 'p2fk05.dat' u 1:2 w l t 'k=0.5', 'p2fk1.dat' u 1:2 w l t 'k=1', \
 'p2fk2.dat' u 1:2 w l t 'k=2', 'p2fk4.dat' u 1:2 w l t 'k=4', \
 'p2fk6.dat' u 1:2 w l t 'k=6','p2fk20.dat' u 1:2 w l t 'k=20'
