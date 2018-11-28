 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'projetil.eps'
 set title 'Projetil com forca proporcional a velocidade'
 set xrange [0:20]
 set yrange [0:]
 set xlabel 'x'
 set ylabel 'y'
 plot 'projetilk0.dat' u 2:3 w l t 'k=0', 'projetilk01.dat' u 2:3 w l t 'k=0.1', \
 'projetilk025.dat' u 2:3 w l t 'k=0.25', 'projetilk05.dat' u 2:3 w l t 'k=0.5', \
 'projetilk1.dat' u 2:3 w l t 'k=1', 'projetilk2.dat' u 2:3 w l t 'k=2'
