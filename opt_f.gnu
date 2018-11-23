 reset
 set term postscript eps enhanced color
 set output 'opt_f.eps'
 set hidden3d
 set dgrid3d 100,100 qnorm 2
 set view map
 splot 'opt_f.dat' u 1:2:3 palette notitle, \
       'opt_x.dat' u 1:2:(0)
