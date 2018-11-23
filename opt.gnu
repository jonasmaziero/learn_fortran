 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'opt.eps'
 set view map
 set pm3d at b map
 set dgrid3d 200,200,2
 splot 'opt_f.dat' u 1:2:3 ls 1 w l nosurface notitle, \
       'opt_x.dat' u 1:2:(0) w lp ls 2 nocontour notitle
