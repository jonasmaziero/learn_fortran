 reset
 set terminal postscript color enhanced 'Helvetica' 24
 set output 'opt.eps'
 set contour base
 set pm3d map
 set cntrparam bspline
 set view map
 splot 'opt_f.dat' u 1:2:3 ls 1 w l nosurface notitle, \
       'opt_x.dat' u 1:2:(0) w lp ls 2 nocontour
