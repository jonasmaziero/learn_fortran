# run this file using "sh script.sh" no terminal
echo "primeiro script está funcionando"
gfortran files.f90
./a.out
gfortran plots.f90
./a.out
rm *.dat
