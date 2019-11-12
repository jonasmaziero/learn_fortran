!-------------------------------------------------------------------------------
! Para compilar use:
! gfortran -c 19_modf.f90
! Isso criará o objeto 19_modf.o, que já é assembly. Depois usa compilando com
! gfortran main.f90 19_modf.o
! Note que 19_modf.f90 não será mais compilado, só será usado.
module modf
  implicit none

contains

  function func(x)
    implicit none
    real(8) :: x, func
      func = x**3.d0 + 3.d0*x**2.d0 - x - 4.d0
  end function

end module
!-------------------------------------------------------------------------------
