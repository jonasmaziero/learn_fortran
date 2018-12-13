!-----------------------------------------------------------------------------------------------------------------------------------
! Para compilar use:
! gfortran -c 12modfunc.f90
! Isso criará o objeto 12modfunc.o, que já é assembly. Depois usa compilando com
! gfortran main.f90 12modfunc.o
! Note que 12modfunc.f90 não será mais compilado, só será usado.
module modfunc
  implicit none

contains

  function func(x)
    implicit none
    real(8) :: x, func
      func = x**3.d0 + 3.d0*x**2.d0 - x - 4.d0
  end function

end module
!-----------------------------------------------------------------------------------------------------------------------------------
