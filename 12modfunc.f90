!-----------------------------------------------------------------------------------------------------------------------------------
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
