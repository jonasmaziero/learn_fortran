!-----------------------------------------------------------------------------------------------------------------------------------
module par
  implicit none
  integer, parameter :: N = 6
  real(8) :: x(N), f(N)
  integer, parameter :: Nmax = 100
  real(8), parameter :: del = 0.001
  real(8), parameter :: err = 0.00001
end module par
!-----------------------------------------------------------------------------------------------------------------------------------
module ffs
  implicit none

  contains

    function y(M,q,x)
      implicit none
      real(8) :: y
      integer :: M
      real(8) :: q(M), x
      !y = q(1) + q(2)*x + q(3)*x**2 + q(4)*x**3
      !y = q(1) + q(2)*x + q(3)*x**2
      y = q(1) + q(2)*x
    end function

end module ffs
!-----------------------------------------------------------------------------------------------------------------------------------
