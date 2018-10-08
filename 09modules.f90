module cte
implicit none
  real(8), parameter :: pi = 4.d0*datan(1.d0)
  real(8), parameter :: c = 299792458.d0  ! velocidade da luz (m/s)
  real(8), parameter :: e = 1.6021766208d0/1.d19  ! carga elétrica do próton (C)
  real(8), parameter :: eps0 = 8.854187817d0/1.d12  ! permissividade do vácuo (F/m)
  real(8), parameter :: mu0 = (4.d0*pi)/1.d7  ! permeabilidade do vácuo (Tm/A)
  real(8), parameter :: G = 6.67408d0/1.d11  ! constante gravitacional (Nm^2/kg^2)
  real(8), parameter :: h = 6.626070040d0/1.d34 ! Constante de Plank (Js)
  real(8), parameter :: hb = h/(2.d0*pi)
  real(8), parameter :: kB = 1.38064852d0/1.d23 ! Constante de Boltzmann (J/K)
  real(8), parameter :: alpha = (e**2.d0)/(4.d0*pi*eps0*hb*c)  ! constante de estrutura fina
end module cte
