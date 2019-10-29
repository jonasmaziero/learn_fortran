!-------------------------------------------------------------------------------
module cte ! as variáveis declaradas no module não devem ser declaradas no programa que o usa
implicit none
  real(8), parameter :: pi = 4.d0*datan(1.d0)
  real(8), parameter :: c = 299792458.d0  ! velocidade da luz (m/s)
  real(8), parameter :: e = 1.6021766208d0/1.d19  ! carga elétrica do próton (C)
  real(8), parameter :: eps0 = 8.854187817d0/1.d12  ! permissividade do vácuo (F/m)
  real(8), parameter :: k = 1.d0/(4.d0*pi*eps0) ! constante elétrica (Nm^2/C^2)
  real(8), parameter :: mu0 = (4.d0*pi)/1.d7  ! permeabilidade do vácuo (Tm/A)
  real(8), parameter :: G = 6.67408d0/1.d11  ! constante gravitacional (Nm^2/kg^2)
  real(8), parameter :: h = 6.626070040d0/1.d34 ! Constante de Plank (Js)
  real(8), parameter :: hb = h/(2.d0*pi)
  real(8), parameter :: kB = 1.38064852d0/1.d23 ! constante de Boltzmann (J/K)
  real(8), parameter :: alpha = (e**2.d0)/(4.d0*pi*eps0*hb*c)  ! constante de estrutura fina
  real(8), parameter :: me = 9.109382d0/1.d31 ! massa do elétron (kg)
  real(8), parameter :: mp = 1.672622d0/1.d27 ! massa do próton (kg)
  real(8), parameter :: mn = 1.674927d0/1.d27 ! massa do nêutron (kg)
  real(8), parameter :: u = 1.660539d0/1.d27 ! unidade de massa atômica (kg)
  real(8), parameter :: Na = 6.022142d0/1.d23 ! número de Avogrado
end module cte
!-------------------------------------------------------------------------------
