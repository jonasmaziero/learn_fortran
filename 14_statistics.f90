!-------------------------------------------------------------------------------
include '13_random.f90'
!-------------------------------------------------------------------------------
program estatistica
  !call statistics()
  call composto()
end program
!-------------------------------------------------------------------------------
subroutine statistics()
  implicit none
  integer, parameter :: d = 6
  real :: dp(d), normalizacao, va(d), media, media2, desvio_padrao, mediana

  !dp = (/ 1.0/d, 1.0/d, 1.0/d, 1.0/d, 1.0/d, 1.0/d /)
  !dp(1) = 1.0/6.0;  dp(2) = dp(1); dp(3) = dp(1);  dp(4) = dp(1); dp(5) = dp(1);  dp(6) = dp(1)  ! dado perfeito, teórico
  call init_gnu();  call simula_dado(10**6,dp);  write(*,*) "dp = ", dp  ! dado real, simulado
  va = (/ 1, 2, 3, 4, 5, 6 /)  ! valores possíveis da variável aleatória
  write(*,*) "Soma das probabilidades = ", normalizacao(d, dp)
  write(*,*) "Média do dado = ", media(d, dp, va)
  write(*,*) "Média do quadrado do dado = ", media2(d, dp, va)
  write(*,*) "Desvio padrão do dado= ", desvio_padrao(d, dp, va)
  write(*,*) "Mediana do dado = ", mediana(d,dp,va)

end subroutine
!-------------------------------------------------------------------------------
real function normalizacao(d, dp)
  implicit none
  integer :: d, j
  real :: dp(d)

  normalizacao = 0
  do j = 1, d
    if (dp(j) < 0.0) write(*,*)'prob. negativa'
    normalizacao = normalizacao + dp(j)
  end do

end function
!-------------------------------------------------------------------------------
real function media(d, dp, va)
  implicit none
  integer :: d, j
  real :: dp(d), va(d)

  media = 0.0
  do j = 1, d
    media = media + va(j)*dp(j)
  end do

end function
!-------------------------------------------------------------------------------
real function media2(d, dp, va)
  implicit none
  integer :: d, j
  real :: dp(d), va(d)

  media2 = 0
  do j = 1, d
    media2 = media2 + (va(j)**2)*dp(j)
  end do

end function
!-------------------------------------------------------------------------------
real function desvio_padrao(d, dp, va)
  implicit none
  integer :: d, j
  real :: dp(d), va(d), media, media2

  desvio_padrao = sqrt(media2(d, dp, va) - (media(d, dp, va))**2)

end function
!-------------------------------------------------------------------------------
function mediana(d,dp,va)
  implicit none
  real :: mediana
  integer :: d, j
  real :: dp(d), va(d), sp

  sp = 0
  do j = 1, d
    sp = sp + dp(j)
    if (sp >= 0.5) then
      mediana = va(j)
      exit
    endif
  enddo

end function
!-------------------------------------------------------------------------------
subroutine composto() ! para testes
  implicit none
  integer, parameter :: Na = 2, Nb = 2
  real :: A(Na), B(Nb), pab(Na,Nb), covariancia, cov

  A = (/ -1, 1 /)!;  write(*,*) A
  B = (/ -1, 1 /)!;  write(*,*) B
  pab = reshape( (/ (/0.5, 0.0/), &  ! O símbolo & é para continuar um mesmo comando em várias linhas
                    (/0.0, 0.5/) /), (/2,2/), order=(/2,1/) )  ! order faz 'linha primeiro'
  write(*,*) 'Distribuição conjunta de probabilidades'
  call display_array(pab, Na, Nb)
  cov = covariancia(Na, Nb, pab, A, B)
  write(*,*) 'Cov(A,B) = ', cov

end subroutine
!-------------------------------------------------------------------------------
real function covariancia(Na, Nb, pab, A, B)
  implicit none
  integer :: Na, Nb, j, k
  real :: pab(Na,Nb), pa(Na), pb(Nb), A(Na), B(Nb), media, Am, Bm, ABm, media_AB

  call dpa(Na, Nb, pab, pa);  write(*,*) 'pa = ', pa
  Am = media(Na, pa, A);  write(*,*) 'Am = ', Am
  call dpb(Na, Nb, pab, pb);  write(*,*) 'pb = ', pb
  Bm = media(Nb, pb, B);  write(*,*) 'Bm = ', Bm
  ABm = media_AB(Na, Nb, pab, A, B);  write(*,*) 'ABm = ', ABm
  covariancia = ABm - Am*Bm

end function
!-------------------------------------------------------------------------------
real function media_AB(Na, Nb, pab, A, B)
  implicit none
  integer :: Na, Nb, j, k
  real :: pab(Na,Nb), A(Na), B(Nb)

  media_AB = 0
  do j = 1, Na
    do k = 1, Nb
      media_AB = media_AB + A(j)*B(k)*pab(j,k)
    enddo
  enddo

end function
!-------------------------------------------------------------------------------
subroutine dpa(Na, Nb, pab, pa)
  implicit none
  integer :: Na, Nb, j, k
  real :: pab(Na,Nb), pa(Na)

  do j = 1, Na
    pa(j) = 0
    do k = 1, Nb
      pa(j) = pa(j) + pab(j,k)
    enddo
  enddo

end subroutine
!-------------------------------------------------------------------------------
subroutine dpb(Na, Nb, pab, pb)
  implicit none
  integer :: Na, Nb, j, k
  real :: pab(Na,Nb), pb(Nb)

  do k = 1, Nb
    pb(k) = 0
    do j = 1, Na
      pb(k) = pb(k) + pab(j,k)
    enddo
  enddo

end subroutine
!-------------------------------------------------------------------------------
subroutine display_array(A, nl, nc)
  implicit none
  real :: A(nl,nc)
  integer :: nl, nc, j, k

  do j = 1, nl
    write(*,*) (A(j,k), k=1,nc)  ! implied d
  enddo

end subroutine
!-------------------------------------------------------------------------------
