!------------------------------------------------------------------------------------------------------------------------------------
program rand
!  implicit none
  real :: dp(2), dpd(6)

!  call init_gnu() ! initicializa o gerador de números aleatórios
  !call random_real()
  !call random_integer()
  !call simula_moeda(10**6,dp); write(*,*) dp

  call simula_dado(10**2,dpd); write(*,*) dpd

end program
!------------------------------------------------------------------------------------------------------------------------------------
subroutine simula_moeda(N,dp)
  implicit none
  integer :: N  ! No. de lançamentos da moeda
  real(8) :: rn(N)
  real :: dp(2)
  integer :: j

  call rand_real(N, rn)
  dp = 0
  do j = 1, N
    if (rn(j) < 0.5) then
      dp(1) = dp(1) + 1
    else
      dp(2) = dp(2) + 1
    endif
  enddo
  dp = dp/N

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
subroutine simula_dado(N,dp)
  implicit none
  integer :: N  ! No. de lançamentos do dado
  real(8) :: rn(N), fa
  real :: dp(6)
  integer :: j, k

  fa = 1.0/6.0
  call rand_real(N, rn)
  dp = 0
  do j = 1, N
    do k = 1, 6
      if ((rn(j) >= (k-1)*fa) .and. (rn(j) < k*fa)) dp(k) = dp(k) + 1
    enddo
  enddo
  dp = dp/N
  open(unit=13,file='dado.dat',status='unknown')
  do j = 1, 6; write(13,*) j, dp(j);  enddo

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
subroutine random_real()
  implicit none
  integer, parameter :: d = 10
  real(8) :: rn(d)
  integer :: j

  do j = 1, d
    call rand_real(d, rn)
    write(*,*) rn(j)
  enddo

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
subroutine random_integer()
  implicit none
  integer, parameter :: d = 10
  integer :: rni(d)
  integer :: j, minn, maxx

  minn = 0;  maxx = 10  ! intervalo para os inteiros
  do j = 1, d
    call rand_int(minn, maxx, d, rni)
    write(*,*) rni(j)
  enddo

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
!                                                    Gnu's RNG
subroutine init_gnu()  ! Initialization for the GNU RNG RANDOM_NUMBER()
IMPLICIT NONE
INTEGER, ALLOCATABLE :: seed(:)
INTEGER :: i, n, un, istat, dt(8), pid, t(2), s
INTEGER(8) :: count, tms

CALL RANDOM_SEED(size = n)
ALLOCATE(seed(n))
! First try if the OS provides a random number generator
OPEN(newunit=un, file="/dev/urandom", access="stream", form="unformatted", action="read", status="old", iostat=istat)
IF (istat == 0) THEN
  read(un) seed
  close(un)
ELSE
  ! Fallback to XOR:ing the current time and pid. The PID is useful in case one launches multiple instances of the same program in parallel.
  CALL SYSTEM_CLOCK(count)
  IF (count /= 0) THEN
    t = TRANSFER(count, t)
  ELSE
    CALL DATE_AND_TIME(values=dt)
    tms = (dt(1) - 1970) * 365_8 * 24 * 60 * 60 * 1000 + dt(2) * 31_8 * 24 * 60 * 60 * 1000 + dt(3) * 24 * 60 * 60 * 60 * 1000 &
                       + dt(5) * 60 * 60 * 1000 + dt(6) * 60 * 1000 + dt(7) * 1000 + dt(8)
    t = TRANSFER(tms, t)
  ENDIF
  s = ieor(t(1), t(2))
  pid = getpid() + 1099279 ! Add a prime
  s = ieor(s, pid)
  IF (n >= 3) THEN
     seed(1) = t(1) + 36269
     seed(2) = t(2) + 72551
     seed(3) = pid
     IF (n > 3) THEN
        seed(4:) = s + 37 * (/ (i, i = 0, n - 4) /)
     ENDIF
  ELSE
     seed = s + 37 * (/ (i, i = 0, n - 1 ) /)
  ENDIF
ENDIF
CALL RANDOM_SEED(put=seed)

end
!------------------------------------------------------------------------------------------------------------------------------------
subroutine rand_real(d, rn) ! Calls the Gnu "standard" random number generator.
  !See https://gcc.gnu.org/onlinedocs/gfortran/RANDOM_005fNUMBER.html
  ! The runtime-library implements George Marsaglia's KISS (Keep It Simple Stupid) random number generator.
  implicit none
  integer :: d  ! Dimension of the vector of random numbers
  real(8) :: rn(1:d) ! Vector whose components are random numbers uniformly distributed in [0,1)

  call random_number(rn)

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
subroutine rand_int(minn,maxx,d,rn)
  implicit none
  integer :: d, minn, maxx, j
  integer :: rn(d)
  real(8) :: rrn(d)

  call rand_real(d, rrn)
  do j = 1, d
    rn(j) = minn + floor((maxx+1-minn)*rrn(j)) ! floor(x) retorna o maior inteiro <= x
  enddo

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
