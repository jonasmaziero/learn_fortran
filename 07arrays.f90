!------------------------------------------------------------------------------------------------------------------------------------
program arrrays
  call array1D()
  !call array2D()
end program
!------------------------------------------------------------------------------------------------------------------------------------
subroutine array1D()
  implicit none
  integer, parameter :: d = 2 ! constantes em Fortran
  real :: va(d), vb(d), pe, produto_escalar
  complex :: vac(d), vbc(d), pec

  write(*,*)"entre com um array de d = 2"
  read(*,*) va
  write(*,*) va
  !stop

  write(*,*)'arrays 1D'
  va = (/1,1/);  write(*,*) '|a> = ', va
  vb(1) = 1; vb(2) = 1;  write(*,*) '|b> = ', vb
  pe = produto_escalar(d, va, vb)
  write(*,*) "pe = ", pe

  write(*,*)'arrays 1D complexos'
  vac(1) = 1.0; vac(2) = (0.0,1.0);  write(*,*) '|ac> = ', vac
  vbc(1) = 1.0; vbc(2) = (0.0,1.0);  write(*,*) '|bc> = ', vbc
  call produto_escalar_c(d, vac, vbc, pec)
  write(*,*) "pec = ", pec
  write(*,"(A10,1F10.5)") "Re(pec) = ", real(pec)
  write(*,"(A10,1F8.3)") "Im(pec) = ", aimag(pec)

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
real function produto_escalar(d, A, B)
  implicit none
  integer :: d, j
  real :: A(d), B(d), pe

  produto_escalar = 0
  do j = 1, d
    produto_escalar = produto_escalar + A(j)*B(j)
  end do

end function
!------------------------------------------------------------------------------------------------------------------------------------
subroutine produto_escalar_c(d, A, B, pe)
  implicit none
  integer :: d, j
  complex :: A(d), B(d), pe

  pe = 0
  do j = 1, d
    pe = pe + conjg(A(j))*B(j)
  end do

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
subroutine array2D()
  implicit none
  integer, parameter :: d = 2
  real :: A(d,d), B(d,d), AB(d,d), trace
  complex :: psi(d), sigma(d,d), sanduiche

  write(*,*)'arrays 2D'
  A = reshape((/(/1,0/),(/0,1/)/), (/2,2/), order=(/2,1/))
  write(*,*)'A'
  call display_array(A, d, d)
  B(1,1) = 1;  B(1,2) = 2;  B(2,1) = 3;  B(2,2) = 4
  write(*,*)'B'
  call display_array(B, d, d)
  call produto_matricial(d, A, B, AB)
  write(*,*)'AB'
  call display_array(AB, d, d)
  write(*,*) 'Tr(AB) = ', trace(AB, d)

  psi(1) = 1.0/sqrt(2.0);  psi(2) = psi(1);  write(*,*)'psi = ', psi
  sigma = reshape((/(/0,1/),(/1,0/)/), (/2,2/), order=(/2,1/))  ! inicialização de arrays 2D
  write(*,*)'sigma'
  call display_array(real(sigma), d, d)
  write(*,*)'<psi|sigma|psi> = ', sanduiche(d, psi, sigma)

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
subroutine produto_matricial(d, A, B, AB)
  implicit none
  integer :: d, j, k, l
  real :: A(d,d), B(d,d), AB(d,d)

  AB = 0
  do j = 1, d
    do k = 1, d
      do l = 1, d
        AB(j,k) = AB(j,k) + A(j,l)*B(l,k)
      end do
    end do
  end do

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
subroutine display_array(A, nl, nc)
  implicit none
  real :: A(nl,nc)
  integer :: nl, nc, j, k

  do j = 1, nl
    write(*,*) (A(j,k), k=1,nc)  ! implied d
  enddo

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
real function trace(A, d)
  implicit none
  integer :: d, j
  real :: A(d,d)

  trace = 0
  do j = 1, d
    trace = trace + A(j,j)
  enddo

end function
!------------------------------------------------------------------------------------------------------------------------------------
complex function sanduiche(d, psi, A)
  implicit none
  integer :: d, j, k
  complex :: A(d,d), psi(d)

  sanduiche = 0
  do k = 1, d
    do j = 1, d
      sanduiche = sanduiche + conjg(psi(k))*A(k,j)*psi(j)
    enddo
  enddo

end function
!------------------------------------------------------------------------------------------------------------------------------------
