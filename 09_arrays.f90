!-------------------------------------------------------------------------------
!program arrrays
  !call array1D()
!  call array2D()
!end program
!-------------------------------------------------------------------------------
subroutine array1D()
  implicit none
  integer, parameter :: d = 2 ! constantes em Fortran
  real :: va(1:d), vb(d) ! Essas duas declarações são equivalentes
  real :: pe, produto_escalar
  complex, dimension(d) :: vac, vbc ! outra forma de declarar arrays
  complex :: pec

  write(*,*)"Entre com um array de d = 2"
  ! No terminal entra com e.g. 1,3 (tem que ser nesse formato)
  read(*,*) va
  write(*,*) va
  !stop ! termina o subprograma aqui

  !arrays 1D: 2 modos de inicialização (equivalentes)
  va = (/1,1/);  write(*,*) '|a> = ', va
  vb(1) = 2; vb(2) = 3;  write(*,*) '|b> = ', vb
  pe = produto_escalar(d, va, vb)
  write(*,*) "pe = ", pe

  write(*,*)'arrays 1D complexos'
  vac(1) = 1.0; vac(2) = (2.0,1.0);  write(*,*) '|ac> = ', vac
  vbc(1) = 1.0; vbc(2) = (1.0,1.0);  write(*,*) '|bc> = ', vbc
  call produto_escalar_c(d, vac, vbc, pec)
  write(*,*) "pec = ", pec
  write(*,"(A10,1F10.5)") "Re(pec) = ", real(pec) ! Escrita formatada
  write(*,"(A10,2F8.3)") "Im(pec) = ", aimag(pec), 2*aimag(pec)
end subroutine
!-------------------------------------------------------------------------------
real function produto_escalar(d, A, B)
  implicit none
  integer :: d, j
  real :: A(d), B(d), pe

  produto_escalar = 0
  do j = 1, d
    produto_escalar = produto_escalar + A(j)*B(j)
  end do

end function
!-------------------------------------------------------------------------------
real function norma(d,v)
  implicit none
  integer :: d, j
  real :: v(d)

  norma = 0.0
  do j = 1, d
    norma = norma + v(j)**2.0
  enddo
  norma = sqrt(norma)

  ! Outra maneira (mais simples) de obter a norma
  !norma = sqrt(produto_escalar(d, v, v))

end function
!-------------------------------------------------------------------------------
! O Fortran não aceita definir uma função complexa (por isso usei subrotina)
subroutine produto_escalar_c(d, A, B, pe)
  implicit none
  integer :: d, j
  complex :: A(d), B(d), pe

  pe = 0
  do j = 1, d
    pe = pe + conjg(A(j))*B(j)
  end do

end subroutine
!-------------------------------------------------------------------------------
subroutine array2D()
  implicit none
  integer, parameter :: d = 2, m = 2, n = 2
  real :: A(m,n), B(d,d), AB(d,d), trace
  complex :: psi(d), sigma(d,d), sanduiche

  ! esse comando colocar o array linha após linha, como costumamos trabalhar com matrizes
  A = reshape((/(/1,2/),(/3,4/)/), (/2,2/), order=(/2,1/))  ! inicialização de arrays 2D
  write(*,*) "A = ", A ! o Fortran guarda arrays na memória coluna após coluna
  write(*,*) "A = "
  call display_array(A, d, d)  ! mostra o array como uma matriz
  ! para obter as dimensões de um array use:
  write(*,*) "shape de A = ", shape(A), ", nlA = ", size(A,1), ", ncA = ", size(A,2)
  ! Faça uma função que retorne o traço (soma dos elementos na diagonal principal) de uma matriz
  write(*,*) 'Tr(A) = ', trace(d,A)
  !stop

  B(1,1) = 5;  B(1,2) = 6;  B(2,1) = 7;  B(2,2) = 8 ! outra forma de inicialização
  write(*,*)'B = ', B
  write(*,*)'B = '
  call display_array(B, d, d)

  AB = 0 ! maneira simples de inicializar um array, com todos elementos iguais
  write(*,*)'AB = ', AB
  call produto_matricial(d, d, d, A, B, AB)
  write(*,*)'AB = ', AB
  write(*,*)'AB = '
  call display_array(AB, d, d)
  !stop

  psi(1) = 1.0/sqrt(2.0);  psi(2) = psi(1);  write(*,*)'psi = ', psi
  sigma = reshape((/(/0,1/),(/1,0/)/), (/2,2/), order=(/2,1/))
  write(*,*)'sigma'
  call display_array(real(sigma), d, d)
  write(*,*)'<psi|sigma|psi> = ', sanduiche(d, psi, sigma)

end subroutine
!-------------------------------------------------------------------------------
subroutine display_array(A, nl, nc)
  implicit none
  integer :: nl, nc, j, k
  real :: A(nl,nc)

  do j = 1, nl
    write(*,*) (A(j,k), k=1,nc)  ! implied do
  enddo

end subroutine
!-------------------------------------------------------------------------------
real function trace(d,A)
  implicit none
  integer :: d, j
  real :: A(d,d)

  trace = 0
  do j = 1, d
    trace = trace + A(j,j)
  enddo

end function
!-------------------------------------------------------------------------------
subroutine soma_matricial(m,n,X,Y,XmY)
  implicit none
  integer :: m,n,j,k
  real :: X(m,n), Y(m,n), XmY(m,n)

  do j = 1, m
    do k = 1, n
      XmY(j,k) = X(j,k) + Y(j,k)
    enddo
  enddo

end subroutine
!-------------------------------------------------------------------------------
subroutine produto_matricial(p,q,r, A, B, AB)
  implicit none
  integer :: p,q,r ,j, k, l
  real :: A(p,q), B(q,r), AB(p,r)

  AB = 0
  do j = 1, p
    do k = 1, r
      do l = 1, q
        AB(j,k) = AB(j,k) + A(j,l)*B(l,k)
      end do
    end do
  end do

end subroutine
!-------------------------------------------------------------------------------
complex function sanduiche(d, psi, A) !<psi|A|psi> valor médio
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
!-------------------------------------------------------------------------------
