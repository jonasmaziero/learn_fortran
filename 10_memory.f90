!-------------------------------------------------------------------------------
program mem ! Alocação dinâmica de memória
  call memalloc()
end program mem
!-------------------------------------------------------------------------------
subroutine memalloc()
  implicit none
  integer :: d
  real, allocatable :: x
  integer, allocatable :: vec(:)
  complex, allocatable :: A(:,:)

  allocate(x) ! só pra dizer que pode ser com qualquer tipo de variável
  x = 1.0
  write(*,*) 'x = ',x
  deallocate(x)
  !stop

  !d = 2
  write(*,*) 'Entre com a dimensão do vetor'
  read(*,*) d
  allocate(vec(d))
  !vec = (/1,2/)
  write(*,*) 'Entre com o vetor'
  read(*,*) vec
  write(*,*) 'vec = ',vec
  deallocate(vec)
  write(*,*) 'vec depois de deallocate = ',vec
  
  d = 4
  allocate(vec(d))
  vec = (/1,2,3,4/)
  write(*,*) 'vec = ',vec
  deallocate(vec)
  write(*,*) 'vec depois de deallocate = ',vec

  allocate(A(2,2))
  A(1,1) = 0; A(1,2) = -(0,1); A(2,1) = conjg(A(1,2)); A(2,2) = A(1,1)
  write(*,*) 'A = ',A
  deallocate(A)

end subroutine memalloc
!-------------------------------------------------------------------------------
