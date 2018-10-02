!------------------------------------------------------------------------------------------------------------------------------------
program mem ! Alocação dinâmica de memória
  call memalloc()
end program mem
!------------------------------------------------------------------------------------------------------------------------------------
subroutine memalloc()
  implicit none
  integer :: d
  integer, allocatable :: vec(:)
  real, allocatable :: x

  allocate(x)
  x = 1.0
  write(*,*) x
  deallocate(x)
  !stop

  d = 2
  allocate(vec(d))
  vec = (/1,2/)
  write(*,*) vec
  deallocate(vec)

  d = 4
  allocate(vec(d))
  vec = (/1,2,3,4/)
  write(*,*) vec
  deallocate(vec)

end subroutine memalloc
!------------------------------------------------------------------------------------------------------------------------------------
