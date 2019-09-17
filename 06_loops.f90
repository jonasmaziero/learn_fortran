!-------------------------------------------------------------------------------
program loops
  implicit none
  integer(kind=8) :: j,fatorial
  integer :: k, fibonacci

  !call loopj()
  !call loopx()
  !call loopjk()
  !call loopn()
  !do j = 0, 31
  !  write(*,*) j,"! = ",fatorial(j)
  !enddo
  !write(*,*)"Sequência de Fibonacci"
  !do k = 0, 10
  !  write(*,*) fibonacci(k)
  !enddo
  call primos(50)

end program
!-------------------------------------------------------------------------------
subroutine loopj()
  implicit none
  integer :: j

  do j = 130, 10, -20
    write(*,*) "j = ",j**2
  end do

end subroutine loopj
!-------------------------------------------------------------------------------
subroutine loopx()  ! loop implícito (cuidado com a condição de parada)
  implicit none
  real :: xi, xf, y, dx
  integer :: N

  xi = 0.0;  xf = 10.0;  N = 10
  dx = (xf-xi)/dble(N)  ! dble(j) passa j pra real
  write(*,*) "xi = ", xi, "; xf = ", xf, "; N = ", N, "; dx =", dx
  y = xi - dx  ! inicialização
  do
    y = y + dx
    if (y > xf) exit  ! nunca esqueça dessa condição
    write(*,*) "y = ", y
  end do

end subroutine
!-------------------------------------------------------------------------------
subroutine loopjk()  ! nested loops
  implicit none
  integer :: j, k

  do j = 1, 5
    do k = 2, 5, 2
      write(*,*) "j = ",j, "; k = ",k
    end do
  end do

end subroutine
!-------------------------------------------------------------------------------
subroutine loopn()  ! loops nomeados
  implicit none
  integer :: j, k

  loopj: do j = 1, 5
    dok: do k = 1, 5, 1
      if (mod(j,2) == 0) cycle loopj
      ! mod(x,y) retorna o resto da divisão de x e y
      write(*,*) "j = ",j, "; k = ",k
    end do dok
  end do loopj

end subroutine
!-------------------------------------------------------------------------------
function fatorial(n)  ! n! = n(n-1)(n-2)...(2)(1); 0! = 1;  1! = 1
implicit none
integer(8) :: n, j, fatorial

  if (n == 0 .or. n == 1) then
    fatorial = 1
  else !if (n > 1) then
    fatorial = 1
    do j = 2, n
      fatorial = fatorial*j
    end do
  end if

end
!-------------------------------------------------------------------------------
recursive function fatrec(n) result(fat)  ! Implementação recursiva do fatorial
implicit none
integer :: fat
integer :: n

  if (n == 0 .or. n == 1) then
    fat = 1
  else
    fat = n*fatrec(n-1)
  end if

end
!-------------------------------------------------------------------------------
function fibonacci(n)
implicit none
integer :: j, F1, F2, F, n, fibonacci
  if (n < 2) then
    F = n
  else if (n >= 2) then
    F1 = 0
    F2 = 1
    do j = 2, n
      F = F1 + F2
      F1 = F2
      F2 = F
    end do
  end if
  fibonacci = F
end function
!-------------------------------------------------------------------------------
recursive function fibrec(n) result(fib)  ! Implementação recursiva da Fibonacci
  implicit none
  integer :: fib, n

  if (n < 2) then
    fib = n
  else
    fib = fibrec(n-2)+fibrec(n-1)
  end if

end
!-------------------------------------------------------------------------------
subroutine pares(N)  ! números divisíveis por 2
  implicit none
  integer j, N

  do j = 1, N
    if( mod(j,2) == 0 ) write(*,*) j
  enddo

end subroutine
!-------------------------------------------------------------------------------
subroutine primos(N)  ! números divisiveis somente por 1 e por ele mesmo
  implicit none
  integer j, k, N

  if (N < 2) write(*,*) "N deve ser maior que 1"
  if (N == 2) then
    write(*,*) N
  else if (N > 2) then
    write(*,*) 2
    if(N >= 3) write(*,*) 3
    doj: do j = 4, N
      dok: do k = 2, j/2
        if (mod(j,k) == 0) cycle doj
      enddo dok
      write(*,*) j
    enddo doj
  endif

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
