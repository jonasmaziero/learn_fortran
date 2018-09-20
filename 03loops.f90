!------------------------------------------------------------------------------------------------------------------------------------
program loops
implicit none
integer :: fatorial, n, fibonacci, fatrec, fibrec

  !call do_loops()
  !call do_nested()
  do n = 0, 12
    !write(*,*) n,"! = ",fatorial(n)
    !write(*,*) n,"! = ",fatrec(n)
    !write(*,*) fibonacci(n)
    write(*,*) fibrec(n)
  enddo
  !n = 100
  !call pares(n)
  !call primos(n)
end program
!------------------------------------------------------------------------------------------------------------------------------------
subroutine do_loops()
implicit none
integer :: j, N = 10

  !!! do com índices para início e fim
  write(*,*) "do loop com indices"
  do j = 1, N, 1  ! o último número dá o passo para os valores de j
    if (j == 3) cycle  ! cycle volta para o início sem implementar os comandos abaixo dele e vai para o próximo valor de j
    if (mod(j,2)==0) cycle
    if (j >= 9) exit ! se a condição é satisfeita, o exit causa a saída por completo do loop
    write(*,*) "j = ", j  ! escreve j na tela
  end do

  !!! do implícito (executa indefinidamente se não colocarmos uma condição com exit)
  write(*,*)
  write(*,*) "do loop implicito"
  j = 0
  do
    j = j + 1
    write(*,*) "j = ", j
    if (j > 3) exit
  end do

end subroutine
!------------------------------------------------------------------------------------------------------------------------------------
subroutine do_nested()
implicit none
integer :: j, k

  doj: do j = 1, 10, 3  ! podemos adicionar nomes aos loops para aumetar nosso controle
    dok: do k = 1, 13, 4
      if (k == 5 .or. k == 9) cycle dok
      if (j > 6) exit doj
      write(*,*) "j =", j, ",  k =", k
    end do dok
  end do doj

end subroutine do_nested
!------------------------------------------------------------------------------------------------------------------------------------
integer function fatorial(n)
implicit none
integer :: n, j

  if (n == 0 .or. n == 1) then
    fatorial = 1
  else if (n > 1) then
    fatorial = 1
    do j = 2, n
      fatorial = fatorial*j
    end do
  end if

end
!------------------------------------------------------------------------------------------------------------------------------------
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
!------------------------------------------------------------------------------------------------------------------------------------
integer function fibonacci(n)
implicit none
integer j, F1, F2, F, n
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
!------------------------------------------------------------------------------------------------------------------------------------
recursive function fibrec(n) result(fib)
  implicit none
  integer :: fib, n

  if (n < 2) then
    fib = n
  else
    fib = fibrec(n-2)+fibrec(n-1)
  end if

end
!------------------------------------------------------------------------------------------------------------------------------------
subroutine pares(N)
  implicit none
  integer j, N

  do j = 1, N
    if( mod(j,2) == 0 ) write(*,*) j
    !write(*,*) j, j/2
  enddo

end subroutine pares
!------------------------------------------------------------------------------------------------------------------------------------
subroutine primos(N)
  implicit none
  integer j, k, N

  if (N < 2) write(*,*) "N deve ser maior que 1"
  if (N == 2) then
    write(*,*) N
  else if (N > 2) then
    doj: do j = 2, N
      dok: do k = 2, ((j/2)+1)
        if (mod(j,k) == 0) cycle doj
      enddo dok
      write(*,*) j
    enddo doj
  endif

end subroutine primos
!------------------------------------------------------------------------------------------------------------------------------------
