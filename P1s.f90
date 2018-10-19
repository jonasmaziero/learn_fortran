!----------------------------------------------------
program P1
implicit none
real, parameter :: L = 1.e-3, C = 1.e-3, R = 1.e-3, q0 = 1.0
real :: omega, omegap, w, wp
character(len=10) :: resposta
real :: t, dt
real :: q, qp

write(*,*)"Digite 'sem' para resolver o problema sem resistor"
write(*,*)"Digite 'com' para resolver o problema com resistor"
read(*,*) resposta
dt = 0.05
if (resposta == 'sem') then
    open(unit=13,file="carga.dat",status="unknown")
    t = -dt
    do
        t = t + dt
        w = omega(L,C)
        call carga(q0,w,t,q)
        write(13,*) t, q
        if (t > 10) exit
    end do
else if (resposta == 'com') then
    open(unit=14,file="cargap.dat",status="unknown")
    t = -dt
    do
        t = t + dt
        w = omega(L,C)
        wp = omegap(L,C,R,w)
        call cargap(q0,wp,t,R,L,qp)
        write(14,*) t, qp
        if (t > 10) exit
    end do
end if

end program P1
!----------------------------------------------------
function omega(L,C)
implicit none
real :: omega
real :: L, C

omega = 1.0/sqrt(L*C)

end function omega
!----------------------------------------------------
function omegap(L,C,R,w)
implicit none
real :: omegap
real :: L, C, R, w

omegap = sqrt(w**2.0-(R/(2.0*L))**2.0)

end function omegap
!----------------------------------------------------
subroutine carga(q0,w,t,q)
implicit none
real :: q0, w, t, q

q = q0*cos(w*t)

end subroutine carga
!----------------------------------------------------
subroutine cargap(q0,wp,t,R,L,qp)
implicit none
real :: q0, wp, t, R, L, qp

qp = q0*exp(-(R/(2.0*L))*t)*cos(wp*t)

end subroutine cargap
!----------------------------------------------------