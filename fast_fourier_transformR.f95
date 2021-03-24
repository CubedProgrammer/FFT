recursive subroutine fast_fourier_transformR(auxre, auxim, re, im, sz, gp, off)
    implicit none
    real, dimension(:), allocatable::auxre
    real, dimension(:), allocatable::auxim
    real, dimension(:), allocatable::re
    real, dimension(:), allocatable::im
    integer::sz
    integer::gp
    integer::off
    integer::i
    real::tmpre
    real::tmpim
    real::tau
    tau = 3.1415926535897932 * 2
    if(sz > 1)then
        call fast_fourier_transformR(auxre, auxim, re, im, sz, gp * 2, off)
        call fast_fourier_transformR(auxre, auxim, re, im, sz, gp * 2, off + gp)
        do i=0,sz/2-1,gp
            auxre(i + off) = re(2 * i + off)
            auxim(i + off) = im(2 * i + off)
            tmpre = re(2 * i + off + gp) * cos(tau * i / gp / sz) + im(2 * i + off + gp) * sin(tau * i / gp / sz)
            tmpim = im(2 * i + off + gp) * cos(tau * i / gp / sz) - re(2 * i + off + gp) * sin(tau * i / gp / sz)
            auxre(i + off) = auxre(i + off) + tmpre
            auxim(i + off) = auxim(i + off) + tmpim
            auxre(sz / 2 + i + off) = auxre(i + off) - tmpre
            auxim(sz / 2 + i + off) = auxim(i + off) - tmpim
        enddo
    endif
    do i=0,sz-1,1
        re(i) = auxre(i)
        im(i) = auxim(i)
    enddo
endsubroutine
