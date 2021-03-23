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
    if(sz > 1)then
        call fast_fourier_transformR(auxre, auxim, re, im, sz, gp * 2, off)
        call fast_fourier_transformR(auxre, auxim, re, im, sz, gp * 2, off + gp)
        do i=0,sz/2-1,gp
            re(i + off) = auxre(2 * i + off)
            im(i + off) = auxim(2 * i + off)
        enddo
    endif
endsubroutine

