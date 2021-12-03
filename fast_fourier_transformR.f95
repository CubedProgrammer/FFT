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
    integer::odd
    real::tmpre
    real::tmpim
    real::tau
    real::rt_of_unity_re
    real::rt_of_unity_im
    tau = 6.2831853071795865
    if(sz > gp)then
        call fast_fourier_transformR(auxre, auxim, re, im, sz, gp * 2, off)
        call fast_fourier_transformR(auxre, auxim, re, im, sz, gp * 2, off + gp)
        do i=1,sz/2,gp
            auxre(i + off) = re(1 + 2 * (i - 1) * gp + off)
            auxim(i + off) = im(1 + 2 * (i - 1) * gp + off)
            rt_of_unity_re = cos(tau * (i - 1) * gp / sz)
            rt_of_unity_im = sin(tau * (i - 1) * gp / sz)
            odd = (2 * i - 1) * gp + off + 1
            tmpre = re(odd) * rt_of_unity_re + im(odd) * rt_of_unity_im
            tmpim = im(odd) * rt_of_unity_re - re(odd) * rt_of_unity_im
            auxre(sz / 2 + i + off) = auxre(i + off) - tmpre
            auxim(sz / 2 + i + off) = auxim(i + off) - tmpim
            auxre(i + off) = auxre(i + off) + tmpre
            auxim(i + off) = auxim(i + off) + tmpim
        enddo
        do i=off + 1,sz,gp
            re(i) = auxre(i)
            im(i) = auxim(i)
        enddo
    endif
endsubroutine
