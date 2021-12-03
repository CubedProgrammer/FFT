subroutine fast_fourier_transform(re, im, sz)
    interface
        recursive subroutine fast_fourier_transformR(auxre, auxim, re, im, sz, gp, off)
            real, dimension(:), allocatable::auxre
            real, dimension(:), allocatable::auxim
            real, dimension(:), allocatable::re
            real, dimension(:), allocatable::im
            integer::sz
            integer::gp
            integer::off
        endsubroutine
    endinterface
    real, dimension(:), allocatable::re
    real, dimension(:), allocatable::im
    real, dimension(:), allocatable::auxre
    real, dimension(:), allocatable::auxim
    integer::sz
    integer::nsz = 1
    integer::i
    real, dimension(:), allocatable::nre
    real, dimension(:), allocatable::nim
    do while(nsz < sz)
        nsz = nsz * 2
    enddo
    if(nsz /= sz)then
        allocate(nre(nsz))
        allocate(nim(nsz))
        allocate(auxre(nsz))
        allocate(auxim(nsz))
        do i=1,sz
            nre(i)=re(i)
            nim(i)=im(i)
        enddo
        do i=sz+1,nsz
            nre(i) = 0
            nim(i) = 0
        enddo
        call fast_fourier_transformR(auxre, auxim, nre, nim, nsz, 1, 0)
        do i=1,sz
            re(i)=nre(i)
            im(i)=nim(i)
        enddo
        deallocate(nre)
        deallocate(nim)
    else
        allocate(auxre(sz))
        allocate(auxim(sz))
        call fast_fourier_transformR(auxre, auxim, re, im, sz, 1, 0)
    endif
    deallocate(auxre)
    deallocate(auxim)
endsubroutine fast_fourier_transform
