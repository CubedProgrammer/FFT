subroutine fast_fourier_transform(re, im, sz)
implicit none
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
        do i=0,sz-1
            nre(i)=re(i)
            nim(i)=im(i)
        enddo
        do i=sz,nsz-1
            nre(i) = 0
            nim(i) = 0
        enddo
        call fast_fourier_transformR(auxre, auxim, nre, nim, nsz, 1, 0)
        deallocate(nre)
        deallocate(nim)
    else
        allocate(auxre(nsz))
        allocate(auxim(nsz))
        call fast_fourier_transformR(auxre, auxim, re, im, sz, 1, 0)
    endif
    deallocate(auxre)
    deallocate(auxim)
endsubroutine fast_fourier_transform
