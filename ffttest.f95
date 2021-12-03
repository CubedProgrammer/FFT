program ffttest
implicit none
    interface
        subroutine fast_fourier_transform(re, im, sz)
            real, dimension(:), allocatable::re
            real, dimension(:), allocatable::im
            integer::sz
        endsubroutine
    endinterface
    real, dimension(:), allocatable::re
    real, dimension(:), allocatable::im
    integer::i
    integer::sz
    sz = 5
    allocate(re(sz))
    allocate(im(sz))
    do i=1,sz
        read*, re(i), im(i)
    enddo
    call fast_fourier_transform(re, im, sz)
    do i=1,sz
        print*, re(i), im(i)
    enddo
    deallocate(re)
    deallocate(im)
endprogram ffttest
