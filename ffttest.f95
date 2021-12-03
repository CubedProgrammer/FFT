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
    allocate(re(4))
    allocate(im(4))
    do i=1,4
        read*, re(i), im(i)
    enddo
    call fast_fourier_transform(re, im, 4)
    do i=1,4
        print*, re(i), im(i)
    enddo
    deallocate(re)
    deallocate(im)
endprogram ffttest
