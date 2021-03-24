program ffttest
implicit none
    real, dimension(:), allocatable::re
    real, dimension(:), allocatable::im
    integer::i
    do i=0,4
        read(*,*)re(i), im(i)
    enddo
    call fast_fourier_transform(re, im, 4)
    do i=0,4
        print*,re(i), im(i)
    enddo
endprogram ffttest
