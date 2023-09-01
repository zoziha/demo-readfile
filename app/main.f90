program main

    use readfile_module, only: readfile, read_lines, string_t
    use timer_module, only: timer
    implicit none
    type(string_t), allocatable :: lines(:)
    type(timer) :: tmr
    integer :: i

    open (1, file='src/readfile.f90', status='old', action='read')
    call tmr%tic()
    do i = 1, 1000
        rewind (1)
        lines = read_lines(1)
    end do
    print *, 'Elapsed time: ', tmr%toc(), 's'
    print *, lines(size(lines))%s, size(lines)

    call tmr%tic()
    do i = 1, 1000
        rewind (1)
        lines = readfile(1)
    end do
    print *, 'Elapsed time: ', tmr%toc(), 's'
    print *, lines(size(lines))%s, size(lines)
    close (1)

    ! check for another file
    open (1, file='fpm.toml', status='old', action='read')
    rewind (1)
    lines = readfile(1)
    print *, lines(size(lines))%s, size(lines)

    rewind (1)
    lines = read_lines(1)
    print *, lines(size(lines))%s, size(lines)
    close (1)

end program main
