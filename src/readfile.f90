module readfile_module

    implicit none

    private
    public :: readfile, read_lines, string_t

    type string_t
        character(len=:), allocatable :: s
    end type string_t

    integer, parameter :: max_line = 100000  ! The maximum number of rows
    integer, parameter :: BUFFER_SIZE = 1024

contains

    !> readfile method 1
    function readfile(fh) result(lines)
        integer, intent(in) :: fh
        type(string_t), allocatable :: lines(:)
        character(len=:), allocatable :: file_content
        integer :: i, length, stat, line_count
        integer, save :: idx(max_line) = 1
        character(len=BUFFER_SIZE) :: buffer

        inquire (fh, size=length)
        allocate (character(len=length) :: file_content)

        ! read file into a single string
        line_count = 0
        do
            read (fh, '(a)', iostat=stat) buffer
            if (is_iostat_end(stat)) exit
            line_count = line_count + 1
            length = len_trim(buffer)
            idx(line_count + 1) = idx(line_count) + length
            file_content(idx(line_count):idx(line_count + 1) - 1) = buffer(1:length)
        end do

        ! allocate lines from file content string
        allocate (lines(line_count))
        do i = 1, line_count
            allocate(lines(i)%s, source=file_content(idx(i):idx(i + 1) - 1))
        end do

    end function readfile

    !> Determine number or rows in a file given a LUN
    integer function number_of_rows(s) result(nrows)
        integer, intent(in) :: s
        integer :: ios
        rewind (s)
        nrows = 0
        do
            read (s, *, iostat=ios)
            if (ios /= 0) exit
            nrows = nrows + 1
        end do
        rewind (s)
    end function number_of_rows

    !> read lines into an array of TYPE(STRING_T) variables
    function read_lines(fh) result(lines)
        integer, intent(in) :: fh
        type(string_t), allocatable :: lines(:)

        integer :: i
        integer :: iostat

        allocate (lines(number_of_rows(fh)))
        do i = 1, size(lines)
            call getline(fh, lines(i)%s, iostat)
        end do

    end function read_lines

    !>AUTHOR: fpm(1) contributors
    !!LICENSE: MIT
    !>
    !!##NAME
    !!     getline(3f) - [M_io:READ] read a line of arbintrary length from specified
    !!     LUN into allocatable string (up to system line length limit)
    !!    (LICENSE:PD)
    !!
    !!##SYNTAX
    !!   subroutine getline(unit,line,iostat,iomsg)
    !!
    !!    integer,intent(in)                       :: unit
    !!    character(len=:),allocatable,intent(out) :: line
    !!    integer,intent(out)                      :: iostat
    !!    character(len=:), allocatable, optional  :: iomsg
    !!
    !!##DESCRIPTION
    !!    Read a line of any length up to programming environment maximum
    !!    line length. Requires Fortran 2003+.
    !!
    !!    It is primarily expected to be used when reading input which will
    !!    then be parsed or echoed.
    !!
    !!    The input file must have a PAD attribute of YES for the function
    !!    to work properly, which is typically true.
    !!
    !!    The simple use of a loop that repeatedly re-allocates a character
    !!    variable in addition to reading the input file one buffer at a
    !!    time could (depending on the programming environment used) be
    !!    inefficient, as it could reallocate and allocate memory used for
    !!    the output string with each buffer read.
    !!
    !!##OPTIONS
    !!    LINE    The line read when IOSTAT returns as zero.
    !!    LUN     LUN (Fortran logical I/O unit) number of file open and ready
    !!            to read.
    !!    IOSTAT  status returned by READ(IOSTAT=IOS). If not zero, an error
    !!            occurred or an end-of-file or end-of-record was encountered.
    !!    IOMSG   error message returned by system when IOSTAT is not zero.
    !!
    !!##EXAMPLE
    !!
    !!   Sample program:
    !!
    !!    program demo_getline
    !!    use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
    !!    use,intrinsic :: iso_fortran_env, only : iostat_end
    !!    use FPM_filesystem, only : getline
    !!    implicit none
    !!    integer :: iostat
    !!    character(len=:),allocatable :: line, iomsg
    !!       open(unit=stdin,pad='yes')
    !!       INFINITE: do
    !!          call getline(stdin,line,iostat,iomsg)
    !!          if(iostat /= 0) exit INFINITE
    !!          write(*,'(a)')'['//line//']'
    !!       enddo INFINITE
    !!       if(iostat /= iostat_end)then
    !!          write(*,*)'error reading input:',iomsg
    !!       endif
    !!    end program demo_getline
    !!
    subroutine getline(unit, line, iostat, iomsg)

        !> Formatted IO unit
        integer, intent(in) :: unit

        !> Line to read
        character(len=:), allocatable, intent(out) :: line

        !> Status of operation
        integer, intent(out) :: iostat

        !> Error message
        character(len=:), allocatable, optional :: iomsg

        character(len=BUFFER_SIZE) :: buffer
        character(len=256) :: msg
        integer :: size
        integer :: stat

        allocate (character(len=0) :: line)
        do
            read (unit, '(a)', advance='no', iostat=stat, iomsg=msg, size=size) &
                & buffer
            if (stat > 0) exit
            line = line//buffer(:size)
            if (stat < 0) then
                if (is_iostat_eor(stat)) then
                    stat = 0
                end if
                exit
            end if
        end do

        if (stat /= 0) then
            if (present(iomsg)) iomsg = trim(msg)
        end if
        iostat = stat

    end subroutine getline

end module readfile_module
