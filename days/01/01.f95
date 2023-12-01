program day_00
    implicit none

    ! Too lazy to dynamically allocate lines read from file,
    ! the largest line is 56, so we just round up..
    character(len=100) :: line
    integer :: iost, char_index, intval, concatval
    integer, allocatable :: pairs(:), new_pairs(:), all_calibrations(:)

    character(len=5) :: aside, bside, final
    open(unit=10, file='input.txt', status='old', action='read')
    
    ! Allocate "new_pairs" globally, we will keep mutating the first and only element
    ! of this array as we loop, then read it into "pairs" and then move on...
    allocate(new_pairs(1))

    ! Allocate all calibrations as zero-size..
    allocate(all_calibrations(0))

    do
        ! We allocate a zero-size array here, and we will allocate on assignment below
        allocate(pairs(0))

        ! Read a line, if we hit EOF, go to next loop iteration
        read(10, '(A)', iostat=iost) line
        if (iost /= 0) exit
        
        ! Loop over the characters in the line, and read them into "intval"
        do char_index = 1, len_trim(line)
            ! Skip over if we cannot convert to int here..
            read(line(char_index:char_index), *, iostat=iost) intval
            if (iost /= 0) cycle

            ! Allocate by assignment so pairs is always only as much as we need it to be
            new_pairs(1) = intval
            pairs = [pairs, new_pairs]
        end do

        ! Convert both the first and last elements of "pairs" to strings and concatenate.
        write(aside,'(I1)') pairs(1)
        write(bside,'(I1)') pairs(size(pairs))
        final = trim(aside) // trim(bside)

        ! Read in the concatenated string as an integer and append to all_calibrations
        read(final, *) concatval
        all_calibrations = [all_calibrations, concatval]
        deallocate(pairs)
    end do

    ! Let's also close the file cause we're goot like that
    close(10)

    ! Finally sum all the calibrations and print
    print *, sum(all_calibrations)
end program day_00
