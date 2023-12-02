program day_01
    implicit none

    ! Too lazy to dynamically allocate lines read from file,
    ! the largest line is 56, so we just round up..
    character(len=100) :: line
    integer :: iost, char_index, intval, all_calibrations
    integer, allocatable :: pairs(:)

    ! Open our input file for reading using an arbitrary handle number - 10..
    open(unit=10, file='input.txt', status='old', action='read')
    
    all_calibrations = 0

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

            ! Allocate to pairs using pairs + current int value
            pairs = [pairs, intval]
        end do
        
        ! Concatenating any two single digit ints can be done via: int1 * 10 + int2
        ! We are essentially just slamming first member of pairs with the last one
        all_calibrations = all_calibrations + (pairs(1) * 10 + pairs(size(pairs)))

        ! Free up pairs for next iteration
        deallocate(pairs)
    end do

    ! Let's also close the file cause we're goot like that
    close(10)

    ! Finally sum all the calibrations and print
    print *, all_calibrations
end program day_01
