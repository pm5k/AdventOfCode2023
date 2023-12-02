program day_01
    implicit none

    character(len=100) :: line
    character(len=5), dimension(9) :: keys, values
    integer :: iost, i, j, start, found, all_calibrations
    complex, allocatable :: pairs(:), temp 

    ! This is our "map"..
    keys = [character(len=5) :: "one","two","three","four","five","six","seven","eight","nine"]
    values = [character(len=1) :: "1","2","3","4","5","6","7","8","9"]

    ! Open our input file for reading using an arbitrary handle number - 10..
    open(unit=10, file='input.txt', status='old', action='read')
    
    all_calibrations = 0
    do
        ! We allocate a zero-size array here, and we will allocate on assignment below
        allocate(pairs(0))

        ! Read a line, if we hit EOF, go to next loop iteration
        read(10, '(A)', iostat=iost) line
        if (iost /= 0) exit
        
        ! The first double loop will iterate over each key and find all of it's occurrences
        ! inside the line provided along with it's index in the line.
        do i = 1, size(keys)
            start = 1
            do
                found = INDEX(trim(line(start:)), trim(keys(i)))
                if (found == 0) exit
                pairs = [pairs, COMPLEX(IACHAR(trim(values(i))) - IACHAR("0"), found + start - 1)]
                start = start + found + len_trim(keys(i)) - 1
            end do
        end do

        ! The secound double loop will do much the same but with the values to find int reprs
        ! inside the line provided.
        do i = 1, size(values)
            start = 1
            do
                found = index(trim(line(start:)), trim(values(i)))
                if (found == 0) exit
                pairs = [pairs, COMPLEX(IACHAR(trim(values(i))) - IACHAR("0"), found + start - 1)]
                start = start + found + len_trim(values(i)) - 1
            end do
        end do

        ! Use bubble sort to sort the pairs by index where (actual_val, index_in_line).
        do i = 1, size(pairs)-1
            do j = 1, size(pairs)-i
                if (pairs(j)%im > pairs(j+1)%im) then
                    temp = pairs(j)
                    pairs(j) = pairs(j+1)
                    pairs(j+1) = temp
                endif
            end do
        end do

        ! Concatenating any two single digit ints can be done via: int1 * 10 + int2
        ! We are essentially just slamming first member of pairs with the last one
        all_calibrations = all_calibrations + (pairs(1)%re * 10 + pairs(size(pairs))%re)

        ! Free up pairs for next iteration
        deallocate(pairs)
    end do

    ! Let's also close the file cause we're goot like that
    close(10)

    ! Finally sum all the calibrations and print
    print *, all_calibrations
end program day_01
