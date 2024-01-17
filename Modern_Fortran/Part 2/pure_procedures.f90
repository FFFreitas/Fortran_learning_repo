program pure_procedures
        implicit none
        integer:: s1, s2
        s1 = 1
        s2 = 2
        print *, s1, s2
        print *, sum1(s1, s2)
        print *, sum1([1, 2, 3], [4, 5, 6])
        print *, sum1([1, 2], 3)
        print *, sum1(1, [2, 3, 4])
        ! print *, sum1([1, 2], [2, 3, 4])

        contains
                pure elemental integer function sum1(a, b)
                        integer, intent(in):: a, b
                        sum1 = a+b
                end function sum1
end program pure_procedures
