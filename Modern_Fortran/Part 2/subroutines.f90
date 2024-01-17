program subroutines_example
        implicit none
        integer:: a, b
        a = 1
        b = 3
        print *, a, b
        call add(a, b)
        print *, a, b

        contains
                subroutine add(a, b)
                        integer, intent(in):: a
                        integer, intent(in out):: b
                        b = a+b
                end subroutine add
end program subroutines_example

