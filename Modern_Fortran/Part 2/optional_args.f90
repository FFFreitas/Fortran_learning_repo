program subroutines_example
        implicit none
        integer:: a, b, c
        a = 1
        b = 3
        print *, a, b
        call add(a, b, c)
        call add(a, b, c, .true.)
        call add(a, b, c, debug=.true.)

        print *, a, b

        contains
                subroutine add(a, b, res, debug)
                        integer, intent(in):: a, b
                        integer, intent(out):: res
                        logical, intent(in), optional:: debug
                        if (present(debug)) then
                                if (debug) then
                                        print *, 'DEBUG: subroutine add a = ', a
                                        print *, 'DEBUG: subroutine add b = ', b
                                end if
                        end if
                        res = a+b

                        if (present(debug)) then
                                if (debug) print *, &
                                        'DEBUG: subroutine add, ress = ', res
                        end if
                end subroutine add
end program subroutines_example

