program tsunami_ref
        implicit none

        integer:: i, n
        integer, parameter:: grid_size = 100
        integer, parameter:: num_time_steps = 100

        real, parameter:: dt = 1,  dx = 1, c = 1  
        real, dimension(grid_size):: h

        real:: dh(grid_size)
        integer, parameter:: icenter = 25
        real, parameter:: decay = 0.02


        if (grid_size <= 0) stop 'grid size must be > 0'
        if (dt <= 0) stop 'dt must be > 0'
        if (dx <= 0) stop 'dx must be > 0'
        if (c <= 0) stop 'c must be > 0'

        ! we can also use do concurrent here
        call set_gausian(h, icenter, decay)

        print *, 0, h

        time_loop: do n = 1, num_time_steps
                
                dh = diff(h)

                do i = 1, grid_size
                        h(i) = h(i) - c*dh(i) / dx*dt
                end do
        end do time_loop

        print *, n, h

        contains
                function diff(x) result(dx)
                        real, intent(in):: x(:)
                        real:: dx(size(x))
                        integer:: im
                        im = size(x)
                        dx(1) = x(1) - x(im)
                        dx(2:im) = x(2:im) - x(1:im-1)
                end function diff

                subroutine set_gausian(x, icenter, decay)
                        real, intent(in out):: x(:)
                        integer, intent(in):: icenter
                        real, intent(in):: decay
                        do concurrent(i = 1:size(x))
                                x(i) = exp(-decay * (i-icenter)**2)
                        end do
                end subroutine set_gausian


end program tsunami_ref
