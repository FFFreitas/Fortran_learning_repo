program tsunami
        implicit none

        integer:: i, n
        integer, parameter:: grid_size = 100
        integer, parameter:: num_time_steps = 100

        real, parameter:: dt = 1,  dx = 1, c = 1  
        real, dimension(grid_size):: h
        ! real:: h(grid_size) ! <- shor hand version

        real:: dh(grid_size)
        integer, parameter:: icenter = 25
        real, parameter:: decay = 0.02


        if (grid_size <= 0) stop 'grid size must be > 0'
        if (dt <= 0) stop 'dt must be > 0'
        if (dx <= 0) stop 'dx must be > 0'
        if (c <= 0) stop 'c must be > 0'

        ! we can also use do concurrent here
        do concurrent (i = 1:grid_size)
                h(i) = exp(-decay * (i-icenter)**2)
        end do

        print *, 0, h

        time_loop: do n = 1, num_time_steps
                dh(1) = dh(1) - h(grid_size)
                do i = 2, grid_size
                        dh(i) = h(i) - h(i-1)
                end do

                do i = 1, grid_size
                        h(i) = h(i) - c*dh(i) / dx*dt
                end do
        end do time_loop

        print *, n, h



end program tsunami
