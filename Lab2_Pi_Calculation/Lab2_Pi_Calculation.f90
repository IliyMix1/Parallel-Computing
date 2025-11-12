program Lab2
        IMPLICIT NONE
        include 'mpif.h'

        !Declaring variables
        integer :: ierr, rank, size
        integer :: i, j, n, n_min, n_max
        double precision :: pi, mypi


        call MPI_INIT(ierr)
        call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
        call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)


        if (rank == 0) then 
                !Getting "n" from user
                write(6, *)"Enter the number of terms in the series:"
                read(5, *) n
        endif

        !Sending and receiving "n" on every process
        call MPI_BCAST(n, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
                
        !Setting limits
        n_max = n/size*(rank+1)
        n_min = n/size*rank + 1
        
        !Calculating some part of the arctan(1) on every process, cause it equals to pi/4
        mypi = 0d0
        do i=n_min, n_max
                mypi = mypi +(-1.d0)**i/(2.d0*i + 1.d0)  
        enddo
        
        !Gathring all calculated parts together and sending it to master-process  
        call MPI_REDUCE(mypi, pi, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
        
        !We calculated (pi/4 - 1), so we must clean things up
        if (rank == 0) then
                pi = 4.d0*(pi + 1.d0)  !Adding 1, because we skipped the first term of the series(sum starts from i=1)
                print *, "pi is approximately = ", pi
        endif   

        call MPI_FINALIZE(ierr)
end program Lab2


