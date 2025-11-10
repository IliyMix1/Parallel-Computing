program main
  implicit none
  include 'mpif.h'
  
  !Declaring variables
  integer :: ierr, rank, size, status(MPI_STATUS_SIZE)
  integer :: n, m, half
  integer :: i, j
  real :: temp
  real, allocatable :: mat(:,:), mat1(:,:), mat2(:,:), mat3(:,:), mat4(:,:), mat_recv(:,:)

  !Initialing the MPI 
  call MPI_INIT(ierr)
  !Returning total number of processes(must be 4)
  call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
  !Giving every process a personal number(rank)
  call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
 
 
  if(rank == 0) then
        !Reading the matrix from file
        open(10, file='A', form='formatted', status='unknown') 
        read(10, *) n, m
        allocate(mat(n, m))
      
        !Copying the matrix to array
        do i = 1, n
              read(10, *) (mat(i, j), j = 1, m) 
        end do       

        !Printing the matrix 
        print *, 'Matrix before algorithm'
        do i = 1, n
             do j = 1, m
                  write(*, '(f8.1)', advance='no') mat(i, j)
             enddo
             print *   
        enddo
        !Closing the file
        close(10)


        !Initializing small matrixes dimensions(we assume that they're even)
        half = n/2  
        allocate(mat1(half, half), mat2(half, half), mat3(half, half), mat4(half, half), mat_recv(half, half))
 
        !Breaking the big matrix to 4 small matrixes
        do i = 1, half 
           do j = 1, half
              mat1(i, j) = mat(i, j)            !Top left  square
              mat2(i, j) = mat(i, j+half)       !Top right square
              mat3(i, j) = mat(i+half, j)       !Bottom left  square
              mat4(i, j) = mat(i+half, j+half)  !Bootom right square
           enddo
        enddo
       
        !Transposing the small top left matrix
        do i = 1, half
             do j = i+1, half
                  temp = mat1(i, j)
                  mat1(i, j) = mat1(j, i)
                  mat1(j, i) = temp
             enddo
        enddo

        !Sending small matrixes and their dimension
        call MPI_BCAST(half, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
        call MPI_SEND(mat2, half*half, MPI_REAL, 1, 11, MPI_COMM_WORLD, ierr)        
        call MPI_SEND(mat3, half*half, MPI_REAL, 2, 12, MPI_COMM_WORLD, ierr)        
        call MPI_SEND(mat4, half*half, MPI_REAL, 3, 13, MPI_COMM_WORLD, ierr)
       
        !Receiving transposed matrixes
        call MPI_RECV(mat2, half*half, MPI_REAL, 1, 11, MPI_COMM_WORLD, status, ierr) 
        call MPI_RECV(mat3, half*half, MPI_REAL, 2, 12, MPI_COMM_WORLD, status, ierr) 
        call MPI_RECV(mat4, half*half, MPI_REAL, 3, 13, MPI_COMM_WORLD, status, ierr)
  endif
  
  if(rank /= 0) then
        !Receiving the small matrixes and their dimension
        call MPI_BCAST(half, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
        !Initializing the size of array
        allocate(mat1(half, half))

        if (rank == 1) then
             call MPI_RECV(mat1, half*half, MPI_REAL, 0, 11, MPI_COMM_WORLD, status, ierr)
        else if (rank == 2) then
             call MPI_RECV(mat1, half*half, MPI_REAL, 0, 12, MPI_COMM_WORLD, status, ierr)
        else if (rank == 3) then
             call MPI_RECV(mat1, half*half, MPI_REAL, 0, 13, MPI_COMM_WORLD, status, ierr)
        end if
        
        !Transposing small matrixes
        do i = 1, half
             do j = i+1, half
                  temp = mat1(i, j)
                  mat1(i, j) = mat1(j, i)
                  mat1(j, i) = temp
             enddo
        enddo

        !Sending transposed matrixes to master process
        if (rank == 1) then
             call MPI_SEND(mat1, half*half, MPI_REAL, 0, 11, MPI_COMM_WORLD, ierr)
        else if (rank == 2) then
             call MPI_SEND(mat1, half*half, MPI_REAL, 0, 12, MPI_COMM_WORLD, ierr)
        else if (rank == 3) then
             call MPI_SEND(mat1, half*half, MPI_REAL, 0, 13, MPI_COMM_WORLD, ierr)
        end if
        
        !Clearing the memory
        deallocate(mat1)
  end if
  
  !Gathering small matrixes to one big matrix
  if(rank == 0) then
        do i = 1, half
             do j = 1, half
                  mat(i, j) = mat1(i, j)
                  mat(i, j+half) = mat3(i, j)
                  mat(i+half, j) = mat2(i, j)
                  mat(i+half, j+half) = mat4(i, j)
             enddo
        enddo
        
        !Printing the result
        print *, 'Matrix after algorithm'
        do i = 1, n
             do j = 1, m
                  write(*, '(f8.1)', advance='no') mat(i, j)
             enddo
             print *   !Switching to a new line
        enddo
  endif
  call MPI_FINALIZE(ierr)
end program main
