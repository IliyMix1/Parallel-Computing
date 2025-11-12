# Parallel-Computing
Fortran. MPI
Here I store my small projects for university 

If you use linux and want to run my code, you first of all must install MPI. Type this in terminal:
sudo apt install mpich

Then, you need to open the directory where you put the file and compile it. Type this in terminal:
mpif90 file_name 

To run file, type this in terminal:
mpirun -np number_of_processes ./a.out
