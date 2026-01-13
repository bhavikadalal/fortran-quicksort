How to Compile and Run (Normal System)
From the project root:
bash /In terminal run below command:
gfortran src/sorting_module.f90 test/run_tests.f90 -o run_tests
./run_tests

------------------------------------------------------------------------

Running with Docker:
1. Build the Docker image
bash /In terminal run below command:
docker build -t arch-gfortran -f gfortran.docker .

2. Run a container with your project mounted
bash /In terminal run below command:
docker run -it --rm -v $(pwd):/workspace arch-gfortran

3. Inside the container, compile and run
bash /In terminal run below command:
cd /workspace
gfortran src/sorting_module.f90 test/run_tests.f90 -o run_tests
./run_tests

