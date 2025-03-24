marbl_home = ${HOME}/MARBL
marbl_mod_path = $(marbl_home)/include/gnu
marbl_lib_path= $(marbl_home)/lib
netcdf_mod_path=
netcdf_lib_path=/opt/cray/pe/netcdf/4.9.0.15/CRAYCLANG/18.0/lib

overhead-fortran.exe: overhead.F90 call_compute_subroutine.o marbl_io_mod.o marbl_tools_mod.o marbl_netcdf_mod.o marbl_mpi_mod.o
	gfortran -o overhead-fortran.exe -g -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu  \
		overhead.F90 call_compute_subroutine.o marbl_io_mod.o marbl_tools_mod.o marbl_netcdf_mod.o marbl_mpi_mod.o $(marbl_lib_path)/libmarbl-gnu.a -fPIE -O2 -L$(netcdf_lib_path) -lnetcdff -lnetcdf


call_compute_subroutine.o: call_compute_subroutine.F90 marbl_io_mod.o marbl_tools_mod.o
	gfortran -c $^ -I$(marbl_mod_path) -fPIE -O2 -lmarbl-gnu -D_NETCDF


marbl_io_mod.o: marbl_io_mod.F90 marbl_netcdf_mod.o marbl_mpi_mod.o
	gfortran -c marbl_io_mod.F90 -I$(marbl_mod_path) -fPIE -O2 -lmarbl-gnu -D_NETCDF


marbl_netcdf_mod.o: marbl_netcdf_mod.F90
	gfortran -c $^ -I$(marbl_mod_path) -fPIE -O2 -lmarbl-gnu -I$(netcdf_mod_path) -D_NETCDF

%.o: %.F90
	gfortran -c $^ -I$(marbl_mod_path) -fPIE -O2 -lmarbl-gnu -D_NETCDF


clean: 
	rm *.o *.mod *.exe