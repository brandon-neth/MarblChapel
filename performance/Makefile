marbl_home = ${HOME}/MARBL
marbl_mod_path = $(marbl_home)/include/gnu
marbl_lib_path= $(marbl_home)/lib

netcdf_flags=-I/opt/cray/pe/netcdf/4.9.0.13/gnu/12.3/include -L /opt/cray/pe/netcdf/4.9.0.13/gnu/12.3/lib/ -lnetcdff
overhead-fortran.exe: overhead.F90 call_compute_subroutine.o marbl_io_mod.o marbl_tools_mod.o marbl_netcdf_mod.o marbl_mpi_mod.o
	gfortran -fPIE -O2 -D_NETCDF -o  overhead-fortran.exe -g \
	  -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu  \
		overhead.F90 call_compute_subroutine.o marbl_io_mod.o marbl_tools_mod.o marbl_netcdf_mod.o marbl_mpi_mod.o \
		$(marbl_lib_path)/libmarbl-gnu.a \
		-I/opt/cray/pe/netcdf/4.9.0.13/gnu/12.3/include -L /opt/cray/pe/netcdf/4.9.0.13/gnu/12.3/lib/ -lnetcdff


call_compute_subroutine.o: call_compute_subroutine.F90 marbl_io_mod.o marbl_tools_mod.o
	gfortran -c $^ -I$(marbl_mod_path) -fPIE -O2 -lmarbl-gnu -D_NETCDF


marbl_io_mod.o: marbl_io_mod.F90 marbl_netcdf_mod.o marbl_mpi_mod.o
	gfortran -c marbl_io_mod.F90 -I$(marbl_mod_path) -fPIE -O2 -lmarbl-gnu -D_NETCDF


marbl_netcdf_mod.o: marbl_netcdf_mod.F90
	gfortran -D_NETCDF -c $^ -I$(marbl_mod_path) -fPIE -O2 -lmarbl-gnu $(netcdf_flags)

%.o: %.F90
	gfortran -c $^ -I$(marbl_mod_path) -fPIE -O2 -lmarbl-gnu -D_NETCDF



overhead_chpl.exe: overhead.chpl
	chpl -lgfortran -L$(fortran_lib_path) -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu ../Marbl/MarblChapel.o $< -o $@ --ccflags -fPIE -M../Marbl

clean: 
	rm *.o *.mod *.exe