marbl_home = ${HOME}/MARBL-marbl0.45.0
marbl_mod_path = $(marbl_home)/include/gnu
marbl_lib_path= $(marbl_home)/lib

fortran_lib_path = /opt/homebrew/Cellar/gcc/14.2.0_1/lib/gcc/14/

clean: 
	rm *.o *.mod *.exe

MarblChapel.o: MarblChapel.f90
	gfortran -I$(marbl_mod_path) -c MarblChapel.f90

test%.exe: test%.chpl Marbl.chpl MarblChapel.h MarblChapel.o
	chpl -lgfortran -L$(fortran_lib_path) -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu MarblChapel.o $< -o $@