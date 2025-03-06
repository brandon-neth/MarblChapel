marbl_home = ${HOME}/MARBL
marbl_mod_path = $(marbl_home)/include/gnu
marbl_lib_path= $(marbl_home)/lib

fortran_lib_path = /opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/

clean: 
	rm *.o *.mod *.exe

%.exe: %.f90
	gfortran -I$(marbl_mod_path) -g $^ -fPIE -O2 -o $@ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu
