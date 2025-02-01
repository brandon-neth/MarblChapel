
fortran_lib_path = /opt/homebrew/Cellar/gcc/14.2.0_1/lib/gcc/14/

clean:
	rm -f *.o *.exe *.mod *.out
	
%.o: %.f90
	gfortran -c $^

%.exe: %.c %.o
	gcc -lgfortran -L$(fortran_lib_path) -o $@ $^
