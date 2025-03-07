marbl_home = ${HOME}/MARBL
marbl_mod_path = $(marbl_home)/include/gnu
marbl_lib_path= $(marbl_home)/lib

fortran_lib_path = /opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/

clean: 
	rm *.o *.mod *.exe


test.o: test.f90
	gfortran -cpp -g -I$(marbl_mod_path) -c test.f90 -fPIE -O2 -lmarbl-gnu

fortranTest.exe: test.f90
	gfortran -cpp -DCOMPILE_MAIN -I$(marbl_mod_path) -g $^ -fPIE -O2 -o $@ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu

chapelTest.exe: chapelTest.chpl ../Marbl/MarblChapel.o
	chpl -g -lgfortran -L$(fortran_lib_path) -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu ../Marbl/MarblChapel.o $< -o $@ -M../Marbl --ccflags -fPIE --fast --savec debug --codegen

cTest.o: cTest.c
	cc -g -c $^ -o $@ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu
cTest.exe: cTest.o test.o
	gfortran -g $^ -o $@ -I$(marbl_mod_path) -lmarbl-gnu -L$(marbl_lib_path) -fPIE -O2

../Marbl/MarblChapel.o: ../Marbl/MarblChapel.f90
	gfortran -g -c $^ -o $@ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu -fPIE -O2

.PHONY: runAll
runAll: fortranTest.exe chapelTest.exe cTest.exe
	/usr/bin/time -f "%e seconds" ./fortranTest.exe pointer 1000
	/usr/bin/time -f "%e seconds" ./fortranTest.exe allocatable 1000
	/usr/bin/time -f "%e seconds" ./cTest.exe pointer 1000
	/usr/bin/time -f "%e seconds" ./cTest.exe allocatable 1000
	/usr/bin/time -f "%e seconds" ./chapelTest.exe --n=1000