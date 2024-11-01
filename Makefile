marbl_home = ${HOME}/MARBL-marbl0.45.0
marbl_mod_path = $(marbl_home)/include/gnu
marbl_lib_path= $(marbl_home)/lib

clean: 
	rm *.o *.mod *.exe

driver1.exe: driver1.f90
	gfortran driver1.f90 -o $@

driver2.exe: driver2.f90
	gfortran driver2.f90 -o $@

driver3.exe: driver3Lib.f90 driver3.chpl driver3Lib.h
	gfortran -c driver3Lib.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ driver3Lib.h driver3Lib.o driver3.chpl -o $@

driver4.exe: driver4.chpl driver4Lib.h driver4Lib.f90
	gfortran -c driver4Lib.f90
	chpl driver4Lib.h driver4Lib.o driver4.chpl -o $@

driver5.exe: driver5.chpl driver5.h driver5.f90
	gfortran -c driver5.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ driver5.h driver5.o driver5.chpl -o $@

driver6.exe: driver6.chpl driver6.h driver6.f90
	gfortran -c driver6.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ driver6.h driver6.o driver6.chpl -o $@

driver7.exe: driver7.chpl driver7.h driver7.f90
	gfortran -c driver7.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ driver7.h driver7.o driver7.chpl -o $@

driver8.exe: driver8.chpl driver8.h driver8.f90
	gfortran -c driver8.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ driver8.h driver8.o driver8.chpl -o $@


driver9.exe: driver9.chpl driver9.h driver9.f90
	gfortran -c driver9.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ driver9.h driver9.o driver9.chpl -o $@


driver10.exe: driver10.f90
	gfortran -I$(marbl_mod_path) -L$(marbl_lib_path) driver10.f90 -lmarbl-gnu

driver11.exe: driver11.f90
	gfortran -I$(marbl_mod_path) -L$(marbl_lib_path) driver11.f90 -lmarbl-gnu -o $@
	
driver11.o: driver11.f90
	gfortran -c -I$(marbl_mod_path) -L$(marbl_lib_path) driver11.f90 -lmarbl-gnu -o $@

chpl_driver11.exe: driver11.chpl driver11.h driver11.o
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver11.h driver11.o driver11.chpl -o


driver12.exe: driver12.f90
	gfortran -I$(marbl_mod_path) -L$(marbl_lib_path) driver12.f90 -lmarbl-gnu -o $@


driver13.exe: driver13.f90
	gfortran -I$(marbl_mod_path) -L$(marbl_lib_path) driver13.f90 -lmarbl-gnu -o $@

chpl_driver13.exe: driver13.chpl driver13.h driver13.o
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver13.h driver13.o driver13.chpl -o $@

driver14.exe: driver14.f90 driver14.h driver14.c
	gfortran -c driver14.f90
	gcc -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver14.o driver14.c -o $@


driver15.exe: driver15.f90 driver15.h driver15.c
	gfortran -c driver15.f90
	gcc -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver15.o driver15.c -o $@

driver16.exe: driver16.f90 driver16.h driver16.c
	gfortran -c driver16.f90
	gcc -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver16.o driver16.c -o $@

driver17.exe: driver17.f90 driver17.h driver17.c
	gfortran -c driver17.f90
	gcc -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver17.o driver17.c -o $@


driver18.exe: driver18.f90 driver18.h driver18.chpl
	gfortran -c driver18.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver18.o driver18.chpl -o $@


driver19.exe: driver19.f90 driver19.h driver19.chpl
	gfortran -c driver19.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver19.o driver19.chpl -o $@


driver20.exe: driver20.f90 driver20.h driver20.chpl
	gfortran -I$(marbl_mod_path) -L$(marbl_lib_path) -c driver20.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver20.o driver20.chpl -o $@


driver21.exe: driver21.f90 driver21.h driver21.chpl
	gfortran -I$(marbl_mod_path) -L$(marbl_lib_path) -c driver21.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver21.o driver21.chpl -o $@

driver22.exe: driver22.chpl
	chpl `pkg-config --libs netcdf` `pkg-config --cflags netcdf` driver22.chpl -o driver22.exe

driver23.exe: driver23.f90
	gfortran -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu $^ -o $@


driver24.exe: driver24.f90 driver24.h driver24.chpl
	gfortran -I$(marbl_mod_path) -L$(marbl_lib_path) -c driver24.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver24.o driver24.chpl -o $@
