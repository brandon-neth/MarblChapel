clean:
	rm -f driver1 driver2 driver3 driver4 driver5 
	rm -f *.o *.mod

driver1: driver1.f90
	gfortran -o driver1 driver1.f90


driver2: driver2.f90
	gfortran -o driver2 driver2.f90

driver3: driver3Lib.f90 driver3.chpl driver3Lib.h
	gfortran -c driver3Lib.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ driver3Lib.h driver3Lib.o driver3.chpl

driver4: driver4.chpl driver4Lib.h driver4Lib.f90
	gfortran -c driver4Lib.f90
	chpl driver4Lib.h driver4Lib.o driver4.chpl

driver5: driver5.chpl driver5.h driver5.f90
	gfortran -c driver5.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ driver5.h driver5.o driver5.chpl


driver6: driver6.chpl driver6.h driver6.f90
	gfortran -c driver6.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ driver6.h driver6.o driver6.chpl

driver7: driver7.chpl driver7.h driver7.f90
	gfortran -c driver7.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ driver7.h driver7.o driver7.chpl

driver8: driver8.chpl driver8.h driver8.f90
	gfortran -c driver8.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ driver8.h driver8.o driver8.chpl


driver9: driver9.chpl driver9.h driver9.f90
	gfortran -c driver9.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ driver9.h driver9.o driver9.chpl


marbl_home = ${HOME}/MARBL-marbl0.45.0
marbl_mod_path = $(marbl_home)/include/gnu
marbl_lib_path= $(marbl_home)/lib
driver10: driver10.f90
	gfortran -I$(marbl_mod_path) -L$(marbl_lib_path) driver10.f90 -lmarbl-gnu

driver11: driver11.f90
	gfortran -I$(marbl_mod_path) -L$(marbl_lib_path) driver11.f90 -lmarbl-gnu -o driver11
	
driver11.o: driver11.f90
	gfortran -c -I$(marbl_mod_path) -L$(marbl_lib_path) driver11.f90 -lmarbl-gnu -o driver11.o

chpl_driver11: driver11.chpl driver11.h driver11.o
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver11.h driver11.o driver11.chpl -o chpl_driver11


driver12: driver12.f90
	gfortran -I$(marbl_mod_path) -L$(marbl_lib_path) driver12.f90 -lmarbl-gnu -o driver12


driver13: driver13.f90
	gfortran -I$(marbl_mod_path) -L$(marbl_lib_path) driver13.f90 -lmarbl-gnu -o driver13

chpl_driver13: driver13.chpl driver13.h driver13.o
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver13.h driver13.o driver13.chpl -o chpl_driver13

driver14: driver14.f90 driver14.h driver14.c
	gfortran -c driver14.f90
	gcc -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver14.o driver14.c -o driver14


driver15: driver15.f90 driver15.h driver15.c
	gfortran -c driver15.f90
	gcc -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver15.o driver15.c -o driver15

driver16: driver16.f90 driver16.h driver16.c
	gfortran -c driver16.f90
	gcc -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver16.o driver16.c -o driver16

driver17: driver17.f90 driver17.h driver17.c
	gfortran -c driver17.f90
	gcc -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver17.o driver17.c -o driver17


driver18: driver18.f90 driver18.h driver18.chpl
	gfortran -c driver18.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver18.o driver18.chpl -o driver18


driver19: driver19.f90 driver19.h driver19.chpl
	gfortran -c driver19.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver19.o driver19.chpl -o driver19


driver20: driver20.f90 driver20.h driver20.chpl
	gfortran -I$(marbl_mod_path) -L$(marbl_lib_path) -c driver20.f90
	chpl -lgfortran  -L/opt/homebrew/Cellar/gcc/14.2.0/lib/gcc/14/ -I$(marbl_mod_path) -L$(marbl_lib_path) -lmarbl-gnu driver20.o driver20.chpl -o driver20