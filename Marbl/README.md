This README describes how to use the MARBL interop Chapel library.

# Step 1: Building MARBL

The first step is to build the MARBL Fortran library. 
This is relatively straightforward, 

```
git clone  https://github.com/marbl-ecosys/MARBL.git
cd MARBL
git checkout marbl0.45.0
cd src
make -j8
```

This will create the module files and the library file for the MARBL library. 
The library file (`libmarbl-gnu.a`) is found in the `lib` directory of the top-level MARBL directory. 
The module files are found in the `include/gnu` directory of the top-level MARBL directory. 
**These are important directories, and you should set environment variables for these two directories**. 
This guide uses `MARBL_LIB_PATH` and `MARBL_INCLUDE_PATH`, respectively.
If you are still in the source directory, the following commands will do this for you:
```
cd ../lib
export MARBL_LIB_PATH=`pwd`
cd ../include/gnu
export MARBL_INCLUDE_PATH=`pwd`
```

### Troubleshooting Building MARBL

If the make command fails and you see a line in the output that says 
```
env: python: No such file or directory
```
This is usually caused by your system using `python3` instead of python, and can be solved by activating an anaconda environment. 

# Step 2: Building the Interop Library Code

Next, we need to compile the Fortran portions of the interoperability library.
This invokes your fortran compiler. This README uses `gfortran`, but your system may differ. 

From this directory, with the environment variables set as described above, run the command 
```
gfortran -I$MARBL_INCLUDE_PATH -c MarblChapel.f90
```
This will create two files: `MarblChapel.o` and `marblchapel.mod`. 

You will need this directory as an environment variable as well. To do this, run the following command:
```
export CHPL_MARBL_PATH=`pwd`
```

# Step 3: Compile Your Code With the `Marbl` Module

Now you are ready to compile your program using the interop library.

You'll need to add the following flags and arguments to your `chpl` compilation command:
```
-M$CHPL_MARBL_PATH $CHPL_MARBL_PATH/MarblChapel.o -lmarbl-gnu -L$MARBL_LIB_PATH
```

The `-M$CHPL_MARBL_PATH` flag tells the Chapel compiler where to look for included modules, in this case the `Marbl.chpl` file.
The `$CHPL_MARBL_PATH/MarblChapel.o` argument compiles the Fortran code from step 2 into your program.
The `-lmarbl-gnu` and `-L$MARBL_LIB_PATH` tell the compiler to include the MARBL library itself and also where to look to find it.

Depending on your system, you may also need to link against the Fortran standard library. There will be two flags to add here, on `-l*` and one `-L*` but the specifics will depend on your system and how you have installed your Fortran compiler. 
