This README describes how to use the MARBL interop Chapel library. This README, and the library, assumes you are using version 0.45.0.


# Step 0: Building with `-fPIE`

Depending on the system you are using, you may need to compile with the `-fPIE` throughout this process. If, when compiling your Chapel program, you see something like the following:
```
symbol `__marbl_interface_MOD___vtab_marbl_interface_Marbl_interface_class' can not be used when making a PIE object; recompile with -fPIE
/usr/lib64/gcc/x86_64-suse-linux/12/../../../../x86_64-suse-linux/bin/ld: failed to set dynamic section sizes: bad value
clang++: error: linker command failed with exit code 1 (use -v to see invocation)
error: Make Binary - Linking
make: *** [Makefile:14: test1.exe] Error 1
```
Then you are in this unfortunate crowd.

There are 3 compilations that need to be passed the `-fPIE` flag:
- The MARBL library build
- The interop layer Fortran build
- Your Chapel program build

### Building MARBL with `-fPIE`

To build MARBL with this flag, you'll need to modify the `Makefile` within MARBL's `src` directory. The exact command you will need to modify depends on which compiler you are using, and is most likely the `gnu` rule on line 111.

To the end of the `FCFLAGS` definition, you need to add `-fPIE`.
So the gnu line will change from
```
$(MAKE) -f $(MAKE_DIR)/Makefile $(LIB_DIR)/libmarbl-gnu$(MPISUF).a USE_DEPS=TRUE FC=gfortran FCFLAGS="-Wall -Wextra -Wno-compare-reals -Werror -O2 -ffree-form -J $(OBJ_DIR)/gnu$(MPISUF) -cpp" OBJ_DIR=$(OBJ_DIR)/gnu$(MPISUF) INC_DIR=$(INC_DIR)/gnu$(MPISUF)
```
to
```
$(MAKE) -f $(MAKE_DIR)/Makefile $(LIB_DIR)/libmarbl-gnu$(MPISUF).a USE_DEPS=TRUE FC=gfortran FCFLAGS="-Wall -Wextra -Wno-compare-reals -Werror -O2 -ffree-form -J $(OBJ_DIR)/gnu$(MPISUF) -cpp -fPIE" OBJ_DIR=$(OBJ_DIR)/gnu$(MPISUF) INC_DIR=$(INC_DIR)/gnu$(MPISUF)
```

Make sure you've added `-fPIE` to the __inside__ of the quoted string.

### Interop Layer with `-fPIE`

For the interop layer (Step 2 below), add `-fPIE` to the end of the `gfortran` command.

### Chapel Program with `-fPIE`

For your Chapel program (Step 3 below), add `--cflags -fPIE` to your `chpl` command.

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

Depending on your system, you may also need to link against the Fortran standard library. There will be two flags to add here, on `-l*` and one `-L*` but the specifics will depend on your system and how you have installed your Fortran compiler. Before searching for those paths, try adding `-lgfortran`, as many systems have the library already in the search path.
