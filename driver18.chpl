use CTypes;
require "driver18.h";
extern record marbl_interop_type {
  var nz: c_int;
  var nt: c_int;
  var isalt: c_int;
  var itemp: c_int;
  var tracer_array: c_ptr(c_double);
}

extern proc compute_timestep(const ref interop_obj : marbl_interop_type);

var interop_obj: marbl_interop_type;


// populate the object to pass to Fortran
interop_obj.nz = 30;
interop_obj.nt = 10;
interop_obj.isalt = 1;
interop_obj.itemp = 2;

var tracerArray: [1..interop_obj.nt, 1..interop_obj.nz] c_double;
tracerArray = 0.0;
interop_obj.tracer_array = c_ptrTo(tracerArray);

// call out to Fortran
compute_timestep(interop_obj);

// print the updated values
for i in 1..interop_obj.nt {
  for j in 1..interop_obj.nz {
    write(tracerArray[i,j], " ");
  }
  writeln();
}