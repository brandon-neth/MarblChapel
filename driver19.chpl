use CTypes;
require "driver19.h";
extern record marbl_interop_type {
  var nz: c_int;
  var nt: c_int;
  var isalt: c_int;
  var itemp: c_int;
  var tracer_array: c_ptr(c_double);
  var marbl_instance_obj: c_ptr(void);
}

extern proc link_marbl_obj(const ref interop_obj : marbl_interop_type);
extern proc use_marbl_obj(const ref interop_obj : marbl_interop_type);

var interop_obj: marbl_interop_type;


// populate the object to pass to Fortran
interop_obj.nz = 30;
interop_obj.nt = 10;
interop_obj.isalt = 1;
interop_obj.itemp = 2;


var tracerArray: [1..interop_obj.nt, 1..interop_obj.nz] c_double;
tracerArray = 0.0;
interop_obj.tracer_array = c_ptrTo(tracerArray);

link_marbl_obj(interop_obj);

writeln("Linked marbl object with interop object");

use_marbl_obj(interop_obj);

writeln("Used marbl object from void pointer in interop object.");