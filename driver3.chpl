use CTypes;

proc main() {
var D = {1..10};

var temperature : [D] c_double;
var salinity : [D] c_double;
var oxygen : [D] c_double;

var uwind : c_double;
var vwind : c_double;

extern proc init_driver( temp: c_ptr(c_double), salinity: c_ptr(c_double), 
                        oxygen: c_ptr(c_double), const ref length: c_int, const ref uwind: c_double, const ref vwind: c_double): c_int;

for i in D {
  temperature[i] = i + 273.15;
  salinity[i] = i / 100.0;
  oxygen[i] = 10.0 - i*.3;
}

uwind = 9.8;
vwind = 1.2;
writeln("calling to fortran");
var x: c_int = init_driver(c_ptrTo(temperature), c_ptrTo(salinity), c_ptrTo(oxygen),10: c_int, uwind,vwind);
}
