// Passing slices of an array to a Fortran subroutine

use CTypes;
use Random;


extern proc applyCurrent(const ref length: c_int, velocities: c_ptr(c_double), 
                         accelerations: c_ptr(c_double), const ref dt: c_double);


var data: [1..3,1..10] real;
var dt = 1.0;

data[1,..] = 0.0;
fillRandom(data[2,..]);
fillRandom(data[3,..]);
forall a in data[1,..] do a = a - a:int;

for i in 1..10 {
  writeln("Timestep ", i);
  writeln("p: ", data[1,..]);
  writeln("v: ", data[2,..]);
  writeln("a: ", data[3,..]);
  applyCurrent(10:c_int, c_ptrTo(data[2,..]), c_ptrTo(data[3,..]), dt:c_double);
  data[1,..] += dt * data[2,..];
}
