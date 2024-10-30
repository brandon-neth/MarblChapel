use CTypes;
use Random;

extern proc updateKinematics(const ref numColumns: c_int, 
                             const ref columnLength: c_int, 
                             data: c_ptr(c_double));

var data: [1..3,1..10] real;

data[1,..] = 0.0;
fillRandom(data[2,..]);
fillRandom(data[3,..]);

for i in 1..10 {
  writeln("Timestep ", i);
  writeln("p: ", data[1,..]);
  writeln("v: ", data[2,..]);
  writeln("a: ", data[3,..]);
  updateKinematics(3:c_int, 10:c_int, c_ptrTo(data));
}
