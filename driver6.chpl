use CTypes;
var D = 0..9;
extern proc initArrays(arr1: c_ptr(c_double), arr2: c_ptr(c_double),
                      const ref size: c_int, const ref val: c_double);

var arr1: [D] real(64) = 1.0;
var arr2: [D] real(64) = 2.0;

writeln("Calling initArrays");
initArrays(c_ptrTo(arr1), c_ptrTo(arr2), 10:c_int, 2.6:c_double);

writeln("arr1: ", arr1);
writeln("arr2: ", arr2);