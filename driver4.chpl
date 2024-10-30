use CTypes;

/*
// extern declaration of a function in exampleLib.f90
extern proc myfunc(formal1 : real, formal2 : c_int) : real;
var answer = myfunc(2.0, 3);
writeln("answer should be 8.0, answer = ", answer);
*/
// array example
var D = 0..9;
extern proc initArray(arr: c_ptr(c_double),
                      const ref size: c_int,
                      const ref val: c_double);

var arr : [D] c_double;

initArray(c_ptrTo(arr), arr.size:c_int, 42.0:c_double);
writeln("arr = ", arr);