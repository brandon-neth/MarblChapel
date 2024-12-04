// Tests that setSurfaceFluxForcingValue writes out a warning correctly
// if given an invalid variable name

use Marbl;

var marblInstance : marblInteropType;
// Verify that the marbl_obj pointer is not null
assert(marblInstance.marbl_obj:int != 0);

var numLevels = 10;
var numParSubcols = 1;
var numElementsSurfaceFlux = 1;
var deltaZ: [1..10] real = 10.0;
var zw: [1..10] real = + scan deltaZ;
var zt: [1..10] real = 0.0;
for i in 2..10 {
  zt[i] = (zw[i-1] + zw[i]) * 0.5;
}
var activeLevelCount = 8;
marblInstance.initMarblInstance(numLevels, numParSubcols, 
  numElementsSurfaceFlux, deltaZ, zw, zt, activeLevelCount);

var surfaceSalinity = 32.80;

marblInstance.setSurfaceFluxForcingValue("Incorrect name", surfaceSalinity);