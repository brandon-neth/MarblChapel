use List;
use CTypes;
use Random;

require "driver25.h";
extern record marbl_interop_type {
  var marbl_instance_obj: c_ptr(void);
  proc init() {
    writeln("marbl_interop_type init");
    init this;
    init_interop_obj(this);
  }
  
  proc initMarblInstance(const ref gcm_num_levels, const ref gcm_num_PAR_subcols, const ref gcm_num_elements_surface_flux, ref gcm_delta_z, ref gcm_zw, ref gcm_zt) {
    init_marbl_instance(this, gcm_num_levels: c_int, gcm_num_PAR_subcols: c_int, gcm_num_elements_surface_flux: c_int, c_ptrTo(gcm_delta_z), c_ptrTo(gcm_zw), c_ptrTo(gcm_zt));
  }

  proc setSurfaceFluxForcingValues(const ref nz, const ref nt, ref tracerArray, const ref salinity, const ref temperature, const ref uwind, const ref vwind, const ref pressure, const ref pco2air, const ref pco2air_alt, const ref dust, const ref fe, const ref nox, const ref nhy) {
    set_surface_flux_forcing_values(this, nz: c_int, nt: c_int, c_ptrTo(tracerArray), salinity: c_double, temperature: c_double, uwind: c_double, vwind: c_double, pressure: c_double, pco2air: c_double, pco2air_alt: c_double, dust: c_double, fe: c_double, nox: c_double, nhy: c_double);
  }

  proc surfaceFluxCompute(tracerArray, dt) {
    compute_surface_fluxes(this);

    var nt, nz: int;
    (nt,nz) = tracerArray.shape;
    update_surface_fluxes(this, nz: c_int, nt: c_int, c_ptrTo(tracerArray), dt: c_double);
  }

  proc setInteriorTendencyForcingValues(const ref nz, const ref nt, ref tracerArray, 
    const ref dust, const ref srflx, const ref Cp, const ref rho0,
    ref temperature, ref salinity, ref z_r) {
    set_interior_tendency_forcing_values(this, nz: c_int, nt: c_int, c_ptrTo(tracerArray), 
    dust: c_double, srflx: c_double, Cp: c_double, rho0: c_double,
    c_ptrTo(temperature), c_ptrTo(salinity), c_ptrTo(z_r));
    }
  proc interiorTendencyCompute(tracerArray, dt) {
    compute_interior_tendencies(this);

    var nt, nz: int;
    (nt,nz) = tracerArray.shape;
    update_interior_tendencies(this, nz: c_int, nt: c_int, c_ptrTo(tracerArray), dt: c_double);
  }
}

extern proc init_interop_obj(const ref marblWrapper: marbl_interop_type);
extern proc init_marbl_instance(const ref marblWrapper: marbl_interop_type, const ref gcm_num_levels: c_int, const ref gcm_num_PAR_subcols: c_int, const ref gcm_num_elements_surface_flux: c_int, 
  gcm_delta_z: c_ptr(c_double), gcm_zw: c_ptr(c_double), gcm_zt: c_ptr(c_double));

extern proc set_surface_flux_forcing_values(
  const ref interop_obj: marbl_interop_type, const ref nz: c_int, 
  const ref nt: c_int, tracer_array: c_ptr(c_double), const ref sss: c_double,  
  const ref sst: c_double,  const ref uwnd: c_double, const ref vwnd: c_double,  
  const ref atmpress: c_double, const ref pco2air: c_double,  
  const ref pco2air_alt: c_double,  const ref dust: c_double, 
  const ref fe: c_double, const ref nox: c_double, const ref nhy: c_double);

extern proc compute_surface_fluxes(const ref interop_obj: marbl_interop_type);
extern proc update_surface_fluxes(const ref interop_obj: marbl_interop_type, const ref nz: c_int, const ref nt: c_int, tracer_array: c_ptr(c_double), const ref dt: c_double);

extern proc set_interior_tendency_forcing_values(const ref interop_obj: marbl_interop_type, 
  const ref nz: c_int, const ref nt: c_int, tracer_array: c_ptr(c_double),
  const ref dust: c_double, const ref srflx: c_double, const ref Cp: c_double,
  const ref rho0: c_double, const ref temperature: c_ptr(c_double),
  const ref salinity: c_ptr(c_double), const ref z_r: c_ptr(c_double));


extern proc compute_interior_tendencies(const ref interop_obj: marbl_interop_type);
extern proc update_interior_tendencies(const ref interop_obj: marbl_interop_type, const ref nz: c_int, const ref nt: c_int, tracer_array: c_ptr(c_double), const ref dt: c_double);


use NetCDF.C_NetCDF;

proc readVar(datasetFile: string, varName: string, type dtype, param dimCount: int) {

  var ncidp: c_int; 
  nc_open(datasetFile.c_str(), 0, ncidp);

  var var_id: c_int;

  nc_inq_varid(ncidp, varName.c_str(), var_id);

  
  var dims: [0..#dimCount] c_int;

  nc_inq_vardimid(ncidp, var_id, dims[0]);

  var dimLengths: dimCount * c_size_t;
  for i in 0..#dimCount {

    nc_inq_dimlen(ncidp, dims[i], dimLengths[i]);
  }

  var typedDimLengths: dimCount * int;
  for i in 0..#dimCount do typedDimLengths[i] = dimLengths[i] : int;

  var D = makeRectangularDomain(1, typedDimLengths);
  var data: [D] dtype;

  var firstIndex: dimCount * c_int;
  for i in 0..#dimCount {
    firstIndex[i] = 1;
  }

  nc_get_var(ncidp, var_id, c_ptrTo(data));

  nc_close(ncidp);
  return data;
}


config const theta_s: real = 5.0;
config const theta_b: real = 2.0;
config const hc: real = 300.0;
config const Nz = 100;
use Math;

proc Cs(ref sigma: [?D] real) {

  var C = (1 - cosh(theta_s * sigma)) / (cosh(theta_s) - 1);
  var C2 = (exp(theta_b * C) - 1) / (1 - exp(-theta_b));

  return C2;
}

proc get_H0(ref h: [?D] real) {

  var k_w : [0..Nz] int;

  for idx in k_w.domain {
    k_w[idx] = idx;
  }

  var sigma_w = (k_w - Nz) / (1.0*Nz);

  var Cs_w = Cs(sigma_w);

  var z_w : [0..Nz, D.dim[0], D.dim[1]] real;
  var H0 : [0..<Nz, D.dim[0], D.dim[1]] real;

  forall (k,j,i) in z_w.domain {
    z_w[k,j,i] = h[j,i] * (hc*sigma_w[k] + h[j,i]*Cs_w[k]) / (hc + h[j,i]);
  }

  forall (k,j,i) in H0.domain {
    H0[k,j,i] = z_w[k+1,j,i] - z_w[k,j,i];
  }

  return H0;
}



var h = readVar("roms_grd.nc", "h", c_double, 2);

// h is the bathymetric depth in roms_grd.nc (attached) //
var H0 = get_H0(h);

var zeta3D = readVar("roms_ini_MARBL.nc", "zeta", c_double, 3);
var zeta = zeta3D[1,..,..];
 // Read in sea surface height ("zeta") from roms_ini_MARBL.nc //
var H: [H0.domain] real;
forall (k,j,i) in H.domain {
  H[k,j,i] = H0[k,j,i] * (1 + zeta[j,i] / h[j,i]);
}

var z_w: [0..Nz, H.domain.dim[1], H.domain.dim[2]] real = 0.0; // interface heights
var z_r: [H.domain] real; // cell centers


writeln("H domain: ", H.domain);
writeln("cell heights at 0,0:");
writeln(H[..,1,1]);


for z in 0..<Nz {
  forall x in H.domain.dim[1] {
    for y in H.domain.dim[2] {
      z_w[z+1,x,y] = z_w[z,x,y] + H[z,x,y];
      z_r[z,x,y] = (z_w[z,x,y] + z_w[z+1,x,y]) / 2;
    }
  }
}

writeln("interface heights:");
writeln(z_w[..,1,1]);

writeln("cell centers: ");
writeln(z_r[..,1,1]);

var actualH: [H.domain.dim[2], H.domain.dim[1], H.domain.dim[0]] real;
forall (z,y,x) in H.domain do actualH[x,y,z] = H[z,y,x];

var actual_z_w: [z_w.domain.dim[2], z_w.domain.dim[1], z_w.domain.dim[0]] real;
forall (z,y,x) in z_w.domain do actual_z_w[x,y,z] = z_w[z,y,x];


var actual_z_r: [z_r.domain.dim[2], z_r.domain.dim[1], z_r.domain.dim[0]] real;
forall (z,y,x) in z_r.domain do actual_z_r[x,y,z] = z_r[z,y,x];

proc readAllVars(datasetFile: string, type dtype, param dimCount: int) {

  var ncidp: c_int; 
  nc_open(datasetFile.c_str(), 0, ncidp);

  var varCount: c_int;
  nc_inq_nvars(ncidp, varCount);

  var varNames:  list(string);

  for i in 0..#varCount {
    var varName: c_ptr(c_char);
    varName = allocate(c_char, NC_MAX_NAME);
    nc_inq_varname(ncidp, i:c_int, varName);
    var stringName = string.createCopyingBuffer(varName);
    deallocate(varName);
    // only add variables with the correct dimensionality count
    var varDims: c_int;
    nc_inq_varndims(ncidp, i:c_int, varDims);
    if (varDims != dimCount) {
      continue;
    }
    // only add variables with the correct shape
    var data = readVar(datasetFile, stringName, dtype, dimCount);
    if data.shape != (1,100,26,26) {
      continue;
    }

    
    varNames.pushBack(stringName);
    
  }

  for variable in varNames do writeln("Var name: ", variable);

  nc_close(ncidp);

  var exampleData = readVar(datasetFile, varNames[0], dtype, dimCount);
  type arrayType = exampleData.type;
  writeln("array type: ", arrayType:string);

  var datas: list(arrayType);

  for varName in varNames {
    var data = readVar(datasetFile, varName, dtype, dimCount);
    writeln("Data for ", varName, ": ", data.shape, ". Sum: ", + reduce data);
    datas.pushBack(data);

  }
  writeln("There were ", varNames.size, " variables with ", dimCount, " dimensions.");
  return datas;
}

var dust = readVar("roms_frc_bgc.nc", "dust", c_float, 3);
var pco2_air = readVar("roms_frc_bgc.nc", "pco2_air", c_float, 3);
var pco2_air_alt =readVar("roms_frc_bgc.nc", "pco2_air_alt", c_float, 3);
var nox = readVar("roms_frc_bgc.nc", "nox", c_float, 3);
var nhy = readVar("roms_frc_bgc.nc", "nhy", c_float, 3);
var fesedflux = readVar("roms_frc_bgc.nc", "fesedflux", c_float, 3);

var tracerDataList = readAllVars("roms_ini_MARBL.nc", c_float, 4);
var exampleTracerData = tracerDataList[0];

writeln("exampleTracerData type: ", exampleTracerData.type : string);
var tracerDomain = exampleTracerData.domain;

var tracerArray: [1..tracerDataList.size, tracerDomain.dim[0], tracerDomain.dim[1], tracerDomain.dim[2], tracerDomain.dim[3]] real;

var nt = tracerDataList.size;
writeln("tracer array domain: ", tracerArray.domain);

var passableTracers: [tracerArray.domain.dim[3],tracerArray.domain.dim[4],tracerArray.domain.dim[0],tracerArray.domain.dim[2]] real;

forall (x,y,t,z) in passableTracers.domain {
  passableTracers[x,y,t,z] = tracerDataList[t-1][1,z,y,x];
}

var tempFull = readVar("roms_ini_MARBL.nc", "temp", c_float, 4);
var salinityFull = readVar("roms_ini_MARBL.nc", "salt", c_float, 4);

var temperature: [passableTracers.domain.dim[0],passableTracers.domain.dim[1],passableTracers.domain.dim[3]] real;
var salinity: [passableTracers.domain.dim[0],passableTracers.domain.dim[1],passableTracers.domain.dim[3]] real;

for (x,y,z) in temperature.domain {
  temperature[x,y,z] = tempFull[1,z,y,x];
  salinity[x,y,z] = salinityFull[1,z,y,x];
}

var marblWrapper: marbl_interop_type;

var x = 1;
var y = 1;
marblWrapper.initMarblInstance(Nz, 1, 1, actualH[x,y,..], actual_z_w[x,y,..], actual_z_r[x,y,..]);

var originalTracers: [passableTracers.domain] real;

for i in originalTracers.domain do originalTracers[i] = passableTracers[i];


marblWrapper.setSurfaceFluxForcingValues(Nz, passableTracers.shape[2], passableTracers[x,y,..,..], salinity[x,y,1], temperature[x,y,1], 10,0, 10.0, pco2_air[x,y,1], pco2_air_alt[x,y,1], dust[x,y,1], fesedflux[x,y,1], nox[x,y,1], nhy[x,y,1]);


//marblWrapper.surfaceFluxCompute(passableTracers[x,y,..,..], 1.0);

for i in originalTracers.domain {
  if originalTracers(i) != passableTracers(i) {
    writeln("Different at index: ", i, " ", originalTracers(i), " vs ", passableTracers(i));
  }
}

marblWrapper.setInteriorTendencyForcingValues(
  Nz, passableTracers.shape[2], passableTracers[x,y,..,..], dust[1,1,1], 1.0,1.0,1.0, temperature[x,y,..], salinity[x,y,..], z_r);

writeln("Executing interior compute...");

writeln("passableTracer sums:");
for i in 1..nt do writeln(+ reduce passableTracers[x,y,i,..]);

marblWrapper.interiorTendencyCompute(passableTracers[x,y,..,..], 1.0);

writeln("After interior compute: ");


var count = 0;
for i in originalTracers.domain {
  if originalTracers(i) != passableTracers(i) {
    count += 1;
    //writeln("Different at index: ", i, " ", originalTracers(i), " vs ", passableTracers(i));
  }
}
writeln("changes: ", count);

var tracerChanged: [1..nt] bool;
for (x,y,t,z) in originalTracers.domain {
  if originalTracers(x,y,t,z) != passableTracers(x,y,t,z) then tracerChanged[t] = true;
}

for i in 1..nt do
  if tracerChanged[i] then writeln("Tracer changed with idx: ", i);
for i in 1..nt do
  if !tracerChanged[i] then writeln("No tracer changed with idx: ", i);

