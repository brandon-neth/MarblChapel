// goal is to get the tracer sums matching
use List;
use CTypes;
use Random;

require "driver26.h";

module MarblInterop {
  use CTypes;
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

    proc surfaceFluxCompute(ref tracerArray, dt) {
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

} // module MarblInterop
use MarblInterop;

module myNetCDF {
  use NetCDF.C_NetCDF;
  use List;
  use Map;

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

  proc readAllVars(datasetFile: string, type dtype, param dimCount: int) throws {

    var ncidp: c_int; 
    nc_open(datasetFile.c_str(), 0, ncidp);

    var varCount: c_int;
    nc_inq_nvars(ncidp, varCount);

    var varNames:  list(string);
    for i in 0..#varCount {
      
      // only add variables with the correct dimensionality count
      var varDims: c_int;
      nc_inq_varndims(ncidp, i:c_int, varDims);
      if (varDims != dimCount) {
        continue;
      }

      var nameBuffer: c_ptr(c_char) = allocate(c_char, NC_MAX_NAME);
      nc_inq_varname(ncidp, i:c_int, nameBuffer);
      var varName: string =  string.createCopyingBuffer(nameBuffer);
      
      // only add variables with the correct shape
      var data = readVar(datasetFile,  varName, dtype, dimCount);
      if data.shape != (1,100,26,26) {
        continue;
      }
      // temp and salt are their own
      if varName == "temp" || varName == "salt" then continue;

      varNames.pushBack( varName); 
    }
    nc_close(ncidp);

    if varNames.size == 0 then throw new Error("no variables of that size");

    type arrayType = readVar(datasetFile, varNames[0], dtype, dimCount).type;
    var datas: list(arrayType);

    for varName in varNames {
      var data = readVar(datasetFile, varName, dtype, dimCount);
      datas.pushBack(data);
      writeln(varName, " tracer sum: ", + reduce data);
    }
    return datas;
  }

  proc tracerNames(datasetFile: string, type dtype, shape: ?dimCount*int) throws {
    writeln("Collecting tracer names");
    var ncidp: c_int; 
    nc_open(datasetFile.c_str(), 0, ncidp);

    var varCount: c_int;
    nc_inq_nvars(ncidp, varCount);

    var varNames:  list(string);
    for i in 0..#varCount {
      // only add variables with the correct dimensionality count
      var varDims: c_int;
      nc_inq_varndims(ncidp, i:c_int, varDims);
      if (varDims != dimCount) {
        continue;
      }

      var nameBuffer: c_ptr(c_char) = allocate(c_char, NC_MAX_NAME);
      nc_inq_varname(ncidp, i:c_int, nameBuffer);
      var varName: string =  string.createCopyingBuffer(nameBuffer);
      
      // only add variables with the correct shape
      var data = readVar(datasetFile,  varName, dtype, dimCount);
      if data.shape != shape {
        continue;
      }
      // temp and salt are their own
      if varName == "temp" || varName == "salt" then continue;

      varNames.pushBack( varName); 
    }
    return varNames;
  }

  /*
      Returns a map of variable names to the array for that data
  */
  proc readVars(datasetPath: string, type dtype, shape: ?dimCount*int) throws {
    var ncidp: c_int; 
    nc_open(datasetPath.c_str(), 0, ncidp);
    var varCount: c_int;
    nc_inq_nvars(ncidp, varCount);

    const D = makeRectangularDomain(1,shape);
    var varMap: map(string, [D] dtype);

    for i in 0..#varCount {
      // only add variables with the correct dimensionality count
      var varDims: c_int;
      nc_inq_varndims(ncidp, i:c_int, varDims);
      if (varDims != dimCount) {
        continue;
      }

      var nameBuffer: c_ptr(c_char) = allocate(c_char, NC_MAX_NAME);
      nc_inq_varname(ncidp, i:c_int, nameBuffer);
      var varName: string =  string.createCopyingBuffer(nameBuffer);
      deallocate(nameBuffer);

      var data = readVar(datasetPath,  varName, dtype, dimCount);
      if data.shape != shape {
        continue;
      }

      varMap[varName] = data;
    }
    return varMap;
  }
}

use myNetCDF;

module Geometry {
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
}
use Geometry;

//
// Step 1: Build Geometry
//


// h is the bathymetric depth in roms_grd.nc (attached) //
var h = readVar("roms_grd.nc", "h", c_double, 2);

var H0 = get_H0(h);

// Read in sea surface height ("zeta") from roms_ini_MARBL.nc //
var zetaWithTime = readVar("roms_ini_MARBL.nc", "zeta", c_double, 3);
var zeta = zetaWithTime[1,..,..];

var H: [H0.domain] real;
var z_w: [0..Nz, H.domain.dim[1], H.domain.dim[2]] real = 0.0; // interface heights
var z_r: [H.domain] real; // cell centers

forall (k,j,i) in H.domain {
  H[k,j,i] = H0[k,j,i] * (1 + zeta[j,i] / h[j,i]);
}

for z in 0..<Nz {
  forall x in H.domain.dim[1] {
    for y in H.domain.dim[2] {
      z_w[z+1,x,y] = z_w[z,x,y] + H[z,x,y];
      z_r[z,x,y] = (z_w[z,x,y] + z_w[z+1,x,y]) / 2;
    }
  }
}

//
// Step 2. Prepare Surface Values
// 

var dust = readVar("roms_frc_bgc.nc", "dust", c_float, 3);
var pco2_air = readVar("roms_frc_bgc.nc", "pco2_air", c_float, 3);
var pco2_air_alt =readVar("roms_frc_bgc.nc", "pco2_air_alt", c_float, 3);
var nox = readVar("roms_frc_bgc.nc", "nox", c_float, 3);
var nhy = readVar("roms_frc_bgc.nc", "nhy", c_float, 3);
var fesedflux = readVar("roms_frc_bgc.nc", "fesedflux", c_float, 3);


// 
// Step 3. Prepare Tracer Values
//

var tracerShapedVars = readAllVars("roms_ini_MARBL.nc", c_float, 4);
var varNames = tracerNames("roms_ini_MARBL.nc", c_float, (1,100,26,26));

var tracerWithTimeDomain = tracerShapedVars[0].domain;
var tracerArrayDomain = {tracerWithTimeDomain.dim[3], tracerWithTimeDomain.dim[2], 1..tracerShapedVars.size, tracerWithTimeDomain.dim[1]};

var tracerArray: [tracerArrayDomain] real;
for (x,y,t,z) in tracerArrayDomain {
  tracerArray[x,y,t,z] = tracerShapedVars[t-1][1,z,y,x];
}

var tempWithTime = readVar("roms_ini_MARBL.nc", "temp", c_float, 4);
var saltWithTime = readVar("roms_ini_MARBL.nc", "salt", c_float, 4);

var temperature: [tracerWithTimeDomain.dim[3], tracerWithTimeDomain.dim[2], tracerWithTimeDomain.dim[1]] real;
var salinity: [tracerWithTimeDomain.dim[3], tracerWithTimeDomain.dim[2], tracerWithTimeDomain.dim[1]] real;
for (x,y,z) in temperature.domain {
  temperature[x,y,z] = tempWithTime[1,z,y,x];
  salinity[x,y,z] = saltWithTime[1,z,y,x];
}

//
// Step 4. Pick a column and prepare the tracer data
var x = 1;
var y = 1;
var columnTracerView = tracerArray[x,y,..,..];
var columnTracers: [tracerArray.dim[2],tracerArray.dim[3]] real;
var originalTracers: [tracerArray.dim[2],tracerArray.dim[3]] real;
for i in originalTracers.domain {
  columnTracers[i] = columnTracerView[i];
  originalTracers[i] = columnTracers[i];
}

writeln("Column sums");
for t in 0..<columnTracers.shape[0] {
  writeln(varNames[t], ": ", + reduce columnTracers[t+1,..]);
}


//
// Step 5. Start Computing
//

var marblWrapper: marbl_interop_type;
marblWrapper.initMarblInstance(Nz, 1, 1, H[x,y,..], z_w[x,y,..], z_r[x,y,..]);


marblWrapper.setSurfaceFluxForcingValues(Nz, columnTracers.shape[0], columnTracers, salinity[x,y,1], temperature[x,y,1], 10,0, 10.0, pco2_air[x,y,1], pco2_air_alt[x,y,1], dust[x,y,1], fesedflux[x,y,1], nox[x,y,1], nhy[x,y,1]);

marblWrapper.surfaceFluxCompute(columnTracers, 1.0);


marblWrapper.setInteriorTendencyForcingValues(Nz, columnTracers.shape[0], columnTracers, dust[1,1,1], 1.0, 1.0, 1.0, temperature[x,y,..], salinity[x,y,..], z_r);

marblWrapper.interiorTendencyCompute(columnTracers, 1.0);

var nt = 32;
var tracerChanged: [1..nt] bool;
for (t,z) in columnTracers.domain {
  if columnTracers(t,z) != originalTracers(t,z) then tracerChanged[t] = true;
}


for i in 1..nt do
  if tracerChanged[i] then writeln("Tracer changed with idx: ", i);
for i in 1..nt do
  if !tracerChanged[i] then writeln("No tracer changed with idx: ", i);

