// trying out executing for the entire domain
use CTypes;
use Random;
use Time;

require "driver31.h";

module MarblInterop {
  use CTypes;
  var initBarrier: sync bool = true;
  extern record marbl_interop_type {
    
    var marbl_instance_obj: c_ptr(void);
    
    proc init() {
      init this;
      
      init_interop_obj(this);
      
    }
  
    proc initMarblInstance(const ref gcm_num_levels, const ref gcm_num_PAR_subcols, const ref gcm_num_elements_surface_flux, ref gcm_delta_z, ref gcm_zw, ref gcm_zt) {
      var lock = initBarrier.readFE();
      init_marbl_instance(this, gcm_num_levels: c_int, gcm_num_PAR_subcols: c_int, gcm_num_elements_surface_flux: c_int, c_ptrTo(gcm_delta_z), c_ptrTo(gcm_zw), c_ptrTo(gcm_zt));
      initBarrier.writeEF(true);
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
    proc interiorTendencyCompute(ref tracerArray, dt) {
      compute_interior_tendencies(this);

      var nt, nz: int;
      (nt,nz) = tracerArray.shape;
      update_interior_tendencies(this, nz: c_int, nt: c_int, c_ptrTo(tracerArray), dt: c_double);
    }

    proc setInteriorTendencyForcingArray(const ref nz, const ref variableName, ref data) {
      set_interior_tendency_forcing_array(this, nz: c_int, variableName.c_str(), variableName.size: c_int, c_ptrTo(data));
    }

    proc setInteriorTendencyForcingScalar(const ref variableName, const ref data) {
      set_interior_tendency_forcing_scalar(this, variableName.c_str(), variableName.size: c_int, data: c_double);
    }
    proc copyTracerValues(ref tracerArray) {
      var nt, nz: int;
      (nt,nz) = tracerArray.shape;
      copy_tracer_values(this, nz: c_int, nt: c_int, c_ptrTo(tracerArray));
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
    const ref rho0: c_double, temperature: c_ptr(c_double),
    salinity: c_ptr(c_double), z_r: c_ptr(c_double));

  extern proc set_interior_tendency_forcing_array(const ref interop_obj: marbl_interop_type, 
    const ref nz: c_int, variableName: c_ptrConst(c_char), const ref vnLen: c_int, data: c_ptr(c_double));


  extern proc set_interior_tendency_forcing_scalar(const ref interop_obj: marbl_interop_type, 
   variableName: c_ptrConst(c_char), const ref vnLen: c_int, const ref data: c_double);

  extern proc copy_tracer_values(const ref interop_obj: marbl_interop_type, 
    const ref nz: c_int, const ref nt: c_int, tracer_array: c_ptr(c_double));

  extern proc compute_interior_tendencies(const ref interop_obj: marbl_interop_type);
  extern proc update_interior_tendencies(const ref interop_obj: marbl_interop_type, const ref nz: c_int, const ref nt: c_int, tracer_array: c_ptr(c_double), const ref dt: c_double);

} // module MarblInterop
use MarblInterop;

module myNetCDF {
  use NetCDF.C_NetCDF;
  use List;
  use Map;

  proc readDim(datasetFile: string, dimName: string) {
    var ncidp: c_int; 
    nc_open(datasetFile.c_str(), 0, ncidp);

    var dim_id: c_int;

    nc_inq_dimid(ncidp, dimName.c_str(), dim_id);

    var length: c_size_t;

    nc_inq_dimlen(ncidp, dim_id, length);

    return length;
  }
  proc readVar(datasetFile: string, varName: string, type dtype, param dimCount: int) {

    var ncidp: c_int; 
    var error: c_int;
    
    error = nc_open(datasetFile.c_str(), 0, ncidp);
    if error then writeln("Failed to open netcdf file: ", datasetFile);
    var var_id: c_int;

    error = nc_inq_varid(ncidp, varName.c_str(), var_id);
    if error then writeln("Failed to find variable id for var: ", varName);
    
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
    }
    return datas;
  }

  proc tracerNames(datasetFile: string, type dtype, shape: ?dimCount*int) throws {
    //writeln("Collecting tracer names");
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

    var z_w : [0..Nz, D.dim[0], D.d(1)] real;
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


proc main() {
//
// Step 1: Read variables from the initial conditions file
//

  const ncPath = "call_compute_subroutines.20190718.nc";

  // read scalar values for each column
  var numColumns = readDim(ncPath, "column");
  //writeln("Found ", numColumns, " columns");

  var zt = readDim(ncPath, "zt");
  //writeln("Each column has ", zt, " cells");

  var ztCol = readVar(ncPath, "zt", c_double, 1);
  // scalars for each column
 // writeln("Reading scalar values...");
  var dust_flux = readVar(ncPath, "dust_flux", c_double, 1);
  var iron_flux = readVar(ncPath, "iron_flux", c_double, 1);
  var iron_sed_flux = readVar(ncPath, "iron_sed_flux", c_double, 2);
  var nox_flux = readVar(ncPath, "nox_flux", c_double, 1);
  var nhy_flux = readVar(ncPath, "nhy_flux", c_double, 1);
  var atm_co2 = readVar(ncPath, "atm_co2", c_double, 1);
  var atm_alt_co2 = readVar(ncPath, "atm_alt_co2", c_double, 1);
  var u10_sqr = readVar(ncPath, "u10_sqr", c_double, 1);

  //writeln("Reading geometry data...");
  var delta_z = readVar(ncPath, "delta_z", c_double, 1);
  var zw = readVar(ncPath, "zw", c_double, 1);
  var SST = readVar(ncPath, "SST", c_double, 1);
  var SSS = readVar(ncPath, "SSS", c_double, 1);

  //writeln("Redaing tracer values..");
  var PO4 = readVar(ncPath, "PO4", c_double, 2);
  var NO3 = readVar(ncPath, "NO3", c_double, 2);
  var SiO3 = readVar(ncPath, "SiO3", c_double, 2);
  var NH4 = readVar(ncPath, "NH4", c_double, 2);
  var Fe = readVar(ncPath, "Fe", c_double, 2);
  var Lig = readVar(ncPath, "Lig", c_double, 2);
  var O2 = readVar(ncPath, "NH4", c_double, 2);
  var DIC = readVar(ncPath, "DIC", c_double, 2);
  var DIC_ALT_CO2 = readVar(ncPath, "DIC_ALT_CO2", c_double, 2);
  var ALK = readVar(ncPath, "ALK", c_double, 2);
  var ALK_ALT_CO2 = readVar(ncPath, "ALK_ALT_CO2", c_double, 2);
  var DOC = readVar(ncPath, "DOC", c_double, 2);
  var DON = readVar(ncPath, "DON", c_double, 2);
  var DOP = readVar(ncPath, "DOP", c_double, 2);
  var DOPr = readVar(ncPath, "DOPr", c_double, 2);
  var DONr = readVar(ncPath, "DONr", c_double, 2);
  var DOCr = readVar(ncPath, "DOCr", c_double, 2);
  var zooC = readVar(ncPath, "zooC", c_double, 2);
  var spChl = readVar(ncPath, "spChl", c_double, 2);
  var spC = readVar(ncPath, "spC", c_double, 2);
  var spP = readVar(ncPath, "spP", c_double, 2);
  var spFe = readVar(ncPath, "spFe", c_double, 2);
  var spCaCO3 = readVar(ncPath, "spCaCO3", c_double, 2);
  var diatChl = readVar(ncPath, "diatChl", c_double, 2);
  var diatC = readVar(ncPath, "diatC", c_double, 2);
  var diatP = readVar(ncPath, "diatP", c_double, 2);
  var diatFe = readVar(ncPath, "diatFe", c_double, 2);
  var diatSi = readVar(ncPath, "diatSi", c_double, 2);
  var diazChl = readVar(ncPath, "diazChl", c_double, 2);
  var diazC = readVar(ncPath, "diazC", c_double, 2);
  var diazP = readVar(ncPath, "diazP", c_double, 2);
  var diazFe = readVar(ncPath, "diazFe", c_double, 2);
  

  var temperature = readVar(ncPath, "temperature", c_double, 2);
  var salinity = readVar(ncPath, "salinity", c_double, 2);
  var pressure = readVar(ncPath, "pressure", c_double, 2);
  var surfaceShortwave = readVar(ncPath, "QSW_BIN", c_double, 2);
  var o2Factor = readVar(ncPath, "o2_consumption_scalef", c_double, 2);
  const nt: c_size_t = 32;
  const tracerDomain = {1..numColumns, 1..zt};
  const tracerArrayDomain = {1..numColumns, 1..nt, 1..zt};
  //writeln("Tracer domain: ", tracerDomain);
  //writeln("Tracer array domain: ", tracerArrayDomain);

  var tracerArray: [tracerArrayDomain] c_double;
  var tracerShortNames: [1..nt] string = ["PO4", "NO3", "SiO3", "NH4", "Fe", "Lig", "O2", "DIC", "DIC_ALT_CO2",
                          "ALK", "ALK_ALT_CO2", "DOC", "DON", "DOP", "DOPr", "DONr", "DOCr",
                          "zooC", "spChl", "spC", "spP", "spFe", "spCaCO3", "diatChl", "diatC",
                          "diatP", "diatFe", "diatSi", "diazChl", "diazC", "diazP", "diazFe"];
  //writeln("tracer short name length: ", tracerShortNames.size);
  var tracerData : [1..nt, 1..numColumns, 1..zt] c_double;

  for tracerId in 1..nt:int {
    var tracerName = tracerShortNames[tracerId];
    //writeln("tracer name: ", tracerName);
    tracerData[tracerId,..,..] = readVar(ncPath, tracerShortNames[tracerId:int], c_double, 2);
  }

  for (c,t,z) in tracerArrayDomain {
    tracerArray[c,t,z] = tracerData[t,c,z];
  }

  //
  // Step 2: Initialize Geometry
  //

  const dt = 1.0;

  var marblWrapper: marbl_interop_type;
  //writeln("Passing in delta_z: ", delta_z);
  //writeln("Passing in zw: ", zw);
  //writeln("Passing in ztCol: ", ztCol);
  marblWrapper.initMarblInstance(zt, 1, 1, delta_z, zw, ztCol);



  var colIdx = 1;


  
  // Step 3: Surface Flux
  //
  var columnTracersView = tracerArray[colIdx,..,..];
  var columnTracers: [1..nt, 1..zt] c_double;
  var columnTracers_: [1..nt, 1..zt] c_double;
  for i in columnTracersView.domain {
    columnTracers[i] = columnTracersView[i];
    columnTracers_[i] = columnTracers[i];
  }

  marblWrapper.setSurfaceFluxForcingValues(
    zt, nt, columnTracers, salinity[colIdx, 1], 
    temperature[colIdx,1], u10_sqr[colIdx], u10_sqr[colIdx], 1.0, atm_co2[colIdx], atm_alt_co2[colIdx], 
    dust_flux[colIdx], iron_flux[colIdx], nox_flux[colIdx], nhy_flux[colIdx]);
  
  marblWrapper.surfaceFluxCompute(columnTracers, dt);

  marblWrapper.setInteriorTendencyForcingValues(zt, nt, columnTracers, dust_flux[colIdx], 1.0, 1.0, 1.0, temperature[colIdx,..], salinity[colIdx,..], ztCol);
  //marblWrapper.setInteriorTendencyForcingArray(zt,"Surface Shortwave", surfaceShortwave[colIdx,..]);
  marblWrapper.setInteriorTendencyForcingArray(zt,"Pressure", pressure[colIdx,..]);
  marblWrapper.setInteriorTendencyForcingArray(zt,"O2 Consumption Scale Factor", o2Factor[colIdx,..]);
  marblWrapper.setInteriorTendencyForcingArray(zt,"Iron Sediment Flux", iron_sed_flux[colIdx,..]);
  marblWrapper.interiorTendencyCompute(columnTracers, dt);
  /*
  var scaledSalinity: [1..zt] c_double = salinity[colIdx,..] * 1000;
  marblWrapper.setInteriorTendencyForcingArray(zt,"Surface Shortwave", surfaceShortwave[colIdx,..]);
  marblWrapper.setInteriorTendencyForcingValues(zt, nt, columnTracers, dust_flux[colIdx], 1.0, 1.0, 1.0, temperature[colIdx,..], salinity[colIdx,..], ztCol);
  marblWrapper.setInteriorTendencyForcingArray(zt,"Surface Shortwave", surfaceShortwave[colIdx,..]);
  marblWrapper.setInteriorTendencyForcingArray(zt,"Pressure", pressure[colIdx,..]);
  marblWrapper.setInteriorTendencyForcingArray(zt,"Iron Sediment Flux", iron_flux);
*/
  
  /*
  var columnFraction = [1.0,0,0,0,0,0];

  marblWrapper.setInteriorTendencyForcingArray(zt,"Potential Temperature", temperature[colIdx,..]);
  marblWrapper.setInteriorTendencyForcingArray(zt,"Salinity", salinity[colIdx,..]);
  marblWrapper.setInteriorTendencyForcingArray(zt,"Pressure", pressure[colIdx,..]);
  marblWrapper.setInteriorTendencyForcingArray(zt,"Surface Shortwave", surfaceShortwave[colIdx,..]);
  marblWrapper.setInteriorTendencyForcingArray(6, "PAR Column Fraction", columnFraction);
  marblWrapper.setInteriorTendencyForcingArray(zt,"O2 Consumption Scale Factor", o2Factor[colIdx,..]);
  marblWrapper.setInteriorTendencyForcingScalar("Dust Flux", dust_flux[colIdx]);
  marblWrapper.setInteriorTendencyForcingArray(zt,"Iron Sediment Flux", iron_flux);
  
  marblWrapper.copyTracerValues(columnTracers);
  */


  //writeln("NO3 concentrations from the input file :", NO3[colIdx,..]);
  //marblWrapper.interiorTendencyCompute(columnTracers, dt);
  
  var checkPath = "call_compute_subroutines.history.nc";

  var kmt = 50;
  for i in 1..nt {
    var name = tracerShortNames[i];
    writeln("Checking output for tracer: ", name);
    var originalConcentration = readVar(checkPath, name, c_double, 2);
    var expectedTendency = readVar(checkPath, "J_" + name, c_double, 2);
    var expectedNewConcentration = originalConcentration[colIdx,1..kmt] + expectedTendency[colIdx,1..kmt] * dt;
    writeln("  Expected concentrations: ", expectedNewConcentration);


    var calculatedConcentration = columnTracers[i,1..kmt];
    writeln("Calculated concentrations: ", calculatedConcentration);
    var difference = calculatedConcentration - expectedNewConcentration;
    writeln("Sum of Differences: ", + reduce difference);
  }
  return;
}