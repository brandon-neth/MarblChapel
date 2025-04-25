// Full test of MARBL interop using a single MARBL instance.
use Time;

var s: stopwatch;

var ioTime: real;
var initTime: real;
var computeTime: real;
var settingsTime: real;
var surfaceFluxTime: real;
var interiorTendencyTime: real;
config const numRuns = 1;
use Marbl;
use CTypes;
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
}
use myNetCDF;



const ncPath = "call_compute_subroutines.20190718.nc";

for i in 1..numRuns {

  // Dimensions
  s.restart();
  var numColumns = readDim(ncPath, "column");
  var nz = readDim(ncPath, "zt");

  // Geometry
  var ztCol = readVar(ncPath, "zt", c_double, 1);
  var activeLevelCount = readVar(ncPath, "active_level_cnt", c_int, 1);
  var delta_z = readVar(ncPath, "delta_z", c_double, 1);
  var zw = readVar(ncPath, "zw", c_double, 1);

  // Surface Flux
  var dust_flux = readVar(ncPath, "dust_flux", c_double, 1);
  var iron_flux = readVar(ncPath, "iron_flux", c_double, 1);
  var iron_sed_flux = readVar(ncPath, "iron_sed_flux", c_double, 2);
  var nox_flux = readVar(ncPath, "nox_flux", c_double, 1);
  var nhy_flux = readVar(ncPath, "nhy_flux", c_double, 1);
  var atm_co2 = readVar(ncPath, "atm_co2", c_double, 1);
  var atm_alt_co2 = readVar(ncPath, "atm_alt_co2", c_double, 1);
  var u10_sqr = readVar(ncPath, "u10_sqr", c_double, 1);
  var SST = readVar(ncPath, "SST", c_double, 1);
  var SSS = readVar(ncPath, "SSS", c_double, 1);
  var atm_pressure = readVar(ncPath, "atm_pressure", c_double, 1);

  // Tracers
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

  // Interior Forcings
  var temperature = readVar(ncPath, "temperature", c_double, 2);
  var salinity = readVar(ncPath, "salinity", c_double, 2);
  var pressure = readVar(ncPath, "pressure", c_double, 2);
  var surfaceShortwave = readVar(ncPath, "QSW_BIN", c_double, 2);
  var o2Factor = readVar(ncPath, "o2_consumption_scalef", c_double, 2);
  var columnFraction = readVar(ncPath, "FRACR_BIN", c_double, 2);

  const dt = 1.0;

  // Set up the tracer array
  var nt: c_size_t = 32;
  const tracerArrayDomain = {1..numColumns, 1..nt, 1..nz};
  var tracerArray: [tracerArrayDomain] c_double;

  var tracerShortNames: [1..nt] string = ["PO4", "NO3", "SiO3", "NH4", "Fe", "Lig", "O2", "DIC", "DIC_ALT_CO2",
                          "ALK", "ALK_ALT_CO2", "DOC", "DON", "DOP", "DOPr", "DONr", "DOCr",
                          "zooC", "spChl", "spC", "spP", "spFe", "spCaCO3", "diatChl", "diatC",
                          "diatP", "diatFe", "diatSi", "diazChl", "diazC", "diazP", "diazFe"];
  var tracerData : [1..nt, 1..numColumns, 1..nz] c_double;

  for tracerId in 1..nt:int {
    var tracerName = tracerShortNames[tracerId];
    tracerData[tracerId,..,..] = readVar(ncPath, tracerShortNames[tracerId:int], c_double, 2);
  }
  ioTime += s.elapsed();
  s.restart();
  forall (c,t,z) in tracerArrayDomain {
    tracerArray[c,t,z] = tracerData[t,c,z];
  }
  
  
  var marblWrappers: [1..numColumns] marblInteropType;
  initTime += s.elapsed();
  

  for colIdx_ in tracerArrayDomain.dim[0] {
    
    var colIdx = colIdx_ : int;
    var columnTracers: [1..nt, 1..nz] c_double = tracerArray[colIdx,..,..];
    
    // Initialize and verify it connects to something on the Fortran side
    var marblWrapper = marblWrappers[colIdx];
    assert(marblWrapper.marbl_obj:int != 0);
    
    var numParSubcols = columnFraction[colIdx,..].size;  
    var numElementsSurfaceFlux = 5;

    s.restart();
    marblWrapper.importSettings("marbl_with_o2_consumption_scalef.settings");
    settingsTime += s.elapsed();
    s.restart();
    marblWrapper.initMarblInstance(nz, numParSubcols, 5, delta_z, zw, ztCol, activeLevelCount[colIdx]);
    initTime += s.elapsed();
    s.restart();
    
    // Set surface flux forcing
    marblWrapper.setSurfaceFluxForcingValue("sss", salinity[colIdx, 1]);
    marblWrapper.setSurfaceFluxForcingValue("sst", temperature[colIdx, 1]);
    marblWrapper.setSurfaceFluxForcingValue("u10_sqr", u10_sqr[colIdx]);
    marblWrapper.setSurfaceFluxForcingValue("xco2", atm_co2[colIdx]);
    marblWrapper.setSurfaceFluxForcingValue("xco2_alt_co2", atm_alt_co2[colIdx]);
    marblWrapper.setSurfaceFluxForcingValue("Atmospheric Pressure", atm_pressure[colIdx]);
    marblWrapper.setSurfaceFluxForcingValue("Dust Flux", dust_flux[colIdx]);
    marblWrapper.setSurfaceFluxForcingValue("Iron Flux", iron_flux[colIdx]);
    marblWrapper.setSurfaceFluxForcingValue("NOx Flux", nox_flux[colIdx]);
    marblWrapper.setSurfaceFluxForcingValue("NHy Flux", nhy_flux[colIdx]);

    //Copy surface tracers
    marblWrapper.setSurfaceTracers(columnTracers);
    surfaceFluxTime += s.elapsed();
    s.restart();
    // Run surface flux compute
    marblWrapper.surfaceFluxCompute(columnTracers, dt);
    computeTime += s.elapsed();
    s.restart();

    // Set interior tendency forcing values
    
    var scaledSalinity = salinity * 1.0e3;
    var scaledIronSed = iron_sed_flux * 0.01;
    marblWrapper.setInteriorTendencyForcingScalar("Dust Flux", dust_flux[colIdx]);
    marblWrapper.setInteriorTendencyForcingArray("Potential Temperature", temperature[colIdx,..]);
    marblWrapper.setInteriorTendencyForcingArray("Salinity", scaledSalinity[colIdx,..]);
    marblWrapper.setInteriorTendencyForcingArray("Surface Shortwave", surfaceShortwave[colIdx,..]);
    marblWrapper.setInteriorTendencyForcingArray("PAR Column Fraction", columnFraction[colIdx,..]);
    marblWrapper.setInteriorTendencyForcingArray("Pressure", pressure[colIdx,..], activeLevelCount[colIdx]);
    marblWrapper.setInteriorTendencyForcingArray("O2 Consumption Scale Factor", o2Factor[colIdx,..], activeLevelCount[colIdx]);
    marblWrapper.setInteriorTendencyForcingArray("Iron Sediment Flux", scaledIronSed[colIdx,..], activeLevelCount[colIdx]);
    
    marblWrapper.setTracers(columnTracers);

    interiorTendencyTime += s.elapsed();
   
    s.restart();
    // Run interior tendency compute
    marblWrapper.interiorTendencyCompute(columnTracers, dt);
    
    computeTime += s.elapsed();
    s.restart();
    // Copy the calculated values back into the global tracer array
    //tracerArray[colIdx,..,..] = columnTracers[..,..];
  }

  for colIdx in 1..numColumns {
    marblWrappers[colIdx].shutdown();
  }

}

var values = [numRuns:string, ioTime:string,  settingsTime: string, initTime: string, surfaceFluxTime: string, interiorTendencyTime: string,computeTime: string];
writeln("Chapel,", ",".join(values));