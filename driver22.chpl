use CTypes;
use List;
use NetCDF.C_NetCDF;


var ncidp: c_int; 
nc_open("roms_frc_bgc.nc", 0, ncidp);

var name = "dust";
var dust_id: c_int;

nc_inq_varid(ncidp, name.c_str(), dust_id);

writeln("dust variable id: ", dust_id);

var dustData: [1..12,1..26,1..26] c_float;

//nc_get_var_float(ncidp, dust_id, dustData[1,1,1]);


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

  var D = makeRectangularDomain(1, dimLengths);
  var data: [D] dtype;

  var firstIndex: dimCount * c_int;
  for i in 0..#dimCount {
    firstIndex[i] = 1;
  }

  nc_get_var(ncidp, var_id, c_ptrTo(data));

  nc_close(ncidp);
  return data;
}

var data = readVar("roms_frc_bgc.nc", "dust", c_float, 3);
var pco2_air = readVar("roms_frc_bgc.nc", "pco2_air", c_float, 3);
var pco2_air_alt = readVar("roms_frc_bgc.nc", "pco2_air_alt", c_float, 3);
var nox = readVar("roms_frc_bgc.nc", "nox", c_float, 3);
var nhy = readVar("roms_frc_bgc.nc", "nhy", c_float, 3);
var fesedflux = readVar("roms_frc_bgc.nc", "fesedflux", c_float, 3);

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
    writeln("Read variable name: ", stringName);
    // only add variables with the correct dimensionality count
    var varDims: c_int;
    nc_inq_varndims(ncidp, i:c_int, varDims);
    if (varDims != dimCount) {
      writeln("skipping due to dimensionality");
      continue;
    }
    // only add variables with the correct shape
    var data = readVar(datasetFile, stringName, dtype, dimCount);
    if data.shape != (1,100,26,26) {
      writeln("Skipping due to shape.");
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
    writeln("Data for ", varName, ": ", data.shape);
    datas.pushBack(data);
  }
  writeln("There were ", varNames.size, " variables with ", dimCount, " dimensions.");
  return datas;
}

var tracerDataList = readAllVars("roms_ini_MARBL.nc", c_float, 4);

