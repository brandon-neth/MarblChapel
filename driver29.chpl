
module myNetCDF {
  use NetCDF.C_NetCDF;
  use List;
  use Set;
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

  proc getVarNames(datasetPath: string) throws {
    var ncidp: c_int; 
    nc_open(datasetPath.c_str(), 0, ncidp);
    var varCount: c_int;
    nc_inq_nvars(ncidp, varCount);

    var varNames: set(string);
    for i in 0..#varCount {
      var nameBuffer: c_ptr(c_char) = allocate(c_char, NC_MAX_NAME);
      nc_inq_varname(ncidp, i:c_int, nameBuffer);
      var varName: string =  string.createCopyingBuffer(nameBuffer);
      deallocate(nameBuffer);

      varNames.add(varName);
    }


  return varNames;
  }

  proc compareArrays(file1, file2, varName) {

    var error: c_int;
    // First get the dimensionality
    var ncidp: c_int;
    error = nc_open(file1.c_str(), 0, ncidp);
    if error then writeln("Failed to open netcdf file: ", file1);
    
    var var_id: c_int;
    error = nc_inq_varid(ncidp, varName.c_str(), var_id);
    if error then writeln("Failed to find variable id for var: ", varName);

    var numDims: c_int;
    error = nc_inq_varndims(ncidp, var_id, numDims);
    if error then writeln("Failed to read dim count for var: ", varName);

    select numDims {
      when 1 {
        var a1 = readVar(file1, varName, c_double, 1);
        var a2 = readVar(file2, varName, c_double, 1);
        return (+ reduce a1) - (+ reduce a2);
      }
      when 2 {
        var a1 = readVar(file1, varName, c_double, 2);
        var a2 = readVar(file2, varName, c_double, 2);
        return (+ reduce a1) - (+ reduce a2);
      }
      when 3 {
        var a1 = readVar(file1, varName, c_double, 3);
        var a2 = readVar(file2, varName, c_double, 3);
        return (+ reduce a1) - (+ reduce a2);
      }
      otherwise {
        writeln("dimensionality not present in select statement: ", numDims);
        return -1;
      }
    }

  }

}


use myNetCDF;

config const file1 = "call_compute_subroutines.20190718.nc";
config const file2 = "call_compute_subroutines.history.nc";

config const tol = 1e-8;
var file1Names = getVarNames(file1);
var file2Names = getVarNames(file2);

var namesInBoth = file1Names & file2Names;

var just1 = file1Names - file2Names;
var just2 = file2Names - file1Names;

writeln("both: ", namesInBoth, "\n");

writeln("just in ", file1, ": ", just1, "\n");
writeln("just in ", file2, ": ", just2, "\n");

for name in namesInBoth {
  var difference = compareArrays(file1, file2, name);
  if abs(difference) < tol {
    continue;
  }
  writeln("Difference for  ", name, ": ", difference);
}
use CTypes;
var po4Tendency = readVar(file1, "J_PO4", c_double, 2);
writeln("po4 tendency: ");
writeln(po4Tendency);