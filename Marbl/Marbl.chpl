module Marbl {
  /* 
    This module contains code for interoperating with the Fortran 90 MARBL
    biogeochemical library. The Fortran library's repository is found here:
    https://github.com/marbl-ecosys/MARBL/tree/marbl0.45.0. Note that these
    capabilities are tested with MARBL version 0.45.0. Functionality with
    other versions of the library is not guaranteed.

    MARBL interoperability works using three layers: One Chapel, one C, one
    Fortran. First, there is the Chapel layer, found in this file, that
    contains the `marblInteropType` record and a collection of `extern`
    procedures. The `marblInteropType` record is the main way users will
    interact with the MARBL library, using the methods of the record. Second,
    there is the C header layer, which connects the Chapel `extern` procedures
    to their definitions in Fortran. Finally, there is the Fortran layer, which
    defines the various `extern` procedures declared in the Chapel layer. 
  */

  use CTypes;
  use Map;
  use BlockDist;
  require "MarblChapel.h";

  // Used to ensure only one marblInteropType has access to the settings files
  // at a time, as Fortran does not support one process making concurrent reads
  // to the same file

  var barrierDist = blockDist.createDomain({0..<numLocales});
  var initBarriers: [barrierDist] sync bool = true;
  var initBarrier: sync bool = true;

  /* 
    The main driver of MARBL interoperability. Supports shared- and 
    distributed-memory parallel implementations, with each thread getting its
    own instance to compute multiple columns at once. 
   */
  extern record marblInteropType {
    // A pointer to the Fortran-side MARBL instance object. 
    var marbl_obj: c_ptr(void);

    /*
      Initializes the Chapel-side interop object, then creates and connects it
      to a Fortran-side MARBL instance. This method does NOT populate the
      Fortran-side MARBL instance with any information. To do that, use
      `initMarblInstance`.
     */
    proc init() {
      init this;
      init_interop_obj(this);
    }

    /* 
      Frees the Fortran-side MARBL instance.
     */
    proc shutdown() {
      deinit_interop_obj(this);
    }

    /*
      Reads and applies a MARBL settings file to the Fortran-side MARBL
      instance. This process should usually occur before initializing the MARBL
      instance with the `initMarblInstance` method.

      :arg filename: The path to the MARBL settings file

     */
    proc importSettings(filename) {
      var lock = initBarriers[here.id].readFE();
      import_settings(this, filename.c_str(), filename.size : c_int);
      initBarriers[here.id].writeEF(lock);
    }

    /*
      Initializes the Fortran-side MARBL instance.

      :arg numLevels: The maximum number of layers in the columns of this
      simulation. This value should be the same across each MARBL instance.

      :arg numParSubcols: TODO

      :arg numElementsSurfaceFlux: TODO

      :arg deltaZ: The thicknesses of the layers in the column. For example,
      if the boundaries of the first three layers are at depths 10, 20, and 22,
      the first three values of `deltaZ` should be 10, 10, and 2. 

      :arg zw: The depth from the surface to the bottom of each layer in the
      column. For example, if the boundaries of the first three layers are at
      depths 10, 20, and 22, the first three values of `zw` should be 10, 20,
      and 22. 

      :arg zt: The depth from the surface to the midpoints of each layer in the
      column. For example, if the boundaries of the first three layers are at
      depth 10, 20, and 22, the first three values of `zt` should be 5, 15, and
      21. 

      :arg activeLevelCount: The number of active layers in the column this
      instance is simulating. For example, if there are 60 total cells, but
      only 50 cells between the surface and the ocean floor, this argument
      should be 50.
     */
    proc initMarblInstance(const ref numLevels, const ref numParSubcols,
      const ref numElementsSurfaceFlux, ref deltaZ, ref zw, ref zt,
      const ref activeLevelCount) {
      var lock = initBarriers[here.id].readFE();
      init_marbl_instance(this, numLevels: c_int, numParSubcols: c_int, 
      numElementsSurfaceFlux: c_int, c_ptrTo(deltaZ), c_ptrTo(zw), c_ptrTo(zt), 
      activeLevelCount: c_int);
      initBarriers[here.id].writeEF(lock);
    }

    /*
      Sets a surface flux forcing value in the Fortran-side MARBL instance. 

      :arg variableName: The name of the surface flux forcing value to set.
      Accepted values are
        - 'sss' for surface salinity
        - 'sst' for surface temperature
        - 'u10_sqr' for 10 meter wind speed squared
        - 'Atmospheric Pressure'
        - 'xco2' for carbon dioxide concentration
        - 'xco2_alt_co2' for alternative carbon dioxide concentration
        - 'Dust Flux'
        - 'Iron Flux'
        - 'NOx Flux'
        - 'NHy Flux'
        - 'Ice Fraction' 

      :arg value: The value to use for the the surface flux forcing
     */
    proc setSurfaceFluxForcingValue(const ref variableName, const ref value) {
      set_surface_flux_forcing_value(this, variableName.c_str(),
        variableName.size: c_int, value: c_double);
    }

    /* 
      Copies the tracer values at the surface into the Fortran-side MARBL
      instance.

      :arg tracerArray: The array of tracers to pass to the Fortran-side MARBL
      instance. The domain of this array should be the tracers in the first
      dimension and the vertical dimension in the second.
     */ 
    proc setSurfaceTracers(ref tracerArray) {
      var nt, nz: int;
      (nt,nz) = tracerArray.shape;
      set_surface_tracers(this, nt: c_int, nz: c_int, c_ptrTo(tracerArray));
    }

    /* 
      Computes the surface fluxes and updates the tracer array with the calculated
      tendencies.

      :arg tracerArray: The array of tracers to update with the calculated
      tendencies.

      :arg dt: The timestep to apply the tendencies through. The tracer array
      will be incremented by `dt` times the calculated tendencies.
     */
    proc surfaceFluxCompute(ref tracerArray, dt) {
      compute_surface_fluxes(this);
      var nt, nz: int;
      (nt,nz) = tracerArray.shape;
      update_surface_fluxes(this, nt: c_int, nz: c_int, c_ptrTo(tracerArray), 
        dt: c_double);
    }

    /*
      Copies an array into one of MARBL's interior tendency forcing variables. 

      :arg variableName: The name of the interior tendency forcing variable to
      set

      :arg data: The array of data to copy into the MARBL instance

      :arg numElements: Optional. The number of elements to copy into the MARBL
      instance. Default is the full length of `data`.
     */
    proc setInteriorTendencyForcingArray(variableName, ref data, numElements=data.size) {
      set_interior_tendency_forcing_array(this, 
        variableName.c_str(), variableName.size : c_int, 
        c_ptrTo(data), numElements: c_int);
    }

    /*
      Copies a value into one of MARBL's interior tendency forcing variables. 

      :arg variableName: The name of the interior tendency forcing variable to set

      :arg value: The value to copy into the MARBL instance
     */
    proc setInteriorTendencyForcingScalar(variableName, value) {
      set_interior_tendency_forcing_scalar(this, variableName.c_str(),
        variableName.size : c_int, value: c_double);
    }

    /* 
      Copies the tracer values into the Fortran-side MARBL instance.

      :arg tracerArray: The array of tracers to pass to the Fortran-side MARBL
      instance. The domain of this array should be the tracers in the first
      dimension and the vertical dimension in the second.
     */ 
    proc setTracers(ref tracerArray) {
      var nt, nz: int;
      (nt,nz) = tracerArray.shape;
      set_tracers(this, nt: c_int, nz: c_int, c_ptrTo(tracerArray));
    }

    /* 
      Computes the interior tracer tendencies and increments the tracer array.

      :arg tracerArray: The array of tracers to update with the calculated tendencies.

      :arg dt: The timestep to apply the tendencies through. The tracer array will be 
      incremented by `dt` times the calculated tendencies.
     */
    proc interiorTendencyCompute(ref tracerArray, dt) {
    compute_interior_tendencies(this);
    var nt, nz: int;
    (nt,nz) = tracerArray.shape;
    update_interior_tendencies(this, nt: c_int, nz: c_int, c_ptrTo(tracerArray),
      dt: c_double);
    }

    /*
      Gets a diagnostic variable from the Fortran-side MARBL instance.

      :arg variableName: The name of the diagnostic variable to get.

      :arg numDims: The number of dimensions of the diagnostic variable.
        MARBL names its 1D diagnostic variables 2D and its 2D variables 3D.
        Error messages / warnings on the Fortran side reflect this, but this
        function expects the actual dimensionality. Currently supported values
        for this argument are `1` and `2`.
     */
    proc getDiagnostic(variableName, param numDims) throws {
      if numDims == 1 {
        var ptr: c_ptr(c_double);
        var size: c_int;
        get_diagnostic_value_1d(this, variableName.c_str(), variableName.size : c_int, ptr, size);
        if size == -1 {
          throw new Error("MARBL returned an error when getting the diagnostic variable " + variableName);
        }
        var diagnosticArray: [1..size] real = makeArrayFromPtr(ptr, {1..size});
        return diagnosticArray;
      } else if numDims == 2 {
        var ptr: c_ptr(c_double);
        var size1, size2: c_int;
        get_diagnostic_value_2d(this, variableName.c_str(), variableName.size : c_int, ptr, size1, size2);
        if size1 == -1 {
          throw new Error("MARBL returned an error when getting the diagnostic variable " + variableName);
        }
        // note the reversal of the order of the dimensions, as this is Fortran-allocated data
        var diagnosticArray: [1..size2, 1..size1] real = makeArrayFromPtr(ptr, {1..size2, 1..size1});
        return diagnosticArray;
      } else {
        throw new Error("getDiagnostic only supports 1D and 2D diagnostic variables, but got " + numDims);
        return 0;
      }
    }

    proc getInteriorTendencyDiagnosticNames() throws {
      var nameDimMap: map(string, int);
      var count: c_int;
      num_interior_tendency_diagnostics(this, count);
      for i in 1..count {
        var name: c_ptr(c_char);
        var nameLen: c_int;
        var dim: c_int;
        get_interior_tendency_diagnostic_name(this, i, name, nameLen, dim);
        if dim == -1 {
          throw new Error("MARBL returned an error when getting the diagnostic variable with index " + i:string);
        }
        var nameStr = string.createCopyingBuffer(name, nameLen);
        nameDimMap[nameStr] = dim;
      }
      return nameDimMap;
    }

    proc getSurfaceFluxDiagnosticNames() throws {
      var nameDimMap: map(string, int);
      var count: c_int;
      num_surface_flux_diagnostics(this, count);
      for i in 1..count {
        var name: c_ptr(c_char);
        var nameLen: c_int;
        var dim: c_int;
        get_surface_flux_diagnostic_name(this, i, name, nameLen, dim);
        if dim == -1 {
          throw new Error("MARBL returned an error when getting the diagnostic variable with index " + i:string);
        }
        var nameStr = string.createCopyingBuffer(name, nameLen);
        nameDimMap[nameStr] = dim;
      }
      return nameDimMap;
    }
    /*
      Returns a map between the names of the available diagnostic values 
      and their dimensionality.
     */ 
    proc getDiagnosticNames() throws {
      var nameMap = getInteriorTendencyDiagnosticNames();
      var surfaceFluxMap = getSurfaceFluxDiagnosticNames();
      nameMap.extend(surfaceFluxMap);
      return nameMap;
    }


    proc getSavedState(variableName, param numDims) throws {
      if numDims == 1 {
        var ptr: c_ptr(c_double);
        var size: c_int;
        get_saved_state_value_1d(this, variableName.c_str(), variableName.size : c_int, ptr, size);
        if size == -1 {
          throw new Error("MARBL returned an error when getting the saved state variable " + variableName);
        }
        var ptr2: c_ptr(c_double) = allocate(c_double, size, clear=true);
        for i in 0..size do 
          ptr2[i] = ptr[i];
        var savedStateArray: [1..size] real = makeArrayFromPtr(ptr2, {1..size});
        return savedStateArray;
      } else if numDims == 2 {
        var ptr: c_ptr(c_double);
        var size1, size2: c_int;
        get_saved_state_value_2d(this, variableName.c_str(), variableName.size : c_int, ptr, size1, size2);
        if size1 == -1 {
          throw new Error("MARBL returned an error when getting the saved state variable " + variableName);
        }
        var ptr2: c_ptr(c_double) = allocate(c_double, size1 * size2, clear=true);
        for i in 0..size1*size2 do 
          ptr2[i] = ptr[i];
        // note the reversal of the order of the dimensions, as this is Fortran-allocated data
        var savedStateArray: [1..size2, 1..size1] real = makeArrayFromPtr(ptr2, {1..size2, 1..size1});
        
        return savedStateArray;
      } else {
        throw new Error("getSavedState only supports 1D and 2D saved state variables, but got " + numDims);
        return 0;
      }
    }

    proc setSavedState(variableName, ref data) throws {
      if data.domain.rank == 1{
        set_saved_state_value_1d(this, variableName.c_str(), variableName.size : c_int, 
          c_ptrTo(data), data.size : c_int);
      } else if data.domain.rank == 2 {
        set_saved_state_value_2d(this, variableName.c_str(), variableName.size : c_int, c_ptrTo(data), data.shape(0) : c_int, data.shape(1) : c_int);
      } else {
        throw new Error("setSavedState only supports 1D and 2D saved state variables, but got " + data.domain.size);
      }
    }

    proc getInteriorTendencySavedStateNames() throws {
      var nameDimMap: map(string, int);
      var count: c_int;
      num_interior_tendency_saved_states(this, count);
      for i in 1..count {
        var name: c_ptr(c_char);
        var nameLen: c_int;
        var dim: c_int;
        get_interior_tendency_saved_state_name(this, i, name, nameLen, dim);
        if dim == -1 {
          throw new Error("MARBL returned an error when getting the interior tendency saved state name with index " + i:string);
        }
        var nameStr = string.createCopyingBuffer(name, nameLen);
        nameDimMap[nameStr] = dim;
      }
      return nameDimMap;
    }

    proc getSurfaceFluxSavedStateNames() throws {
      var nameDimMap: map(string, int);
      var count: c_int;
      num_surface_flux_saved_states(this, count);
      for i in 1..count {
        var name: c_ptr(c_char);
        var nameLen: c_int;
        var dim: c_int;
        get_surface_flux_saved_state_name(this, i, name, nameLen, dim);
        if dim == -1 {
          throw new Error("MARBL returned an error when getting the surface flux saved state name with index " + i:string);
        }
        var nameStr = string.createCopyingBuffer(name, nameLen);
        nameDimMap[nameStr] = dim;
      }
      return nameDimMap;
    }

    proc getSavedStateNames() throws {
      var nameMap = getInteriorTendencySavedStateNames();
      var surfaceFluxNames = getSurfaceFluxSavedStateNames();
      nameMap.extend(surfaceFluxNames);
      return nameMap;
    }

    proc extractTiming() {
      extract_timing(this);
    }

    proc printLog() {
      print_log(this);
    }
  } // extern record marblInteropType

  extern proc init_interop_obj(const ref marblWrapper: marblInteropType);
  
  extern proc deinit_interop_obj(const ref marblWrapper: marblInteropType);

  extern proc import_settings(const ref marblWrapper: marblInteropType, 
    filename: c_ptrConst(c_char), const ref filename_len: c_int);
  
  extern proc init_marbl_instance(const ref marblWrapper: marblInteropType, 
    const ref num_levels: c_int, const ref num_PAR_subcols: c_int, 
    const ref num_elements_surface_flux: c_int, delta_z: c_ptr(c_double), 
    zw: c_ptr(c_double), zt: c_ptr(c_double), const ref active_level_count: c_int);

  extern proc set_surface_flux_forcing_value(const ref marblWrapper: marblInteropType,
    variable_name: c_ptrConst(c_char), const ref vn_len: c_int, 
    const ref value: c_double);

  extern proc set_surface_tracers(const ref marblWrapper: marblInteropType, 
    const ref nt: c_int, const ref nz: c_int, tracer_array: c_ptr(c_double));

  extern proc compute_surface_fluxes(const ref interop_obj: marblInteropType);

  extern proc update_surface_fluxes(const ref interop_obj: marblInteropType,
    const ref nt: c_int, const ref nz: c_int, tracer_array: c_ptr(c_double),
    const ref dt: c_double);

  extern proc set_interior_tendency_forcings(const ref interop_obj: marblInteropType,
    interior_tendency_forcings: c_ptr(c_double), 
    const ref extent: c_int, const ref num_forcing: c_int, forcing_sizes: c_ptr(c_int));

  extern proc set_interior_tendency_forcing_array(const ref interop_obj: marblInteropType,
    variable_name: c_ptrConst(c_char), const ref vn_len: c_int,
    data: c_ptr(c_double), const ref num_elements: c_int);

  extern proc set_interior_tendency_forcing_scalar(const ref interop_obj: marblInteropType,
    variable_name: c_ptrConst(c_char), const ref vn_len: c_int, const ref value : c_double);
  
  extern proc set_tracers(const ref marblWrapper: marblInteropType, 
    const ref nt: c_int, const ref nz: c_int, tracer_array: c_ptr(c_double));

  extern proc compute_interior_tendencies(const ref marblWrapper: marblInteropType);

  extern proc update_interior_tendencies(const ref interop_obj: marblInteropType,
    const ref nt: c_int, const ref nz: c_int, tracer_array: c_ptr(c_double),
    const ref dt: c_double);

  extern proc get_diagnostic_value_1d(const ref interop_obj: marblInteropType,
    variable_name: c_ptrConst(c_char), const ref vn_len: c_int, 
    ref ptr_out : c_ptr(c_double), ref ptr_out_len: c_int);

  extern proc get_diagnostic_value_2d(const ref interop_obj: marblInteropType,
    variable_name: c_ptrConst(c_char), const ref vn_len: c_int, 
    ref ptr_out :  c_ptr(c_double), ref ptr_out_len1: c_int, ref ptr_out_len2);

  extern proc num_interior_tendency_diagnostics(const ref interop_obj: marblInteropType,
    ref num_diagnostics: c_int);

  extern proc num_surface_flux_diagnostics(const ref interop_obj: marblInteropType,
    ref num_diagnostics: c_int);
  
  extern proc get_interior_tendency_diagnostic_name(const ref interop_obj: marblInteropType,
    const ref idx: c_int, ref name_out: c_ptr(c_char), ref name_out_len: c_int, ref num_dims: c_int);

  extern proc get_surface_flux_diagnostic_name(const ref interop_obj: marblInteropType,
    const ref idx: c_int, ref name_out: c_ptr(c_char), ref name_out_len: c_int, ref num_dims: c_int);

  
  extern proc get_saved_state_value_1d(const ref interop_obj: marblInteropType,
      variable_name: c_ptrConst(c_char), const ref vn_len: c_int, 
      ref ptr_out : c_ptr(c_double), ref ptr_out_len: c_int);
  
  extern proc get_saved_state_value_2d(const ref interop_obj: marblInteropType,
    variable_name: c_ptrConst(c_char), const ref vn_len: c_int, 
    ref ptr_out :  c_ptr(c_double), ref ptr_out_len1: c_int, ref ptr_out_len2);

  extern proc set_saved_state_value_1d(const ref interop_obj: marblInteropType,
      variable_name: c_ptrConst(c_char), const ref vn_len: c_int, 
      data_ptr : c_ptr(c_double), const ref data_len: c_int);
  
  extern proc set_saved_state_value_2d(const ref interop_obj: marblInteropType,
    variable_name: c_ptrConst(c_char), const ref vn_len: c_int, 
    data_ptr : c_ptr(c_double), const ref data_len1: c_int, const ref data_len2);
  
  extern proc num_interior_tendency_saved_states(const ref interop_obj: marblInteropType, 
    ref num_states: c_int);

  extern proc num_surface_flux_saved_states(const ref interop_obj: marblInteropType, 
    ref num_states: c_int);

  extern proc get_interior_tendency_saved_state_name(const ref interop_obj: marblInteropType, 
    const ref idx: c_int, ref name: c_ptr(c_char), ref name_len: c_int, ref dim_out: c_int);

  extern proc get_surface_flux_saved_state_name(const ref interop_obj: marblInteropType, 
    const ref idx: c_int, ref name: c_ptr(c_char), ref name_len: c_int, ref dim_out: c_int);

  extern proc extract_timing(const ref interop_obj: marblInteropType);

  extern proc print_log(const ref interop_obj: marblInteropType);
} // module Marbl