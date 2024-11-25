use CTypes;
use Random;

require "driver24.h";
extern record marbl_interop_type {
  var marbl_instance_obj: c_ptr(void);
  proc init() {
    writeln("marbl_interop_type init");
    init this;
    init_interop_obj(this);
  }
  
  proc initMarblInstance(const ref gcm_num_levels: c_int, const ref gcm_num_PAR_subcols: c_int, const ref gcm_num_elements_surface_flux: c_int, gcm_delta_z, gcm_zw, gcm_zt) {
    init_marbl_instance(this, gcm_num_levels, gcm_num_PAR_subcols, gcm_num_elements_surface_flux, c_ptrTo(gcm_delta_z), c_ptrTo(gcm_zw), c_ptrTo(gcm_zt));
  }

  proc setSurfaceFluxForcingValues(const ref nz, const ref nt, ref tracerArray, const ref salinity: c_double, const ref temperature: c_double, const ref uwind: c_double, const ref vwind: c_double, const ref pressure: c_double, const ref pco2air: c_double, const ref pco2air_alt: c_double, const ref dust: c_double, const ref fe: c_double, const ref nox: c_double, const ref nhy: c_double) {
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

var gcm_num_levels: c_int = 100;
var gcm_num_PAR_subcols: c_int = 1;
var gcm_num_elements_surface_flux: c_int = 1;
var gcm_delta_z: [1..gcm_num_levels] c_double;
var gcm_zw: [1..gcm_num_levels] c_double;
var gcm_zt: [1..gcm_num_levels] c_double;

var nz: c_int = 100;
var nt: c_int = 30;
var dt: c_double = 1.0;

var tracerArray: [1..nt, 1..nz] c_double;
tracerArray = 1.0;

for i in 1..gcm_num_levels {
  gcm_delta_z[i] = 0.0;
  gcm_zw[i] = 0.0;
  gcm_zt[i] = 0.0;
}



var marblWrapper: marbl_interop_type;

writeln("Initializing marbl instance...");

marblWrapper.initMarblInstance(gcm_num_levels, gcm_num_PAR_subcols, 
  gcm_num_elements_surface_flux, gcm_delta_z, gcm_zw, gcm_zt);


writeln("Setting surface flux forcing values...");

var salinity: real = 35000; 
var temperature: real = 303.15;
var uwind: real = 7.3;
var vwind: real = 7.3;
var pressure: real = 1;
var pco2air: real = 418.84;
var pco2air_alt: real = 418.84;
var dust: real = 0.005;
var fe: real = 0.001;
var nox: real = 0.053; 
var nhy: real = 0.0005;

marblWrapper.setSurfaceFluxForcingValues(nz, nt, tracerArray, 
  salinity, temperature, uwind, vwind, pressure, pco2air, pco2air_alt, dust, fe, nox, nhy);

writeln("Computing surface fluxes...");
marblWrapper.surfaceFluxCompute(tracerArray, dt);


writeln("Setting interior tendency forcing values...");

var columnTemp: [1..nz] c_double;
var columnSalinity: [1..nz] c_double;
var z_r: [1..nz] c_double;
for i in 1..nz {
  z_r[i] = i;
  columnTemp[i] = 303.15+i;
  columnSalinity[i] = 35000 + 100*i;
}
var srflx = 0.2;
var Cp = 1.0;
var rho0 = 1.0;

marblWrapper.setInteriorTendencyForcingValues(nz, nt, tracerArray, 
  dust, srflx, Cp, rho0, temperature, salinity, z_r);


writeln("Computing interior tendencies...");
marblWrapper.interiorTendencyCompute(tracerArray, dt);

writeln("Counting changed values...");
var changeCount = 0;
for t in tracerArray do 
  if t != 1.0 then changeCount += 1;

writeln("Change count: ", changeCount);

