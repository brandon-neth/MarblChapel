use CTypes;
use Random;

extern proc chpldrv_column_physics(tracer_array: c_ptr(c_double), const ref nz: c_int, const ref nt: c_int, 
                            const ref sss_ind: c_int, const ref sst_ind: c_int, const ref ifrac_ind: c_int, const ref dust_dep_ind: c_int, const ref fe_dep_ind: c_int, const ref nox_flux_ind: c_int, const ref nhy_flux_ind: c_int, const ref atmpress_ind: c_int, const ref xco2_ind: c_int, const ref xco2_alt_ind: c_int,
                            const ref isalt: c_int, const ref itemp: c_int, 
                            const ref u10_sqr_ind: c_int,
                            const ref uwnd: c_double, const ref vwnd: c_double, const ref pco2air: c_double, const ref pco2air_alt: c_double, const ref dust: c_double, const ref iron: c_double, const ref nox: c_double, const ref nhy: c_double, const ref dt: c_double,
                            Hz: c_ptr(c_double), z_w: c_ptr(c_double), z_r: c_ptr(c_double),
                            const ref dustflux_ind: c_int, const ref PAR_col_frac_ind: c_int, const ref surf_shortwave_ind: c_int, const ref potemp_ind: c_int, const ref salinity_ind: c_int, const ref pressure_ind: c_int, const ref fesedflux_ind: c_int,
                            const ref srflx: c_double, const ref Cp: c_double, const ref rho0: c_double);


var nz = 100;
var nt = 30;

var sss_ind: c_int = 1;
var sst_ind: c_int = 2;
var ifrac_ind: c_int = 3;
var dust_dep_ind: c_int = 4;
var fe_dep_ind: c_int = 5;
var nox_flux_ind: c_int = 6;
var nhy_flux_ind: c_int = 7;
var atmpress_ind: c_int = 8;
var xco2_ind: c_int = 9;
var xco2_alt_ind: c_int = 10;
var isalt: c_int = 1;
var itemp: c_int = 2;
var u10_sqr_ind: c_int = 11;
var uwnd: c_double = 9.8;
var vwnd: c_double = 9.8;
var pco2air: c_double = 100;
var pco2air_alt: c_double = 90;
var dust: c_double = 1.8;
var iron: c_double = 8.2;
var nox: c_double = 3.3;
var nhy: c_double = 4.4;
var dt: c_double = 1.0;

var Hz: [1..nz] real = 1.0;
var z_w: [1..nz] real = 1.0;
var z_r: [1..nz] real = 1.0;
  
var tracer_array: [1..nz,1..nt] real;
fillRandom(tracer_array);

var dustflux_ind: c_int = 1;
var PAR_col_frac_ind: c_int = 2;
var surf_shortwave_ind: c_int = 3;
var potemp_ind: c_int = 4;
var salinity_ind: c_int = 5;
var pressure_ind: c_int = 6;
var fesedflux_ind: c_int = 7;

var srflx: c_double = 2.57;
var Cp: c_double = 3.3;
var rho0: c_double = 1.0;

var tracer_array_start: [1..nz,1..nt] real;
tracer_array_start = tracer_array;

writeln("Calling chpldrv_column_physics from Chapel program...");
chpldrv_column_physics(c_ptrTo(tracer_array), nz:c_int, nt:c_int, 
                         sss_ind:c_int, sst_ind:c_int, ifrac_ind:c_int, dust_dep_ind:c_int, fe_dep_ind:c_int, nox_flux_ind:c_int, nhy_flux_ind:c_int, atmpress_ind:c_int, xco2_ind:c_int, xco2_alt_ind:c_int,
                         isalt:c_int, itemp:c_int, 
                         u10_sqr_ind:c_int,
                         uwnd:c_double, vwnd:c_double, pco2air:c_double, pco2air_alt:c_double, dust:c_double, iron:c_double, nox:c_double, nhy:c_double, dt:c_double,
                         c_ptrTo(Hz), c_ptrTo(z_w), c_ptrTo(z_r),
                         dustflux_ind:c_int, PAR_col_frac_ind:c_int, surf_shortwave_ind:c_int, potemp_ind:c_int, salinity_ind:c_int, pressure_ind:c_int, fesedflux_ind:c_int,
                         srflx:c_double, Cp:c_double, rho0:c_double);

writeln("Done calling chpldrv_column_physics from Chapel program.");
var changeCount = 0;
for i in tracer_array.domain {
  if tracer_array(i) != tracer_array_start(i) {
    changeCount += 1;
  }
}
writeln("Number of elements that changed: ", changeCount);