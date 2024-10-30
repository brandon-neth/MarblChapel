struct marbl_interop_type {
  double * tracer_array;
  int nz, nt;
  int sss_ind, sst_ind, ifrac_ind, dust_dep_ind, fe_dep_ind, nox_flux_ind, nhy_flux_ind, atmpress_ind, xco2_ind, xco2_alt_ind;
  int isalt, itemp;
  int u10_sqr_ind;
  double uwnd, vwnd, pco2air, pco2air_alt, dust, iron, nox, nhy;
  double dt;
  double * Hz, *z_w, *z_r;
  int dustflux_ind, PAR_col_frac_ind, surf_shortwave_ind, potemp_ind, salinity_ind, pressure_ind, fesedflux_ind;
  double srflx, Cp, rho0;
};

void pass_and_use(struct marbl_interop_type * marbl_interop);