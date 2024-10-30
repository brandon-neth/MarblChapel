struct marbl_interop_type {
  void * marbl_instance_obj;
};

void init_interop_obj(struct marbl_interop_type * interop_obj);

void init_marbl_instance(struct marbl_interop_type * interop_obj, 
  int *gcm_num_levels, int *gcm_num_PAR_subcols, int *gcm_num_elements_surface_flux, 
  double* gcm_delta_z, double* gcm_zw, double* gcm_zt);

void set_surface_flux_forcing_values(struct marbl_interop_type * interop_obj, 
  int* nz, int* nt, double* tracer_array,
  int* sss_ind, double* sss, 
  int* sst_ind, double* sst, 
  int* u10_sqr_ind, double* uwnd, double * vwnd, 
  int* atmpress_ind, double* atmpress,
  int* xcod2_ind, double* pco2air, 
  int* xco2_alt_ind, double* pco2air_alt, 
  int* dust_dep_ind, double* dust,
  int* fe_dep_ind, double* fe,
  int* nox_flux_ind, double* nox,
  int* nhy_flux_ind, double* nhy);

void compute_surface_fluxes(struct marbl_interop_type * interop_obj);

void update_surface_fluxes(struct marbl_interop_type * interop_obj, int* nz, int* nt, double* tracer_array, double* dt);

void set_interior_tendency_forcing_values(struct marbl_interop_type * interop_obj, 
  int* nz, int* nt, double* tracer_array,
  double* z_w, double* z_r, double* Hz, 
  int* dustflux_ind, double* dust,
  int* surf_shortwave_ind, double* srflx, double* Cp, double* rho0,
  int* potemp_ind, int* itemp,
  int* salinity_ind, int* isalt,
  int* pressure_ind,
  int* fesedflux_ind);

void compute_interior_tendencies(struct marbl_interop_type * interop_obj);

void update_interior_tendencies(struct marbl_interop_type * interop_obj, int* nz, int* nt, double* tracer_array, double* dt);