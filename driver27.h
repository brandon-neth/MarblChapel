struct marbl_interop_type {
  void * marbl_instance_obj;
};

void init_interop_obj(struct marbl_interop_type * interop_obj);

void init_marbl_instance(struct marbl_interop_type * interop_obj, 
  int *gcm_num_levels, int *gcm_num_PAR_subcols, int *gcm_num_elements_surface_flux, 
  double* gcm_delta_z, double* gcm_zw, double* gcm_zt);

void set_surface_flux_forcing_values(struct marbl_interop_type * interop_obj, 
  int* nz, int* nt, double* tracer_array, double* salinity, double* temperature, 
  double* uwind, double * vwind,  double* presure, double* pco2air,  
  double* pco2air_alt,  double* dust, double* fe, double* nox, double* nhy);

void compute_surface_fluxes(struct marbl_interop_type * interop_obj);

void update_surface_fluxes(struct marbl_interop_type * interop_obj, int* nz, int* nt, double* tracer_array, double* dt);

void set_interior_tendency_forcing_values(struct marbl_interop_type * interop_obj, 
  int* nz, int* nt, double* tracer_array, double* dust,
  double* srflx, double* Cp, double* rho0,
  double* temperature, double* salinity, double* z_r);

void compute_interior_tendencies(struct marbl_interop_type * interop_obj);

void update_interior_tendencies(struct marbl_interop_type * interop_obj, int* nz, int* nt, double* tracer_array, double* dt);

