/*
  This header file contains code for interoperating with the Fortran 90 MARBL biogeochemical library. The Fortran library's repository is found here: https://github.com/marbl-ecosys/MARBL/tree/marbl0.45.0
  Note that these capabilities are tested with MARBL version 0.45.0. Functionality with other versions of the library is not guaranteed.

  MARBL interoperability works using three layers: One Chapel, one C, one Fortran. First, there is the Chapel layer that contains the `marblInteropType` record and a collection of `extern` procedures. The `marblInteropType` record is the main way users will interact with the MARBL library, using the methods of the record. Second, there is the C header layer, contained in this file, which connects the Chapel `extern` procedures to their definitions in Fortran. Finally, there is the Fortran layer, which defines the various `extern` procedures declared in the Chapel layer. 
*/

typedef struct marblInteropType {
  void *  marbl_obj;
} marblInteropType;

void init_interop_obj(struct marblInteropType * interop_obj);

void deinit_interop_obj(struct marblInteropType * interop_obj);

void import_settings(struct marblInteropType * interop_obj, 
  const char * filename, int * filename_len);

void init_marbl_instance(struct marblInteropType * interop_obj, 
  int * num_levels, int * num_PAR_subcols, int * num_elements_surface_flux,
  double* delta_z, double * zw, double * zt, int * active_level_count);

void set_surface_flux_forcing_value(struct marblInteropType * interop_obj,
  const char * variable_name, int * vn_len, double * value);

void set_surface_tracers(struct marblInteropType * interop_obj,
  int * nt, int * nz, double * tracer_array);

void compute_surface_fluxes(struct marblInteropType * interop_obj);

void update_surface_fluxes(struct marblInteropType * interop_obj, 
  int * nt, int * nz, double * tracer_array, double * dt);

void set_surface_flux_forcings(struct marblInteropType *interop_obj, 
  double * surface_flux_forcings, int * num_forcings);
  
void set_interior_tendency_forcings(struct marblInteropType *interop_obj, 
  double * interior_tendency_forcings, int * extent, int * num_forcings, int * forcing_sizes);

void set_interior_tendency_forcing_array(struct marblInteropType * interop_obj, 
  const char * variable_name, int* vn_len, double * data, int * num_elements);

void set_interior_tendency_forcing_scalar(struct marblInteropType * interop_obj, 
  const char * variable_name, int* vn_len, double * value);

void set_tracers(struct marblInteropType * interop_obj,
  int * nt, int * nz, double * tracer_array);

void compute_interior_tendencies(struct marblInteropType * interop_obj);

void update_interior_tendencies(struct marblInteropType * interop_obj, 
  int * nt, int * nz, double * tracer_array, double * dt);

void get_diagnostic_value_1d(struct marblInteropType * interop_obj, 
  const char * variable_name, int * vn_len, double * ptr_out, int * ptr_out_len);

void get_diagnostic_value_2d(struct marblInteropType * interop_obj, 
  const char * variable_name, int * vn_len, double * ptr_out, int * ptr_out_len1, int * ptr_out_len2);

void num_interior_tendency_diagnostics(struct marblInteropType * interop_obj, 
  int * num_diagnostics);

void num_surface_flux_diagnostics(struct marblInteropType * interop_obj, 
  int * num_diagnostics);

void get_interior_tendency_diagnostic_name(struct marblInteropType * interop_obj, 
  int * idx, char * name, int * name_len, int * num_dims);

void get_surface_flux_diagnostic_name(struct marblInteropType * interop_obj, 
  int * idx, char * name, int * name_len, int * num_dims);


void get_saved_state_value_1d(struct marblInteropType * interop_obj, 
  const char * variable_name, int * vn_len, double * ptr_out, int * ptr_out_len);

void get_saved_state_value_2d(struct marblInteropType * interop_obj, 
  const char * variable_name, int * vn_len, double * ptr_out, int * ptr_out_len1, int * ptr_out_len2);
  
void set_saved_state_value_1d(struct marblInteropType * interop_obj, 
  const char * variable_name, int * vn_len, double * data_ptr, int * data_len);

void set_saved_state_value_2d(struct marblInteropType * interop_obj, 
  const char * variable_name, int * vn_len, double * data_ptr, int * data_len1, int * data_len2);
  
void num_interior_tendency_saved_states(struct marblInteropType * interop_obj, 
  int * num_states);

void num_surface_flux_saved_states(struct marblInteropType * interop_obj, 
  int * num_states);

void get_interior_tendency_saved_state_name(struct marblInteropType * interop_obj, 
  int * idx, char * name, int * name_len, int * dim_out);

void get_surface_flux_saved_state_name(struct marblInteropType * interop_obj, 
  int * idx, char * name, int * name_len, int * dim_out);

void extract_timing(struct marblInteropType * interop_obj);

void print_log(struct marblInteropType * interop_obj);