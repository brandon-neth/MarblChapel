struct marbl_interop_type {
  int nz, nt;
  int isalt, itemp;
  double * tracer_array;
};

void compute_timestep(struct marbl_interop_type* marbl_interop);