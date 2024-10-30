struct marbl_interop_type {
  int nz, nt;
  int isalt, itemp;
  double * tracer_array;
  void * marbl_instance_obj;
};

void link_marbl_obj(struct marbl_interop_type* marbl_interop);
void use_marbl_obj(struct marbl_interop_type* marbl_interop);