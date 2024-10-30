struct marbl_instance {
  int nz, nt;
  int isalt, itemp;
  double fe, nox, nhy;
  double* arr;
};

void change_instance(struct marbl_instance* marblDriver);

