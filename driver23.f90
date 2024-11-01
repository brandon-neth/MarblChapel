



program main
  use marbl_interface
  use iso_c_binding
  implicit none
  type(marbl_interface_class) :: marbl_instance
  integer :: nz, nt
  real(c_double), dimension(100) :: dummy_array
  integer :: i, j


  dummy_array = 0.0
  nz = 100

  call marbl_instance%init(gcm_num_levels=nz, gcm_num_PAR_subcols = 1, gcm_num_elements_surface_flux = 1, gcm_delta_z = dummy_array(:), gcm_zw = dummy_array(:),      gcm_zt = dummy_array(:),      lgcm_has_global_ops = .true., unit_system_opt='mks')

  do i = 1,size(marbl_instance%surface_flux_forcings)
    print *, "surface flux forcing name: ", (trim(MARBL_instance%surface_flux_forcings(i)%metadata%varname))
  end do

  do i = 1,size(marbl_instance%interior_tendency_forcings)
    print *, "interior tendency forcing name: ", (trim(MARBL_instance%interior_tendency_forcings(i)%metadata%varname))
  end do

  do i = 1,size(marbl_instance%tracer_metadata)
    print *, "tracer name: ", (trim(marbl_instance%tracer_metadata(i)%long_name))
    print *, "tracer short ename: ", (trim(marbl_instance%tracer_metadata(i)%short_name))
  end do

end program main

