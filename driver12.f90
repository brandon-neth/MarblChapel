module driver12




end module driver12

program main
  use marbl_interface
  use marbl_kinds_mod, only : r8
  use iso_c_binding
  implicit none
  type(MARBL_interface_class) :: marbl_instance
  integer :: marbl_settings_in, open_status, read_status
  character(len=256) :: namelist_line
  character(len=256) :: marbl_namelist_fname='marbl_in'
  character(len=256) ::marbl_tracer_list_fname='marbl_tracer_output_list'
  integer :: nz = 100
  real(r8), dimension(100)                    :: dummy_array
  dummy_array(:)=1.0

  open(unit=marbl_settings_in, file=marbl_namelist_fname, iostat=open_status)
  read(marbl_settings_in,"(A)",iostat=read_status) namelist_line
  call marbl_instance%put_setting(namelist_line)

  call marbl_instance%init(gcm_num_levels=nz, gcm_num_PAR_subcols = 1, gcm_num_elements_surface_flux = 1, gcm_delta_z = dummy_array(:), gcm_zw = dummy_array(:),  gcm_zt = dummy_array(:),   lgcm_has_global_ops = .true., unit_system_opt='mks')

end program main
