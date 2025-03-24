program overhead_fortran
  use marbl_interface, only : marbl_interface_class
  use marbl_logging,   only : marbl_log_type
  use marbl_kinds_mod, only : char_len
  use call_compute_subroutine, only : test
  use marbl_io_mod, only : marbl_io_read_settings_file

  type(marbl_interface_class), dimension(:), allocatable :: marbl_instances
  type(marbl_log_type)          :: driver_status_log
  character(len=char_len) :: hist_file, settings_file
  character(len=3)        :: unit_system_opt
  integer :: num_inst

  unit_system_opt = 'cgs'

  hist_file      = 'history.nc'
  settings_file  = 'marbl_with_o2_consumption_scalef.settings'
  num_inst = 1
  allocate(marbl_instances(num_inst))
  do n=1,num_inst
    call marbl_io_read_settings_file(settings_file, marbl_instances(n))
  end do

  print *, 'calling test'
  call test(marbl_instances, hist_file, unit_system_opt, driver_status_log)

end program overhead_fortran