program overhead_fortran
  use marbl_interface, only : marbl_interface_class
  use marbl_logging,   only : marbl_log_type
  use marbl_kinds_mod, only : char_len
  use call_compute_subroutine, only : test
  use marbl_io_mod, only : marbl_io_read_settings_file

  type(marbl_interface_class), dimension(:), allocatable :: marbl_instances
  type(marbl_log_type)          :: driver_status_log
  character(len=char_len) :: hist_file, settings_file, hist_file_base
  character(len=3)        :: unit_system_opt
  real :: interior(5, 60, 32)
  real :: surface(2, 32, 5)
  integer :: num_inst
  integer :: count_start, count_end, count_rate
  real :: elapsed
  integer :: num_runs, istat, i
  character(len=10) :: arg
  character(len=10) :: run_num
  real :: time_io, time_init, time_compute
  integer ::a,b
  ! Get the first command-line argument
  call get_command_argument(1, arg)

  ! Convert to integer
  read(arg, '(I10)', IOSTAT=istat) num_runs

  unit_system_opt = 'cgs'

  hist_file_base      = 'history.nc'
  settings_file  = 'marbl_with_o2_consumption_scalef.settings'
  num_inst = 5

  time_io = 0
  time_init = 0
  time_compute = 0
  


  do i = 1,num_runs
    allocate(marbl_instances(num_inst))
    do n =1,num_inst
      call marbl_io_read_settings_file(settings_file, marbl_instances(n))
    end do
    write(run_num, '(I5)') i
    hist_file = trim(run_num) // trim(hist_file_base)
    call test(marbl_instances, hist_file, unit_system_opt, driver_status_log, time_io, time_init, time_compute, interior, surface)
    
    deallocate(marbl_instances)

  end do

  print *, 'interior and surface to make the compute happen:', interior, surface
  print '(A, F10.6)', 'IO      Time: ', time_io
  print '(A, F10.6)', 'Init    Time: ', time_init
  print '(A, F10.6)', 'Compute Time: ', time_compute
  
end program overhead_fortran