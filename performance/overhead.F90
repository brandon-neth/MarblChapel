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
  real :: time_io, time_init, time_compute_surface, time_compute_interior, &
          time_config, time_setting_interior, time_setting_surface
  integer ::a,b
  character(len=20) :: str_io, str_init, str_compute_surface, str_compute_interior, &
                       str_num_runs, str_setting_interior, str_setting_surface, str_config
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
  time_setting_surface = 0
  time_setting_interior = 0
  time_compute_interior = 0
  time_compute_surface = 0
  time_config = 0


  do i = 1,num_runs
    allocate(marbl_instances(num_inst))
    call system_clock(count_start, count_rate)
    do n =1,num_inst
      call marbl_io_read_settings_file(settings_file, marbl_instances(n))
    end do
    call system_clock(count_end, count_rate)
    elapsed = (count_end - count_start) / real(count_rate)
    time_config = time_config + elapsed
    write(run_num, '(I5)') i
    hist_file = trim(run_num) // trim(hist_file_base)
    call test(marbl_instances, hist_file, unit_system_opt, driver_status_log, &
      time_io, time_init, time_setting_surface, time_setting_interior, &
      time_compute_surface, time_compute_interior, interior, surface)
    
    deallocate(marbl_instances)

  end do

  write(str_io, '(F7.4)') time_io
  write(str_init, '(F7.4)') time_init
  write(str_setting_interior, '(F7.4)') time_setting_interior
  write(str_setting_surface, '(F7.4)') time_setting_surface
  write(str_compute_interior, '(F7.4)') time_compute_interior
  write(str_compute_surface, '(F7.4)') time_compute_surface
  write(str_config, '(F7.4)') time_config
  write(str_num_runs, '(I0)') num_runs
  WRITE(0,*) 'interior and surface to make the compute happen:', interior, surface

  write(*,'(A)') 'Fortran,' // trim(str_num_runs) // ',' // trim(str_io) // ',' //  trim(str_config) // ',' //  &
    trim(str_init) // ',' // trim(str_setting_surface) // ',' // trim(str_compute_surface) // ',' // &
    trim(str_setting_interior) // ',' // trim(str_compute_interior)
  
end program overhead_fortran