module call_compute_subroutine

  use marbl_constants_mod, only : c0
  use marbl_interface, only : marbl_interface_class
  use marbl_kinds_mod, only : r8, char_len
  use marbl_logging,   only : marbl_log_type
  use marbl_io_mod,    only : grid_data_type
  use marbl_io_mod,    only : forcing_fields_type

  implicit none
  private
  save

  Public :: test

Contains

  !****************************************************************************

  subroutine test(marbl_instances, hist_file, unit_system_opt, driver_status_log, &
    time_io, time_init, time_setting_surface, time_setting_interior, &
    time_compute_surface, time_compute_interior, time_copyback, interior, surface)

    use marbl_settings_mod, only : output_for_GCM_iopt_total_Chl_3d

    use marbl_io_mod, only : marbl_io_open_files
    use marbl_io_mod, only : marbl_io_construct_diag_buffers
    use marbl_io_mod, only : marbl_io_define_history
    use marbl_io_mod, only : marbl_io_copy_into_diag_buffer
    use marbl_io_mod, only : marbl_io_write_history
    use marbl_io_mod, only : marbl_io_read_forcing_field
    use marbl_io_mod, only : marbl_io_read_tracers
    use marbl_io_mod, only : marbl_io_close_files
    use marbl_io_mod, only : marbl_io_destruct_diag_buffers

    type(marbl_interface_class), dimension(:), intent(inout) :: marbl_instances
    character(len=*),                          intent(in)    :: hist_file
    character(len=*),                          intent(in)    :: unit_system_opt
    type(marbl_log_type),                      intent(inout) :: driver_status_log
    real,                                      intent(inout) :: time_io, time_init, time_setting_surface, &
                                                                time_setting_interior, time_compute_surface, &
                                                                time_compute_interior, time_copyback
    real, intent(inout) :: interior(5,60,32), surface(1,32,5)

    character(len=*), parameter :: subname = 'marbl_call_compute_subroutines_drv:test'
    character(len=*), parameter :: infile = 'call_compute_subroutines.20190718.nc'
    character(len=char_len) :: log_message
    real(r8),                  allocatable, dimension(:,:)   :: surface_fluxes              ! num_cols x num_tracers
    real(r8),                  allocatable, dimension(:,:)   :: surface_flux_output         ! num_cols x num_vars
    real(r8),                  allocatable, dimension(:,:,:) :: interior_tendencies         ! num_tracers x num_levels x num_cols
    real(r8),                  allocatable, dimension(:,:)   :: total_Chl                   ! num_levels x num_cols
    real(r8),                  allocatable, dimension(:,:)   :: bot_flux_to_tend            ! num_levels x num_cols
    real(r8),                  allocatable, dimension(:,:,:) :: tracer_initial_vals         ! num_tracers x num_levels x num_cols
    real(r8),                  allocatable, dimension(:)     :: lat                         ! num_cols
    type(forcing_fields_type), allocatable, dimension(:)     :: surface_flux_forcings       ! num_forcings
    type(forcing_fields_type), allocatable, dimension(:)     :: interior_tendency_forcings  ! num_forcings
    integer,                   allocatable, dimension(:)     :: active_level_cnt, col_start, col_cnt
    integer :: a,b

    integer :: num_levels, num_cols, num_tracers, m, n, col_id_loc, col_id, num_PAR_subcols
    integer :: sfo_cnt, flux_co2_id, total_surfChl_id, output_id
    type(grid_data_type) :: grid_data
    integer(8) :: count_start, count_end, count_rate
    real :: elapsed
    
    
    call system_clock(count_start, count_rate)
    ! 1. Open necessary netCDF files
    !    (a) Input (grid info, forcing fields, initial conditions)
    !    (b) Output (diagnostics)
    call marbl_io_open_files(infile, hist_file, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_open', subname)
      print *, "Aborting after failing to open io files"
      return
    end if
    write(log_message, "(A, 1X, A)") "* MARBL output will be written to", trim(hist_file)
    call driver_status_log%log_noerror(log_message, subname)
    call system_clock(count_end)
    elapsed = real(count_end - count_start) / real(count_rate)
    time_io = time_io + elapsed
    ! --------------------------------------------------------------------------

    
    call system_clock(count_start, count_rate)
    ! 2. Initialize the test (reads grid info, distributes columns, etc)
    call set_domain(size(marbl_instances), unit_system_opt, num_levels, active_level_cnt, lat, &
                    num_PAR_subcols, col_start, col_cnt, grid_data, driver_status_log)
    if (driver_status_log%labort_MARBL) then
      call driver_status_log%log_error_trace('set_domain', subname)
      print *, "Aborting after failing to set domains"
      return
    end if
    num_cols = sum(col_cnt)
    call system_clock(count_end)
    elapsed = real(count_end - count_start) / real(count_rate)
    !not in sum
    ! --------------------------------------------------------------------------


    call system_clock(count_start)
    ! 3. Initialize each instance of MARBL
    do n=1, size(marbl_instances)
      call marbl_instances(n)%init(gcm_num_levels = num_levels,                &
                                   gcm_num_PAR_subcols = num_PAR_subcols,      &
                                   gcm_num_elements_surface_flux = col_cnt(n), &
                                   gcm_delta_z = grid_data%delta_z,            &
                                   gcm_zw = grid_data%zw,                      &
                                   gcm_zt = grid_data%zt,                      &
                                   unit_system_opt = unit_system_opt)
      if (marbl_instances(n)%StatusLog%labort_marbl) then
        call marbl_instances(n)%StatusLog%log_error_trace('marbl%init', subname)
        print *, "Aborting after failing to init instances"
        return
      end if
    end do
    call system_clock(count_end)
    elapsed = real(count_end - count_start) / real(count_rate)
    time_init = time_init + elapsed
    ! --------------------------------------------------------------------------


    call system_clock(count_start)
    ! 4. Set up memory for fields MARBL returns to GCM
    !    Also, request flux_co2, total_surf_Chl, and total_Chl
    !    (a) Fields returned from surface_flux_compute()
    sfo_cnt = 0

    sfo_cnt = sfo_cnt+1
    do n=1, size(marbl_instances)
      call marbl_instances(n)%add_output_for_GCM(num_elements=col_cnt(n), &
                                                 field_name="flux_co2", &
                                                 output_id=flux_co2_id)
    end do

    sfo_cnt = sfo_cnt+1
    do n=1, size(marbl_instances)
      call marbl_instances(n)%add_output_for_GCM(num_elements=col_cnt(n), &
                                                 field_name="total_surfChl", &
                                                 output_id=total_surfChl_id)
    end do

    allocate(surface_flux_output(num_cols, sfo_cnt))
    allocate(total_Chl(num_levels, num_cols))
    call system_clock(count_end)
    elapsed = real(count_end - count_start) / real(count_rate)
    !untracked = untracked + elapsed


    
    ! 5. Initialize diagnostic buffers, define diagnostic fields in output netCDF file, and
    !    read initial conditions / forcing data

    !    (a) Set constants

    num_tracers = size(marbl_instances(1)%tracer_metadata)

    !    (b) Initialize diagnostic buffers
    call marbl_io_construct_diag_buffers(num_levels, num_cols, marbl_instances(1))

    !    (c) Initialize memory for fields that driver writes to history (not coming via MARBL diagnostic type)
    !        as well as fields that driver reads (forcing fields)
    allocate(surface_fluxes(num_cols, num_tracers))
    allocate(interior_tendencies(num_tracers, num_levels, num_cols))
    allocate(bot_flux_to_tend(num_levels, num_cols), source=c0)
    do col_id = 1, num_cols
      bot_flux_to_tend(active_level_cnt(col_id),col_id) = 1._r8 / grid_data%delta_z(active_level_cnt(col_id))
    end do
    allocate(tracer_initial_vals(num_tracers, num_levels, num_cols))
    allocate(surface_flux_forcings(size(marbl_instances(1)%surface_flux_forcings)))
    allocate(interior_tendency_forcings(size(marbl_instances(1)%interior_tendency_forcings)))
    ! Allocate memory inside surface_flux_forcings
    do m=1, size(marbl_instances(1)%surface_flux_forcings)
      if (associated(marbl_instances(1)%surface_flux_forcings(m)%field_0d)) then
        allocate(surface_flux_forcings(m)%field_0d(num_cols))
      else
        allocate(surface_flux_forcings(m)%field_1d(num_cols, &
            size(marbl_instances(1)%surface_flux_forcings(m)%field_1d, dim=2)))
      end if
    end do
    ! Allocate memory inside interior_tendency_forcings
    do m=1, size(marbl_instances(1)%interior_tendency_forcings)
      if (associated(marbl_instances(1)%interior_tendency_forcings(m)%field_0d)) then
        allocate(interior_tendency_forcings(m)%field_0d(num_cols))
      else
        allocate(interior_tendency_forcings(m)%field_1d(num_cols, &
            size(marbl_instances(1)%interior_tendency_forcings(m)%field_1d, dim=2)))
      end if
    end do

    
    !    (d) netCDF calls to create history file (dimensions are defined but data is not written)
    !call marbl_io_define_history(marbl_instances, col_cnt, unit_system_opt, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_define_history', subname)
      print *, "Aborting after failing to create history file"
      return
    end if

    call system_clock(count_start)
    !    (e) Read initial conditions and forcing data
    do n=1, num_cols
      !      (i) Read tracer values over full column
      call marbl_io_read_tracers(n, marbl_instances(1)%tracer_metadata, tracer_initial_vals(:,:,n), driver_status_log)
      if (driver_status_log%labort_marbl) then
        call driver_status_log%log_error_trace('read_tracers', subname)
        print *, "Aborting after failing to read tracers"
        return
      end if

      !      (ii) Read surface flux forcing fields
      call marbl_io_read_forcing_field(n, lat(n), unit_system_opt, marbl_instances(1)%surface_flux_forcings, &
                                       surface_flux_forcings, driver_status_log)
      if (driver_status_log%labort_marbl) then
        call driver_status_log%log_error_trace('read_forcing_field(surface)', subname)
        print *, "Aborting after failing to read surface flux forcing fields"
        return
      end if

      !      (iii) Read interior tendency forcing fields
      call marbl_io_read_forcing_field(n, lat(n), unit_system_opt, marbl_instances(1)%interior_tendency_forcings, &
                                       interior_tendency_forcings, driver_status_log, active_level_cnt(n))
      if (driver_status_log%labort_marbl) then
        call driver_status_log%log_error_trace('read_forcing_field(interior)', subname)
        print *, "Aborting after failing to read interior tendency forcing fields"
        return
      end if
    end do
    call system_clock(count_end)
    elapsed = real(count_end - count_start) / real(count_rate)
    time_io = time_io + elapsed
    


    ! --------------------------------------------------------------------------
    ! Brunt of MARBL computations
    ! --------------------------------------------------------------------------

    
    do n=1, size(marbl_instances)

      ! 6. Call surface_flux_compute() (all columns simultaneously)
      !    (a) call set_global_scalars() for consistent setting of time-varying scalars
      !        [surface_flux computation doesn't currently have any time-varying scalars]
      call system_clock(count_start)
      call marbl_instances(n)%set_global_scalars('surface_flux')
      do col_id_loc = 1, col_cnt(n)
        col_id = col_start(n)+col_id_loc

        !  (b) copy surface tracer values into marbl_instances(n)%tracers_at_surface
        marbl_instances(n)%tracers_at_surface(col_id_loc, :) = tracer_initial_vals(:, 1, col_id)

        !  (c) copy surface flux forcings into marbl_instances(n)%surface_flux_forcings
        do m=1, size(marbl_instances(n)%surface_flux_forcings)
          if (associated(marbl_instances(n)%surface_flux_forcings(m)%field_0d)) then
            marbl_instances(n)%surface_flux_forcings(m)%field_0d(col_id_loc) = surface_flux_forcings(m)%field_0d(col_id)
          else
            marbl_instances(n)%surface_flux_forcings(m)%field_1d(col_id_loc,:) = surface_flux_forcings(m)%field_1d(col_id,:)
          end if
        end do
      end do
      
      !    (d) populate marbl_instances(n)%surface_flux_saved_state (with 0s)
      do m=1, size(marbl_instances(n)%surface_flux_saved_state%state)
        marbl_instances(n)%surface_flux_saved_state%state(m)%field_2d(:) = c0
      end do
      call system_clock(count_end)
      time_setting_surface = time_setting_surface + (real(count_end - count_start) / real(count_rate))

      call system_clock(count_start)
      !    (e) call surface_flux_compute()
      call marbl_instances(n)%surface_flux_compute()
      if (marbl_instances(n)%StatusLog%labort_MARBL) then
        call marbl_instances(n)%StatusLog%log_error_trace('surface_flux_compute', subname)
        print *, "Aborting after failing to compute surface fluxes"
        return
      end if
      call system_clock(count_end)
      time_compute_surface = time_compute_surface + (real(count_end - count_start) / real(count_rate))
      
      !    (f) write to diagnostic buffers
      !        Note: passing col_start and col_cnt => surface flux diagnostic buffer
      call marbl_io_copy_into_diag_buffer(col_start(n), col_cnt(n), marbl_instances(n))
      surface_fluxes((col_start(n)+1):(col_start(n)+col_cnt(n)),:) = marbl_instances(n)%surface_fluxes(:,:)
      do output_id = 1, size(marbl_instances(n)%surface_flux_output%outputs_for_GCM)
        surface_flux_output((col_start(n)+1):(col_start(n)+col_cnt(n)),output_id) = &
                  marbl_instances(n)%surface_flux_output%outputs_for_GCM(output_id)%forcing_field_0d(:)
      end do
      ! ------------------------------------------------------------------------

      ! 7. Call interior_tendency_compute() (one column at a time)
      do col_id_loc = 1, col_cnt(n)
        col_id = col_start(n)+col_id_loc
        call system_clock(count_start)
        !  (a) call set_global_scalars() for consistent setting of time-varying scalars
        !      [necessary when running ladjust_bury_coeff, since GCM is responsible
        !       for computing running means of values needed to compute burial coefficients]
        call marbl_instances(n)%set_global_scalars('interior_tendency')

        !  (b) set domain information
        !      In this example, the vertical grid is the same from column to column and
        !      therefore set during initialization. The columns vary in depth, so
        !      the index of the bottom layer must be updated for each column.
        marbl_instances(n)%domain%kmt = active_level_cnt(col_id)
        !  (c) copy tracer values into marbl_instances(n)%tracers
        marbl_instances(n)%tracers = tracer_initial_vals(:,:,col_id)

        !  (d) copy interior tendency forcings into marbl_instances(n)%interior_tendency_forcings
        do m=1, size(marbl_instances(n)%interior_tendency_forcings)
          if (associated(marbl_instances(n)%interior_tendency_forcings(m)%field_0d)) then
            marbl_instances(n)%interior_tendency_forcings(m)%field_0d(1) = &
                 interior_tendency_forcings(m)%field_0d(col_id)
          else
            marbl_instances(n)%interior_tendency_forcings(m)%field_1d(1,:) = &
                 interior_tendency_forcings(m)%field_1d(col_id,:)
          end if
        end do

        !  (e) populate marbl_instances(n)%interior_tendency_saved_state (with 0s)
        do m=1, size(marbl_instances(n)%interior_tendency_saved_state%state)
          if (allocated(marbl_instances(n)%interior_tendency_saved_state%state(m)%field_2d)) then
            marbl_instances(n)%interior_tendency_saved_state%state(m)%field_2d(:) = c0
          else
            marbl_instances(n)%interior_tendency_saved_state%state(m)%field_3d(:,1) = c0
          end if
        end do

        !  (f) set bot_flux_to_tend(:) [1/dz in level kmt, 0 elsewhere]
        marbl_instances(n)%bot_flux_to_tend(:) = bot_flux_to_tend(:,col_id)
        call system_clock(count_end)
        time_setting_interior = time_setting_interior + (real(count_end - count_start) / real(count_rate))

        call system_clock(count_start)
        !  (g) call interior_tendency_compute()
        call marbl_instances(n)%interior_tendency_compute()
        if (marbl_instances(n)%StatusLog%labort_MARBL) then
          call marbl_instances(n)%StatusLog%log_error_trace('interior_tendency_compute', subname)
          print *, "Aborting after failing to compute interior tendencies"
          return
        end if
        call system_clock(count_end)
        time_compute_interior = time_compute_interior + (real(count_end - count_start) / real(count_rate))

        !  (h) write to diagnostic buffer
        !        Note: passing just col_id => interior tendency diagnostic buffer
        call marbl_io_copy_into_diag_buffer(col_id, marbl_instances(n))
        interior_tendencies(:,:,col_id) = marbl_instances(n)%interior_tendencies(:,:)
        call marbl_instances(n)%get_output_for_GCM(output_for_GCM_iopt_total_Chl_3d, total_Chl(:,col_id))
        
      end do ! column

      call system_clock(count_start)
      do b = 1,60
        do a = 1,32
          interior(n,b,a) = interior(n,b,a) + marbl_instances(n)%interior_tendencies(a,b)
        end do
      end do
      do b = 1,32
        surface(1,b,n) = surface(1,b,n) + marbl_instances(n)%surface_fluxes(1,b)
      end do
      call system_clock(count_end)
      elapsed = real(count_end - count_start) / real(count_rate)
      !untracked = untracked + elapsed
      ! the above might be considered as part of the compute steps
    end do ! instance
    
    ! --------------------------------------------------------------------------


    ! 8. Output netCDF
    !call marbl_io_write_history(marbl_instances(1), surface_fluxes, interior_tendencies,  &
    !                            surface_flux_output, total_Chl, tracer_initial_vals,      &
    !                            active_level_cnt, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_write_history', subname)
      print *, "Aborting after failing to write history"
      return
    end if

    ! --------------------------------------------------------------------------

    ! 9. Close all netCDF files
    call marbl_io_close_files(driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('marbl_io_close_files', subname)
      print *, "Aborting after failing to close files"
      return
    end if

    ! --------------------------------------------------------------------------

    ! 10. Deallocate local variables as well as those in marbl_io_mod
    call marbl_io_destruct_diag_buffers()
    deallocate(surface_fluxes)
    deallocate(interior_tendencies)
    deallocate(bot_flux_to_tend)
    deallocate(tracer_initial_vals)
    ! Deallocate memory inside surface_flux_forcings
    do m=1, size(marbl_instances(1)%surface_flux_forcings)
      if (associated(marbl_instances(1)%surface_flux_forcings(m)%field_0d)) then
        deallocate(surface_flux_forcings(m)%field_0d)
      else
        deallocate(surface_flux_forcings(m)%field_1d)
      end if
    end do
    ! Deallocate memory inside interior_tendency_forcings
    do m=1, size(marbl_instances(1)%interior_tendency_forcings)
      if (associated(marbl_instances(1)%interior_tendency_forcings(m)%field_0d)) then
        deallocate(interior_tendency_forcings(m)%field_0d)
      else
        deallocate(interior_tendency_forcings(m)%field_1d)
      end if
    end do
    deallocate(surface_flux_forcings)
    deallocate(interior_tendency_forcings)
    deallocate(active_level_cnt)
    deallocate(col_start)
    deallocate(col_cnt)

    ! --------------------------------------------------------------------------

    ! 11. Shutdown MARBL
    do n=1, size(marbl_instances)
      call marbl_instances(n)%shutdown()
    end do
    
  end subroutine test

  !*****************************************************************************

  subroutine set_domain(num_insts, unit_system_opt, num_levels, active_level_cnt, lat, &
                        num_PAR_subcols, col_start, col_cnt, grid_data, driver_status_log)

    use marbl_tools_mod, only : marbl_tools_distribute_cols
    use marbl_io_mod, only : marbl_io_read_domain

    integer,                             intent(in)    :: num_insts
    character(len=*),                    intent(in)    :: unit_system_opt
    integer,                             intent(out)   :: num_levels
    integer,  dimension(:), allocatable, intent(out)   :: active_level_cnt
    real(r8), dimension(:), allocatable, intent(out)   :: lat
    integer,                             intent(out)   :: num_PAR_subcols
    integer,  dimension(:), allocatable, intent(out)   :: col_start
    integer,  dimension(:), allocatable, intent(out)   :: col_cnt
    type(grid_data_type),                intent(inout) :: grid_data
    type(marbl_log_type),                intent(inout) :: driver_status_log

    character(len=*), parameter :: subname = 'marbl_call_compute_subroutines_drv:set_domain'
    integer :: num_cols

    ! 1. Read domain info from netCDF file
    call marbl_io_read_domain(unit_system_opt, grid_data, active_level_cnt, lat, num_cols, &
                              num_levels, num_PAR_subcols, driver_status_log)
    if (driver_status_log%labort_marbl) then
      call driver_status_log%log_error_trace('read_domain', subname)
      print *, "Aborting after failing to read domain"
      return
    end if

    ! 2. Domain decomposition (distribute columns among instances)
    call marbl_tools_distribute_cols(num_cols, num_insts, col_start, col_cnt, driver_status_log)
    ! NOTE: driver_status_log gets information but there are no abort conditions

  end subroutine set_domain

  !****************************************************************************

end module call_compute_subroutine
