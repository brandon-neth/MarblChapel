module MarblChapel
  ! This module contains code for interoperating between Chapel code and 
  ! the Fortran 90 biogeochemical library MARBL. The Fortran library's 
  ! repository is found here: https://github.com/marbl-ecosys/MARBL/tree/marbl0.45.0
  ! Note that these capabilities are tested with MARBL version 0.45.0. 
  ! Functionality with other versions of the library is not guaranteed.

  ! MARBL interoperability works using three layers: One Chapel, one C, 
  ! one Fortran. First, there is the Chapel layer that contains the `MarblInteropType` 
  ! record and a collection of `extern` procedures. The `MarblInteropType` record 
  ! is the main way users will interact with the MARBL library, using the methods 
  ! of the record. Second, there is the C header layer, which connects the Chapel 
  ! `extern` procedures to their definitions in Fortran. Finally, there is the Fortran 
  ! layer, found in this file, which defines the various `extern` procedures 
  ! declared in the Chapel layer. 

  use marbl_interface
  use marbl_kinds_mod
  use iso_c_binding

  implicit none

  type, bind(C) :: MarblInteropType
    type(c_ptr) :: marbl_obj
  end type marblInteropType

  contains

  subroutine init_interop_obj(interop_obj) bind(C, name='init_interop_obj')
    ! This subroutine allocates a MARBL instance object and connects 
    ! in to the interop object passed in from the Chapel-side. It does NOT 
    ! initialize the MARBL instance. That is done with the `init_marbl_instance` 
    ! subroutine on the Fortran side, which is called by the `initMarblInstance` 
    ! method on the Chapel side.
    implicit none
    type(MarblInteropType), intent(inout) :: interop_obj
    type(marbl_interface_class), pointer :: marbl_instance_ptr

    allocate(marbl_instance_ptr)
    interop_obj%marbl_obj = c_loc(marbl_instance_ptr)
  end subroutine init_interop_obj

  subroutine deinit_interop_obj(interop_obj) bind(C, name='deinit_interop_obj')
    ! This subroutine deallocates a MARBL instance object
    implicit none
    type(MarblInteropType), intent(inout) :: interop_obj
    type(marbl_interface_class), pointer :: marbl_instance

    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)
    
    call marbl_instance%shutdown()
    deallocate(marbl_instance)
  end subroutine deinit_interop_obj

  subroutine import_settings(interop_obj, filename, filename_len) bind(C, name='import_settings')
    ! This subroutine reads a marbl settings file and applies those settings to the MARBL instance.
    ! The caller is responsible for ensuring that only one thread is accessing the settings file at a time.
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    integer(c_int), intent(in) :: filename_len
    character(kind=c_char), dimension(filename_len), intent(in) :: filename
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance
    character(kind=c_char, len=filename_len) :: file_path
    integer(c_int) :: marbl_settings_in, open_status, read_status
    character(len=256) :: namelist_line
    
    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Convert the C-style string to a Fortran-style stirng
    file_path = transfer(filename, file_path)
    ! Read the settings file
    open(action='read', unit=marbl_settings_in, file=file_path, &
      iostat=open_status)
    if (open_status /= 0) then
      print *, 'Failed to open settings file "', trim(file_path), '"'
      return
    end if
    read(marbl_settings_in, "(A)", iostat=read_status) namelist_line
    close(marbl_settings_in)
    if (read_status /= 0) then
      print *, 'Failed to read settings file "', trim(file_path), '"'
      return
    end if
    
    ! Populate the marbl instance with the file settings
    call marbl_instance%put_setting(namelist_line)
  end subroutine import_settings

  subroutine init_marbl_instance(interop_obj, num_levels, num_PAR_subcols, &
    num_elements_surface_flux, delta_z, zw, zt, active_level_count) &
    bind(C,name='init_marbl_instance')
    ! This subroutine initializes the MARBL instance with the given settings
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    integer(c_int), intent(in) :: num_levels, num_PAR_subcols, num_elements_surface_flux
    real(c_double), intent(in) :: delta_z(num_levels), zw(num_levels), zt(num_levels)
    integer(c_int), intent(in) :: active_level_count
    ! Local variables
    type(marbl_interface_class), pointer :: marbl_instance
    integer :: m

    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Initialize the marbl instance
    call marbl_instance%init(gcm_num_levels=num_levels, &
      gcm_num_PAR_subcols=num_PAR_subcols, &
      gcm_num_elements_surface_flux=num_elements_surface_flux, &
      gcm_delta_z=delta_z(:), gcm_zw=zw(:), gcm_zt=zt(:), &
      lgcm_has_global_ops=.true., unit_system_opt='mks')

    ! Update the domain 
    marbl_instance%domain%kmt = active_level_count
    marbl_instance%bot_flux_to_tend(:) = 0.0    
    marbl_instance%bot_flux_to_tend(marbl_instance%domain%kmt) = 1._r8/marbl_instance%domain%delta_z(marbl_instance%domain%kmt) 

    ! Initialize the saved state variables to zero
    do m=1,size(marbl_instance%surface_flux_saved_state%state)
      if (marbl_instance%surface_flux_saved_state%state(m)%rank == 2) then
        marbl_instance%surface_flux_saved_state%state(m)%field_2d = 0.0
      end if
      if (marbl_instance%surface_flux_saved_state%state(m)%rank == 3) then
        marbl_instance%surface_flux_saved_state%state(m)%field_3d = 0.0
      end if
    end do
    do m=1,size(marbl_instance%interior_tendency_saved_state%state)
      if (marbl_instance%interior_tendency_saved_state%state(m)%rank == 2) then
        marbl_instance%interior_tendency_saved_state%state(m)%field_2d = 0.0
      end if
      if (marbl_instance%interior_tendency_saved_state%state(m)%rank == 3) then
        marbl_instance%interior_tendency_saved_state%state(m)%field_3d = 0.0
      end if
    end do
  end subroutine init_marbl_instance

  subroutine set_surface_flux_forcing_value(interop_obj, variable_name, &
    vn_len, value) bind(C,name='set_surface_flux_forcing_value')
    ! This subroutine sets individual surface flux forcing values.
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    integer(c_int), intent(in) :: vn_len
    character(kind=c_char), dimension(vn_len), intent(in) :: variable_name
    real(c_double), intent(in) :: value
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance
    character(kind=c_char, len=vn_len) :: v_name
    integer :: idx
    logical :: found

    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Transfer the C-style string to a Fortran-style string
    v_name = transfer(variable_name, v_name)
    
    ! Find the correct surface flux forcing index and populate the value
    found = .false.
    do idx = 1,size(marbl_instance%surface_flux_forcings)
      if (trim(marbl_instance%surface_flux_forcings(idx)%metadata%varname) == v_name) then
        found = .true.
        marbl_instance%surface_flux_forcings(idx)%field_0d(1) = value
      end if
    end do

    ! Print error if the variable was not found
    if (.not. found) then
      print *, 'Failed to set surface flux forcing value. Could not find variable with name "', v_name, '"'
    end if
  end subroutine set_surface_flux_forcing_value

  subroutine set_surface_tracers(interop_obj, nt, nz, tracer_array) bind(C,name='set_surface_tracers')
    ! This subroutine copies the tracer values into the the MARBL instance for 
    ! computing the surface fluxes
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    integer(c_int), intent(in) :: nt, nz
    real(c_double), intent(in) :: tracer_array(nz, nt)
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance
    integer :: m

    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Copy in the tracer values
    marbl_instance%tracers_at_surface(:,:) = 0
    do m=1,nt
      marbl_instance%tracers_at_surface(1,m) = tracer_array(1,m)
    end do
  end subroutine set_surface_tracers

  subroutine compute_surface_fluxes(interop_obj) bind(C,name='compute_surface_fluxes')
    ! This routine executes the surface flux computation in the MARBL instance
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance

    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Set the global scalar values for this computation
    call marbl_instance%set_global_scalars('surface_flux')

    ! Call the computation subroutine
    call marbl_instance%surface_flux_compute()
  end subroutine compute_surface_fluxes

  subroutine update_surface_fluxes(interop_obj, nt, nz, tracer_array, dt) bind(C, name='update_surface_fluxes')
    ! This routine updates the `tracer_array` with the tendencies 
    ! calculated by the surface_flux_compute subroutine.
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    integer(c_int), intent(in) :: nz, nt
    real(c_double), intent(inout) :: tracer_array(nz,nt)
    real(c_double), intent(in) :: dt
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance
    integer :: m

    ! Get the pointer to the marbl object itself
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Increment the tracer array with the calculated fluxes
    do m=1,nt
      tracer_array(nz,m) = tracer_array(nz,m) + marbl_instance%surface_fluxes(1,m) * dt
    end do
  end subroutine update_surface_fluxes

  subroutine set_interior_tendency_forcing_array(interop_obj, variable_name, &
    vn_len, data, num_elements) bind(C, name='set_interior_tendency_forcing_array')
    ! This subroutine sets an interior tendency forcing vector.
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    integer(c_int), intent(in) :: vn_len, num_elements
    character(kind=c_char), dimension(vn_len), intent(in) :: variable_name
    real(c_double), intent(in) :: data(num_elements)
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance
    character(kind=c_char, len=vn_len) :: v_name
    integer :: idx
    logical :: found

    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Transfer the C-style string to a Fortran-style string
    v_name = transfer(variable_name, v_name)
    
    ! Find the correct interior tendency forcing index and populate the value
    found = .false.
    do idx = 1,size(marbl_instance%interior_tendency_forcings)
      if (trim(MARBL_instance%interior_tendency_forcings(idx)%metadata%varname) == v_name) then
        marbl_instance%interior_tendency_forcings(idx)%field_1d(:,1) = 0.0_r8
        marbl_instance%interior_tendency_forcings(idx)%field_1d(1:num_elements,1) = data(:)
        found = .true.
      end if
    end do

    if (.not. found) then
      print *, 'Failed to set interior tendency forcing array. Could not find variable with name "', v_name, '"'
    end if
  end subroutine set_interior_tendency_forcing_array

  subroutine set_interior_tendency_forcing_scalar(interop_obj, variable_name, &
    vn_len, value) bind(C, name='set_interior_tendency_forcing_scalar')
    ! This subroutine sets an interior tendency forcing scalar value.
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    integer(c_int), intent(in) :: vn_len
    character(kind=c_char), dimension(vn_len), intent(in) :: variable_name
    real(c_double), intent(in) :: value
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance
    character(kind=c_char, len=vn_len) :: v_name
    integer :: idx
    logical :: found

    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Transfer the C-style string to a Fortran-style string
    v_name = transfer(variable_name, v_name)
    
    ! Find the correct interior tendency forcing index and populate the value
    found = .false.
    do idx = 1,size(marbl_instance%interior_tendency_forcings)
      if (trim(MARBL_instance%interior_tendency_forcings(idx)%metadata%varname) == v_name) then
        marbl_instance%interior_tendency_forcings(idx)%field_0d(1) = value
        found = .true.
      end if
    end do

    if (.not. found) then
      print *, 'Failed to set interior tendency forcing scalar. Could not find variable with name "', v_name, '"'
    end if
  end subroutine set_interior_tendency_forcing_scalar


  subroutine set_tracers(interop_obj, nt, nz, tracer_array) bind(C,name='set_tracers')
    ! This subroutine copies the tracer values into the the MARBL instance for 
    ! computing the interior tendencies
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    integer(c_int), intent(in) :: nt, nz
    real(c_double), intent(in) :: tracer_array(nz, nt)
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance
    integer :: m

    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Copy in the tracer values
    do m=1,nt
      marbl_instance%tracers(m,:) = tracer_array(:,m)
    end do
  end subroutine set_tracers


  subroutine compute_interior_tendencies(interop_obj) bind(C,name='compute_interior_tendencies')
    ! This routine executes the interior tendency computation in the MARBL instance
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance
    
    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Set the global scalar values for this computation
    call marbl_instance%set_global_scalars('interior_tendency')

    ! Call the computation subroutine
    call marbl_instance%interior_tendency_compute()
  end subroutine compute_interior_tendencies

  subroutine update_interior_tendencies(interop_obj, nt, nz, tracer_array, dt) bind(C, name='update_interior_tendencies')
    ! This routine updates the `tracer_array` with the tendencies 
    ! calculated by the interior_tendency_compute subroutine.
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    integer(c_int), intent(in) :: nz, nt
    real(c_double), intent(inout) :: tracer_array(nz,nt)
    real(c_double), intent(in) :: dt
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance
    integer :: m

    ! Get the pointer to the marbl object itself
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Increment the tracer array with the calculated tendencies
    do m=1,nt
      tracer_array(:,m) = tracer_array(:,m) + marbl_instance%interior_tendencies(m,:) * dt
    end do
  end subroutine update_interior_tendencies


  subroutine get_diagnostic_value_2d(interop_obj, variable_name, vn_len, ptr_out, ptr_out_len) bind(C, name='get_diagnostic_value_2d')
    ! This routine populates ptr_out and ptr_out_len with the value of the diagnostic variable and its length. 
    ! While the diagnostic variable uses '2d' in its name, it is actually a 1D variable, so we only have one length to return.
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    integer(c_int), intent(in) :: vn_len
    character(kind=c_char), dimension(vn_len), intent(in) :: variable_name
    type(c_ptr), intent(out) :: ptr_out
    integer(c_int), intent(out) :: ptr_out_len
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance
    character(kind=c_char, len=vn_len) :: v_name
    integer :: idx
    logical :: found
    integer :: data_len
    real(c_double), pointer, dimension(:) :: data_ptr


    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Transfer the C-style string to a Fortran-style string
    v_name = transfer(variable_name, v_name)

    ! Find the correct diagnostic variable index
    found = .false.

    ! First check interior_tendency_compute_diagnostics
    do idx = 1,size(marbl_instance%interior_tendency_diags%diags)
      if (trim(marbl_instance%interior_tendency_diags%diags(idx)%short_name) == v_name &
          .or. trim(marbl_instance%interior_tendency_diags%diags(idx)%long_name) == v_name) then
        found = .true.
        if ('none' .ne. trim(marbl_instance%interior_tendency_diags%diags(idx)%vertical_grid)) then
          print *, 'Warning: diagnostic variable ', v_name, ' is not a 2D variable.'
          ptr_out = c_null_ptr
          ptr_out_len = -1
          return
        end if
        data_ptr => marbl_instance%interior_tendency_diags%diags(idx)%field_2d
        data_len = size(marbl_instance%interior_tendency_diags%diags(idx)%field_2d)
      end if
    end do

    ! Second, check surface_flux_diagnostics
    do idx = 1,size(marbl_instance%surface_flux_diags%diags)
      if (trim(marbl_instance%surface_flux_diags%diags(idx)%short_name) == v_name &
          .or. trim(marbl_instance%surface_flux_diags%diags(idx)%long_name) == v_name) then
        found = .true.
        if ('none' .ne. trim(marbl_instance%interior_tendency_diags%diags(idx)%vertical_grid)) then
          print *, 'Warning: diagnostic variable ', v_name, ' is not a 2D variable.'
          ptr_out = c_null_ptr
          ptr_out_len = -1
          return
        end if
        data_ptr => marbl_instance%surface_flux_diags%diags(idx)%field_2d
        data_len = size(marbl_instance%surface_flux_diags%diags(idx)%field_2d)
        print *, 'found in surface_flux_diags.'
        print *, 'data: ', data_ptr
      end if
    end do

    if (.not. found) then
      print *, 'Failed to get diagnostic value. Could not find variable with name "', v_name, '"'
      ptr_out = c_null_ptr
      ptr_out_len = -1
      return
    end if

    ! Return the pointer to the data and its length
    ptr_out = c_loc(data_ptr)
    ptr_out_len = data_len
  end subroutine get_diagnostic_value_2d

  subroutine get_diagnostic_value_3d(interop_obj, variable_name, vn_len, ptr_out, ptr_out_len1, ptr_out_len2) bind(C, name='get_diagnostic_value_3d')
    ! This routine populates ptr_out and ptr_out_len with the value of the diagnostic variable and its length.
    ! While the diagnostic variable uses '3d' in its name, it is actually a 2D variable, so we only have two lengths to return.
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    integer(c_int), intent(in) :: vn_len
    character(kind=c_char), dimension(vn_len), intent(in) :: variable_name
    type(c_ptr), intent(out) :: ptr_out
    integer(c_int), intent(out) :: ptr_out_len1
    integer(c_int), intent(out) :: ptr_out_len2
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance
    character(kind=c_char, len=vn_len) :: v_name
    integer :: idx
    logical :: found
    real(c_double), pointer, dimension(:,:) :: data_ptr
    type(c_ptr) :: ptr_out2

    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Transfer the C-style string to a Fortran-style string
    v_name = transfer(variable_name, v_name)

    ! Find the correct diagnostic variable index
    found = .false.

    ! First check interior_tendency_compute_diagnostics
    do idx = 1,size(marbl_instance%interior_tendency_diags%diags)
      if (trim(marbl_instance%interior_tendency_diags%diags(idx)%short_name) == v_name &
          .or. trim(marbl_instance%interior_tendency_diags%diags(idx)%long_name) == v_name) then
        found = .true.
        if ('none' .eq. trim(marbl_instance%interior_tendency_diags%diags(idx)%vertical_grid)) then
          print *, 'Warning: diagnostic variable ', v_name, ' is not a 3D variable.'
          ptr_out = c_null_ptr
          ptr_out_len1 = -1
          ptr_out_len2 = -1
          return
        end if
        data_ptr => marbl_instance%interior_tendency_diags%diags(idx)%field_3d
        ptr_out_len1 = size(marbl_instance%interior_tendency_diags%diags(idx)%field_3d,1)
        ptr_out_len2 = size(marbl_instance%interior_tendency_diags%diags(idx)%field_3d,2)
      end if
    end do

    ! Second, check surface_flux_diagnostics
    do idx = 1,size(marbl_instance%surface_flux_diags%diags)
      if (trim(marbl_instance%surface_flux_diags%diags(idx)%short_name) == v_name &
          .or. trim(marbl_instance%surface_flux_diags%diags(idx)%long_name) == v_name) then
        found = .true.
        if ('none' .eq. trim(marbl_instance%interior_tendency_diags%diags(idx)%vertical_grid)) then
          print *, 'Warning: diagnostic variable ', v_name, ' is not a 3D variable.'
          ptr_out = c_null_ptr
          ptr_out_len1 = -1
          ptr_out_len2 = -1
          return
        end if
        data_ptr => marbl_instance%surface_flux_diags%diags(idx)%field_3d
        ptr_out_len1 = size(marbl_instance%surface_flux_diags%diags(idx)%field_3d,1)
        ptr_out_len2 = size(marbl_instance%surface_flux_diags%diags(idx)%field_3d,2)
      end if
    end do

    if (.not. found) then
      print *, 'Failed to get diagnostic value. Could not find variable with name "', v_name, '"'
      ptr_out = c_null_ptr
      ptr_out_len1 = -1
      ptr_out_len2 = -1
      return
    end if
    
    ! Return the pointer to the data and its length
    ptr_out = c_loc(data_ptr)

  end subroutine get_diagnostic_value_3d

  subroutine num_interior_tendency_diagnostics(interop_obj, num_diagnostics) bind(C, name='num_interior_tendency_diagnostics')
    ! This routine populates num_diagnostics with the number of interior tendency diagnostics.
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    integer(c_int), intent(out) :: num_diagnostics
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance

    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Return the number of interior tendency diagnostics
    num_diagnostics = size(marbl_instance%interior_tendency_diags%diags)
  end subroutine num_interior_tendency_diagnostics

  subroutine num_surface_flux_diagnostics(interop_obj, num_diagnostics) bind(C, name='num_surface_flux_diagnostics')
    ! This routine populates num_diagnostics with the number of surface flux diagnostics.
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    integer(c_int), intent(out) :: num_diagnostics
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance

    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Return the number of surface flux diagnostics
    num_diagnostics = size(marbl_instance%surface_flux_diags%diags)
  end subroutine num_surface_flux_diagnostics

  subroutine get_interior_tendency_diagnostic_name(interop_obj, idx, ptr_out, ptr_out_len, dim_out) bind(C, name='get_interior_tendency_diagnostic_name')
    ! This routine populates ptr_out and ptr_out_len with the name of the diagnostic variable, its length, and the variable's dimensionality.
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    integer(c_int), intent(in) :: idx
    type(c_ptr), intent(out) :: ptr_out
    integer(c_int), intent(out) :: ptr_out_len
    integer(c_int), intent(out) :: dim_out
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance
    character(len=256), pointer :: name
    integer :: name_len

    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    if (idx > size(marbl_instance%interior_tendency_diags%diags)) then
      print *, 'get_interior_diagnostic_name: Index out of bounds.'
      ptr_out = c_null_ptr
      ptr_out_len = -1
      dim_out = -1
      return
    end if

    allocate(name)

    ! Get the name of the diagnostic variable
    name = marbl_instance%interior_tendency_diags%diags(idx)%short_name
    name_len = len_trim(name)

    ! Return the pointer to the name and its length
    ptr_out = c_loc(name)
    ptr_out_len = name_len
    if ('none' .eq. trim(marbl_instance%interior_tendency_diags%diags(idx)%vertical_grid)) then
      dim_out = 2
    else
      dim_out = 3
    end if

  end subroutine get_interior_tendency_diagnostic_name

  subroutine get_surface_flux_diagnostic_name(interop_obj, idx, ptr_out, ptr_out_len, dim_out) bind(C, name='get_surface_flux_diagnostic_name')
    ! This routine populates ptr_out and ptr_out_len with the name of the diagnostic variable, its length, and the variable's dimensionality.
    implicit none
    ! Parameters
    type(marblInteropType), intent(inout) :: interop_obj
    integer(c_int), intent(in) :: idx
    type(c_ptr), intent(out) :: ptr_out
    integer(c_int), intent(out) :: ptr_out_len
    integer(c_int), intent(out) :: dim_out
    ! Local Variables
    type(marbl_interface_class), pointer :: marbl_instance
    character(len=256), pointer :: name
    integer :: name_len

    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)
    
    if (idx > size(marbl_instance%interior_tendency_diags%diags)) then
      print *, 'get_surface_flux_diagnostic_name: Index out of bounds: ', idx
      ptr_out = c_null_ptr
      ptr_out_len = -1
      dim_out = -1
      return
    end if
    allocate(name)

    ! Get the name of the diagnostic variable
    name = marbl_instance%surface_flux_diags%diags(idx)%short_name
    name_len = len_trim(name)

    ! Return the pointer to the name and its length
    ptr_out = c_loc(name)
    ptr_out_len = name_len
    if ('none' .eq. trim(marbl_instance%surface_flux_diags%diags(idx)%vertical_grid)) then
      dim_out = 2
    else
      dim_out = 3
    end if
    
  end subroutine get_surface_flux_diagnostic_name
end module MarblChapel