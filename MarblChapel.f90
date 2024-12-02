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
    integer(c_int) :: marbl_settings_in, open_status, read_status
    character(len=256) :: namelist_line
    character(len=256) :: marbl_namelist_fname='marbl_with_o2_consumptions_scalef.settings'

    ! Get the pointer to the marbl instance
    call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

    ! Read the default settings file
    open(action='read', unit=marbl_settings_in, file=marbl_namelist_fname, &
      iostat=open_status)
    read(marbl_settings_in, "(A)", iostat=read_status) namelist_line
    close(marbl_settings_in)

    ! Populate the marbl instance with the file settings
    call marbl_instance%put_setting(namelist_line)

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

  
end module MarblChapel