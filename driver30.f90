module driver27
use marbl_interface
use marbl_kinds_mod
use iso_c_binding
implicit none

type, bind(C) :: marbl_interop_type
  type(c_ptr) :: marbl_obj
end type marbl_interop_type

contains

subroutine init_interop_obj(interop_obj) bind(C,name='init_interop_obj')
  type(marbl_interop_type), intent(inout) :: interop_obj
  type(marbl_interface_class), pointer :: marbl_obj_ptr

  allocate(marbl_obj_ptr)
  interop_obj%marbl_obj = c_loc(marbl_obj_ptr)
end subroutine init_interop_obj

subroutine init_marbl_instance(interop_obj, gcm_num_levels, gcm_num_PAR_subcols, gcm_num_elements_surface_flux, gcm_delta_z, gcm_zw, gcm_zt) bind(C,name='init_marbl_instance')
  use marbl_logging, only : marbl_status_log_entry_type
  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  integer(c_int), intent(in)    :: gcm_num_levels
  integer(c_int), intent(in)    :: gcm_num_PAR_subcols
  integer(c_int), intent(in)    :: gcm_num_elements_surface_flux
  real(c_double), intent(in)    :: gcm_delta_z(gcm_num_levels)
  real(c_double), intent(in)    :: gcm_zw(gcm_num_levels)
  real(c_double), intent(in)    :: gcm_zt(gcm_num_levels)
  integer(c_int) :: marbl_settings_in
  type(marbl_interface_class), pointer :: marbl_obj_ptr
  type(marbl_interface_class) :: marbl_obj

  integer :: i, nz

  integer :: open_status, read_status
  character(len=256) :: namelist_line
  character(len=256) :: marbl_namelist_fname='marbl_with_o2_consumption_scalef.settings'
  character(len=256) ::marbl_tracer_list_fname='marbl_tracer_output_list'
  type(marbl_status_log_entry_type), pointer :: tmp
  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_obj_ptr)

  nz = gcm_num_levels

  open(action='read', unit=marbl_settings_in, file=marbl_namelist_fname, iostat=open_status)
  read(marbl_settings_in,"(A)",iostat=read_status) namelist_line
  close(marbl_settings_in)
  
  call marbl_obj_ptr%put_setting(namelist_line)

  call marbl_obj_ptr%init(gcm_num_levels=gcm_num_levels, gcm_num_PAR_subcols = 6, gcm_num_elements_surface_flux = 5, gcm_delta_z = gcm_delta_z(:), gcm_zw = gcm_zw(:),      gcm_zt = gcm_zt(:),      lgcm_has_global_ops = .true., unit_system_opt='mks')
   tmp => marbl_obj_ptr%StatusLog%FullLog
    do while (associated(tmp))
      !print *, trim(tmp%LogMessage)
      tmp => tmp%next
    end do

  !marbl_obj_ptr%domain%zw(:)      = -gcm_zw(nz-1:0:-1) ! bottom interface depth
  !marbl_obj_ptr%domain%zt(:)      = -gcm_zt(nz  :1:-1) ! centre depth
  !marbl_obj_ptr%domain%delta_z(:) = gcm_delta_z(nz  :1:-1) ! thickness


end subroutine init_marbl_instance

subroutine set_surface_flux_forcing_values(interop_obj, nz, nt, tracer_array, &
  salinity, temperature, uwind, vwind, pressure, pco2air, pco2air_alt, dust, &
  fe, nox, nhy) bind(C,name='set_surface_flux_forcing_values')

  implicit none

  type(marbl_interop_type), intent(inout) :: interop_obj
  real(c_double), intent(in) :: salinity, temperature, uwind, vwind, pressure, pco2air, pco2air_alt, dust, fe, nox, nhy
  integer(c_int), intent(in) :: nz, nt
  real(c_double), intent(in) :: tracer_array(nz, nt)
  
  type(marbl_interface_class), pointer :: marbl_instance
  integer :: m, idx

  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

  ! Assign the surface flux forcing values to the correct indices
  do idx = 1,size(marbl_instance%surface_flux_forcings)
    select case (trim(marbl_instance%surface_flux_forcings(idx)%metadata%varname))
    case('sss')
      marbl_instance%surface_flux_forcings(idx)%field_0d(1) = salinity
    case('sst')
      marbl_instance%surface_flux_forcings(idx)%field_0d(1) = temperature
    case('u10_sqr')
      marbl_instance%surface_flux_forcings(idx)%field_0d(1) = (uwind**2) + (vwind**2)
    case('Atmospheric Pressure')
      marbl_instance%surface_flux_forcings(idx)%field_0d(1) = pressure
    case('xco2')
      marbl_instance%surface_flux_forcings(idx)%field_0d(1) = pco2air
    case('xco2_alt_co2')
      marbl_instance%surface_flux_forcings(idx)%field_0d(1) = pco2air_alt
    case('Dust Flux')
      marbl_instance%surface_flux_forcings(idx)%field_0d(1) = dust
    case('Iron Flux')
      marbl_instance%surface_flux_forcings(idx)%field_0d(1) = fe
    case('NOx Flux')
      marbl_instance%surface_flux_forcings(idx)%field_0d(1) = nox
    case('NHy Flux')
      marbl_instance%surface_flux_forcings(idx)%field_0d(1) = nhy
    case('Ice Fraction')
      marbl_instance%surface_flux_forcings(idx)%field_0d(1) = 0
    case DEFAULT
      print *, "Not setting surface flux forcing with name: ", trim(marbl_instance%surface_flux_forcings(idx)%metadata%varname)
    end select
  end do

  ! tracer array
  do m=1,nt
    marbl_instance%tracers_at_surface(1,m) = tracer_array(1,m)
  end do

end subroutine set_surface_flux_forcing_values

subroutine compute_surface_fluxes(interop_obj) bind(C,name='compute_surface_fluxes')
  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  type(marbl_interface_class), pointer :: marbl_instance

  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

  ! call the marbl subroutine
  call marbl_instance%surface_flux_compute()

end subroutine compute_surface_fluxes

subroutine update_surface_fluxes(interop_obj, nz, nt, tracer_array, dt) bind(C,name='update_surface_fluxes')
  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  integer(c_int), intent(in) :: nz, nt
  real(c_double), intent(inout) :: tracer_array(nz, nt)
  real(c_double), intent(in) :: dt
  type(marbl_interface_class), pointer :: marbl_instance
  integer :: m

  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

  ! update the tracer array
  do m=1,nt
    tracer_array(nz,m) = tracer_array(nz,m) + marbl_instance%surface_fluxes(1,m) * dt
  end do

end subroutine update_surface_fluxes


subroutine set_interior_tendency_forcing_values(interop_obj, nz, nt, tracer_array, & 
  dust, srflx, Cp, rho0, temperature, salinity, z_r) bind(C,name='set_interior_tendency_forcing_values')

  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  integer(c_int), intent(in) :: nz, nt
  real(c_double), intent(in) :: tracer_array(nz, nt)
  real(c_double), intent(in) :: dust, srflx, Cp, rho0
  real(c_double), intent(in) :: temperature(nz), salinity(nz)
  real(c_double), intent(in) :: z_r(nz)
  integer :: m, idx

  type(marbl_interface_class), pointer :: marbl_instance

  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

  print *, 'setting' ,size(marbl_instance%interior_tendency_forcings), ' intetrior tendency forcing values'
  print *, 'potential temperature being assigned: ', temperature
  print *, 'salinity being assigned: ', salinity
  do idx = 1,size(marbl_instance%interior_tendency_forcings)
    print *, "Setting",   trim(MARBL_instance%interior_tendency_forcings(idx)%metadata%varname)
    ! set the interior tendency forcings at the right index
    select case (trim(MARBL_instance%interior_tendency_forcings(idx)%metadata%varname))
      case('Dust Flux')
        
        marbl_instance%interior_tendency_forcings(idx)%field_0d(1) = dust
      case('PAR Column Fraction')
        marbl_instance%interior_tendency_forcings(idx)%field_1d(1,1) = 1.0
        continue
      case('Surface Shortwave')
        
        marbl_instance%interior_tendency_forcings(idx)%field_1d(1,1) = srflx * Cp * rho0 * 202.22571403911931 
      case('Potential Temperature')
      print *, 'potential temperature being assigned: ', temperature
        marbl_instance%interior_tendency_forcings(idx)%field_1d(:,1) = temperature(:)
      case('Salinity')
        marbl_instance%interior_tendency_forcings(idx)%field_1d(:,1) = salinity(:) * 1000
      case('Pressure')
        marbl_instance%interior_tendency_forcings(idx)%field_1d(:,1) = z_r(:) * 0.1
      case('Iron Sediment Flux')
        marbl_instance%interior_tendency_forcings(idx)%field_1d(:,1)= 0.
      case('O2 Consumption Scale Factor')
        marbl_instance%interior_tendency_forcings(idx)%field_1d(:,:) = 1.0
        continue
      case('Particulate Remin Scale Factor')
        continue
      case DEFAULT
        print *, "Not setting interior forcing with name: ", trim(MARBL_instance%interior_tendency_forcings(idx)%metadata%varname)
        print *, "Idx: ", idx;
    end select
  end do

  ! column tracers
  do m=1,nt
    marbl_instance%tracers(m,:) = tracer_array(:,m)
  end do

  marbl_instance%bot_flux_to_tend(:) = 0.0
  marbl_instance%bot_flux_to_tend(nz)= 1./marbl_instance%domain%delta_z(nz) 
  marbl_instance%domain%kmt = 50
  marbl_instance%domain%num_elements_interior_tendency = 5
  
end subroutine set_interior_tendency_forcing_values

subroutine compute_interior_tendencies(interop_obj) bind(C,name='compute_interior_tendencies')
  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  type(marbl_interface_class), pointer :: marbl_instance

  real :: sum
  integer :: i,j
  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

  ! call the marbl subroutine
  !print *, 'calling interior_tendency_compute'
  !print *, 'initial NO3 concentrations: '
  !print *, marbl_instance%tracers(2,:)

  !print *, 'precompute tendencies: '
  !print *, marbl_instance%interior_tendencies(:,2)
  call marbl_instance%interior_tendency_compute()

  !print *, 'after compute concentrations:'
  !print *, marbl_instance%tracers(2,:)
  do i = 1, size(marbl_instance%interior_tendencies(:,1))
    sum = 0
    do j = 1, size(marbl_instance%interior_tendencies(i,:))
      sum = sum + marbl_instance%interior_tendencies(i,j)
    end do
    
  end do
end subroutine compute_interior_tendencies

subroutine update_interior_tendencies(interop_obj, nz, nt, tracer_array, dt) bind(C,name='update_interior_tendencies')
  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  integer(c_int), intent(in) :: nz, nt
  real(c_double), intent(inout) :: tracer_array(nz, nt)
  real(c_double), intent(in) :: dt
  type(marbl_interface_class), pointer :: marbl_instance
  integer :: m

  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)


  ! update the tracer array
  do m=1,nt
    tracer_array(:,m) = tracer_array(:,m) + marbl_instance%interior_tendencies(m,nz:1:-1) * dt
  end do
  !print *, 'NO3 interior tendencies: ', marbl_instance%interior_tendencies(2,:)
  !print *, 'index 8: ', marbl_instance%interior_tendencies(2,8)

end subroutine update_interior_tendencies
end module driver27