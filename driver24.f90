module driver24
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
  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  integer(c_int), intent(in)    :: gcm_num_levels
  integer(c_int), intent(in)    :: gcm_num_PAR_subcols
  integer(c_int), intent(in)    :: gcm_num_elements_surface_flux
  real(c_double), intent(in)    :: gcm_delta_z(gcm_num_levels)
  real(c_double), intent(in)    :: gcm_zw(gcm_num_levels)
  real(c_double), intent(in)    :: gcm_zt(gcm_num_levels)

  type(marbl_interface_class), pointer :: marbl_obj_ptr
  type(marbl_interface_class) :: marbl_obj

  real(c_double), allocatable :: dummy_array(:)

  integer :: i, nz

  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_obj_ptr)

  allocate(dummy_array(gcm_num_levels))

  do i = 1, gcm_num_levels
    dummy_array(i) = 0.0
  end do
  nz = gcm_num_levels

  print *, "Associated all pointers, calling init..."
  call marbl_obj_ptr%init(gcm_num_levels=nz, gcm_num_PAR_subcols = 1, gcm_num_elements_surface_flux = 1, gcm_delta_z = gcm_delta_z(:), gcm_zw = dummy_array(:),      gcm_zt = dummy_array(:),      lgcm_has_global_ops = .true., unit_system_opt='mks')
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
    marbl_instance%tracers_at_surface(1,m) = tracer_array(nz,m)
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


  ! set the interior tendency forcings at the right index
  select case (trim(MARBL_instance%interior_tendency_forcings(idx)%metadata%varname))
    case('Dust Flux')
      marbl_instance%interior_tendency_forcings(idx)%field_0d(1) = dust
    case('PAR Column Fraction')
      continue
    case('Surface Shortwave')
      marbl_instance%interior_tendency_forcings(idx)%field_1d(1,1) = srflx * Cp * rho0
    case('Potential Temperature')
      marbl_instance%interior_tendency_forcings(idx)%field_1d(1,:) = temperature(nz:1:-1)
    case('Salinity')
      marbl_instance%interior_tendency_forcings(idx)%field_1d(1,:) = salinity(nz:1:-1)
    case('Pressure')
      marbl_instance%interior_tendency_forcings(idx)%field_1d(1,:) = -z_r(nz:1:-1) * 0.1
    case('Iron Sediment Flux')
      marbl_instance%interior_tendency_forcings(idx)%field_1d(1,:)= 0.
    case('O2 Consumption Scale Factor')
      continue
    case('Particulate Remin Scale Factor')
      continue
    case DEFAULT
      print *, "Not setting interior forcing with name: ", trim(MARBL_instance%interior_tendency_forcings(idx)%metadata%varname)
  end select

  ! column tracers
  do m=1,nt
    marbl_instance%tracers(m,:) = tracer_array(nz:1:-1,m)
  end do
end subroutine set_interior_tendency_forcing_values

subroutine compute_interior_tendencies(interop_obj) bind(C,name='compute_interior_tendencies')
  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  type(marbl_interface_class), pointer :: marbl_instance

  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

  ! call the marbl subroutine
  call marbl_instance%interior_tendency_compute()

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

end subroutine update_interior_tendencies
end module driver24