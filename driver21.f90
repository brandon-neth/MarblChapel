module driver21
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

subroutine read_marbl_settings_file(interop_obj, file_path) bind(C,name='read_marbl_settings_file')
  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  character(kind=c_char), intent(in) :: file_path
  
  ! local variables
  type(marbl_interface_class), pointer :: marbl_instance
  integer :: marbl_settings_in, open_status, read_status
  character(len=256) :: namelist_line

  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

  ! open the settings file
  open(unit=marbl_settings_in, file=file_path, iostat=open_status)

  ! read the settings file
  read(marbl_settings_in,"(A)",iostat=read_status) namelist_line

  ! put the settings into the marbl object
  call marbl_instance%put_setting(namelist_line)
end subroutine read_marbl_settings_file

subroutine set_surface_flux_forcing_values(interop_obj, nz, nt, tracer_array, sss_ind, sss, sst_ind, sst, &
  u10_sqr_ind, uwnd, vwnd,  atmpress_ind, atmpress, xcod2_ind, pco2air,  xco2_alt_ind, pco2air_alt, &
  dust_dep_ind, dust, fe_dep_ind, fe, nox_flux_ind, nox, nhy_flux_ind, nhy) bind(C,name='set_surface_flux_forcing_values')

  implicit none

  type(marbl_interop_type), intent(inout) :: interop_obj
  integer(c_int), intent(in) :: sss_ind, sst_ind, u10_sqr_ind, atmpress_ind, xcod2_ind, xco2_alt_ind, dust_dep_ind, fe_dep_ind, nox_flux_ind, nhy_flux_ind
  real(c_double), intent(in) :: sss, sst, uwnd, vwnd, atmpress, pco2air, pco2air_alt, dust, fe, nox, nhy
  integer(c_int), intent(in) :: nz, nt
  real(c_double), intent(in) :: tracer_array(nz, nt)
  
  type(marbl_interface_class), pointer :: marbl_instance
  integer :: m

  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

  ! sea surface salinity
  if (sss_ind > 0) then
    marbl_instance%surface_flux_forcings(sss_ind)%field_0d(1) = sss
  end if

  ! sea surface temperature
  if (sst_ind > 0) then
    marbl_instance%surface_flux_forcings(sst_ind)%field_0d(1) = sst
  end if

  ! wind speed
  if (u10_sqr_ind > 0) then
    marbl_instance%surface_flux_forcings(u10_sqr_ind)%field_0d(1) = (uwnd**2) + (vwnd**2)
  end if

  ! atmospheric pressure
  if (atmpress_ind > 0) then
    marbl_instance%surface_flux_forcings(atmpress_ind)%field_0d(1) = atmpress
  end if

  ! atmospheric CO2
  if (xcod2_ind > 0) then
    marbl_instance%surface_flux_forcings(xcod2_ind)%field_0d(1) = pco2air
  end if

  ! atmospheric CO2 alternative
  if (xco2_alt_ind > 0) then
    marbl_instance%surface_flux_forcings(xco2_alt_ind)%field_0d(1) = pco2air_alt
  end if

  ! dust
  if (dust_dep_ind > 0) then
    marbl_instance%surface_flux_forcings(dust_dep_ind)%field_0d(1) = dust
  end if

  ! iron
  if (fe_dep_ind > 0) then
    marbl_instance%surface_flux_forcings(fe_dep_ind)%field_0d(1) = fe
  end if

  ! nox
  if (nox_flux_ind > 0) then
    marbl_instance%surface_flux_forcings(nox_flux_ind)%field_0d(1) = nox
  end if

  ! nhy
  if (nhy_flux_ind > 0) then
    marbl_instance%surface_flux_forcings(nhy_flux_ind)%field_0d(1) = nhy
  end if

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


subroutine set_interior_tendency_forcing_values(interop_obj, nz, nt, tracer_array, z_w, z_r, Hz, & 
  dustflux_ind, dust, surf_shortwave_ind, srflx, Cp, rho0, &
  potemp_ind, itemp, salinity_ind, isalt, pressure_ind, fesedflux_ind) bind(C,name='set_interior_tendency_forcing_values')

  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  integer(c_int), intent(in) :: nz, nt
  real(c_double), intent(in) :: tracer_array(nz, nt)
  real(c_double), dimension(nz), intent(in) :: Hz, z_w, z_r
  integer(c_int), intent(in) :: dustflux_ind, surf_shortwave_ind, potemp_ind, salinity_ind, pressure_ind, fesedflux_ind
  real(c_double), intent(in) :: dust, srflx, Cp, rho0
  integer(c_int), intent(in) :: itemp, isalt

  integer :: m

  type(marbl_interface_class), pointer :: marbl_instance

  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

  print *, "updating domain..."
  ! update domain
  marbl_instance%domain%zw(:) = -z_w(nz-1:0:-1) ! bottom interface depth
  marbl_instance%domain%zt(:) = -z_r(nz:1:-1) ! centre depth
  marbl_instance%domain%delta_z = Hz(nz:1:-1) ! thickness
  marbl_instance%domain%kmt = nz ! number of active levels

  print *, "setting flux values..."
  ! dust flux
  if (dustflux_ind > 0) then
    marbl_instance%interior_tendency_forcings(dustflux_ind)%field_0d(1) = dust
  end if

  ! surface shortwave
  if (surf_shortwave_ind > 0) then
    marbl_instance%interior_tendency_forcings(surf_shortwave_ind)%field_1d(1,1) = srflx * Cp * rho0
  end if

  ! potential temperature
  if (potemp_ind > 0) then
    marbl_instance%interior_tendency_forcings(potemp_ind)%field_1d(1,:) = tracer_array(nz:1:-1,itemp)
  end if

  print *, "setting salinity..."
  ! salinity
  if (salinity_ind > 0) then
    marbl_instance%interior_tendency_forcings(salinity_ind)%field_1d(1,:) = tracer_array(nz:1:-1,isalt)
  end if

  ! pressure
  if (pressure_ind > 0) then
    marbl_instance%interior_tendency_forcings(pressure_ind)%field_1d(1,:) = -z_r(nz:1:-1) * 0.1
  end if

  print *, "iron flux..."
  print *, "interior tendency forcing size: ", size(marbl_instance%interior_tendency_forcings)
  ! iron flux
  if (fesedflux_ind > 0) then
    marbl_instance%interior_tendency_forcings(fesedflux_ind)%field_1d(1,:)= 0.
  end if

  print *, "setting column tracers..."
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

end module driver21