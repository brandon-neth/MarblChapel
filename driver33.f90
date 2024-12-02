module driver32
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

subroutine init_marbl_instance(interop_obj, gcm_num_levels, gcm_num_PAR_subcols, gcm_num_elements_surface_flux, gcm_delta_z, gcm_zw, gcm_zt, active_level_cnt) bind(C,name='init_marbl_instance')
  use marbl_logging, only : marbl_status_log_entry_type
  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  integer(c_int), intent(in)    :: gcm_num_levels
  integer(c_int), intent(in)    :: gcm_num_PAR_subcols
  integer(c_int), intent(in)    :: gcm_num_elements_surface_flux
  real(c_double), intent(in)    :: gcm_delta_z(gcm_num_levels)
  real(c_double), intent(in)    :: gcm_zw(gcm_num_levels)
  real(c_double), intent(in)    :: gcm_zt(gcm_num_levels)
  integer(c_int), intent(in)    :: active_level_cnt
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
  marbl_obj_ptr%domain%kmt = active_level_cnt

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
      marbl_instance%surface_flux_forcings(idx)%field_0d(1) = salinity * 1.0e3_r8
    case('sst')
      marbl_instance%surface_flux_forcings(idx)%field_0d(1) = temperature
    case('u10_sqr')
      marbl_instance%surface_flux_forcings(idx)%field_0d(1) = uwind
    case('Atmospheric Pressure')
      marbl_instance%surface_flux_forcings(idx)%field_0d(1) =  0.99799725827383001_r8
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
  marbl_instance%tracers_at_surface(:,:) = 0
  do m=1,nt
    marbl_instance%tracers_at_surface(1,m) = tracer_array(1,m)
  end do

  marbl_instance%bot_flux_to_tend(:) = 0.0    
  marbl_instance%bot_flux_to_tend(marbl_instance%domain%kmt)= 1._r8/marbl_instance%domain%delta_z(marbl_instance%domain%kmt) 

  marbl_instance%domain%num_elements_surface_flux = 5
end subroutine set_surface_flux_forcing_values

subroutine compute_surface_fluxes(interop_obj) bind(C,name='compute_surface_fluxes')
  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  type(marbl_interface_class), pointer :: marbl_instance

  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

  call marbl_instance%set_global_scalars('surface_flux')
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

subroutine set_interior_tendency_forcing_scalar(interop_obj, variableName, vnLen, data) bind(C,name='set_interior_tendency_forcing_scalar')
  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  real(c_double), intent(in) :: data
  integer(c_int), intent(in) :: vnLen
  character(kind=c_char), dimension(vnLen), intent(in) :: variableName
  character(kind=c_char,len=vnLen) :: vName
  type(marbl_interface_class), pointer :: marbl_instance
  integer :: idx
  logical :: found

  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

  vName = transfer(variableName, vName)
  found = .false.
  do idx = 1,size(marbl_instance%interior_tendency_forcings)
    if (trim(MARBL_instance%interior_tendency_forcings(idx)%metadata%varname) == vName) then
      marbl_instance%interior_tendency_forcings(idx)%field_0d(1) = data
      found = .true.
    end if
  end do

  if (.not. found)then
    print *, "Failed to set forcing scalar. Could not find variable with name ", vName
  end if
end subroutine set_interior_tendency_forcing_scalar

subroutine set_interior_tendency_forcing_array(interop_obj, nz, variableName, vnLen, data) bind(C,name='set_interior_tendency_forcing_array')
  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  integer(c_int), intent(in) :: nz
  real(c_double), intent(in) :: data(nz)
  integer(c_int), intent(in) :: vnLen
  character(kind=c_char), dimension(vnLen), intent(in) :: variableName
  character(kind=c_char,len=vnLen) :: vName
  type(marbl_interface_class), pointer :: marbl_instance
  integer :: idx
  logical :: found

  
  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)
  vName = transfer(variableName, vName)
  found = .false.
  do idx = 1,size(marbl_instance%interior_tendency_forcings)
    if (trim(MARBL_instance%interior_tendency_forcings(idx)%metadata%varname) == vName) then
    marbl_instance%interior_tendency_forcings(idx)%field_1d(:,1) = 0.0_r8
      marbl_instance%interior_tendency_forcings(idx)%field_1d(1:nz,1) = data(1:nz)
      if (vName == "Iron Sediment Flux") then
        marbl_instance%interior_tendency_forcings(idx)%field_1d(1:nz,1) = data(1:nz) * 0.01_r8
      end if
      if (vName == "Salinity") then
        marbl_instance%interior_tendency_forcings(idx)%field_1d(1:nz,1) = data(1:nz) * 1.0e3_r8
      end if
      found = .true.
    end if
  end do

  if (.not. found)then
    print *, "Failed to set forcing array. Could not find variable with name ", vName
  end if
end subroutine set_interior_tendency_forcing_array


subroutine set_interior_tendency_forcing_values(interop_obj, nz, nt, tracer_array, & 
  dust, srflx, Cp, rho0, temperature, salinity, z_r) bind(C,name='set_interior_tendency_forcing_values')

  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  integer(c_int), intent(in) :: nz, nt
  real(c_double), intent(in) :: tracer_array(nz, nt)
  real(c_double), intent(in) :: dust, srflx, Cp, rho0
  real(c_double), intent(in) :: temperature(nz), salinity(nz)
  real(c_double), intent(in) :: z_r(nz)
  integer :: m, idx, j

  integer :: kmt
  type(marbl_interface_class), pointer :: marbl_instance

  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

  kmt = marbl_instance%domain%kmt

  do idx = 1,size(marbl_instance%interior_tendency_forcings)
    select case (trim(MARBL_instance%interior_tendency_forcings(idx)%metadata%varname))
      case('Dust Flux')
        
        marbl_instance%interior_tendency_forcings(idx)%field_0d(1) = dust
      case('PAR Column Fraction')
        marbl_instance%interior_tendency_forcings(idx)%field_1d(1,1) = 1.0
        continue
      case('Surface Shortwave')
        
        marbl_instance%interior_tendency_forcings(idx)%field_1d(1,1) = srflx * Cp * rho0
        marbl_instance%interior_tendency_forcings(idx)%field_1d(1,1) = 202.22571403911931_r8
      case('Potential Temperature')
        marbl_instance%interior_tendency_forcings(idx)%field_1d(1:kmt,1) = temperature(1:kmt)
      case('Salinity')
        marbl_instance%interior_tendency_forcings(idx)%field_1d(1:kmt,1) = salinity(1:kmt) * 1.0e3_r8
      case('Pressure')
        marbl_instance%interior_tendency_forcings(idx)%field_1d(1:kmt,1) = z_r(1:kmt) * 0.1
      case('Iron Sediment Flux')
        marbl_instance%interior_tendency_forcings(idx)%field_1d(:,1)= 0.
      case('O2 Consumption Scale Factor')
        marbl_instance%interior_tendency_forcings(idx)%field_1d(1:kmt,1) = 1.0
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
  marbl_instance%bot_flux_to_tend(kmt)= 1._r8/marbl_instance%domain%delta_z(kmt) 
  marbl_instance%domain%kmt = kmt
  marbl_instance%domain%num_elements_interior_tendency = 1
  
end subroutine set_interior_tendency_forcing_values


subroutine copy_tracer_values(interop_obj, nz, nt, tracer_array) bind(C, name='copy_tracer_values')
  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  integer(c_int), intent(in) :: nz, nt
  real(c_double), intent(in) :: tracer_array(nz, nt)
  integer :: m

  type(marbl_interface_class), pointer :: marbl_instance

  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)

  do m=1,nt
    marbl_instance%tracers(m,:) = tracer_array(:,m)
  end do
end subroutine copy_tracer_values

subroutine compute_interior_tendencies(interop_obj) bind(C,name='compute_interior_tendencies')
  implicit none
  type(marbl_interop_type), intent(inout) :: interop_obj
  type(marbl_interface_class), pointer :: marbl_instance

  real :: sum
  integer :: i,j
  ! Get the pointer to the marbl object itself
  call c_f_pointer(interop_obj%marbl_obj, marbl_instance)
  
  ! call the marbl subroutine
  

  call marbl_instance%set_global_scalars('interior_tendency')
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
    tracer_array(:,m) = tracer_array(:,m) + marbl_instance%interior_tendencies(m,:) * dt
  end do
  

end subroutine update_interior_tendencies
end module driver32