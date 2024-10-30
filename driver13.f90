module driver13

  use marbl_interface
  use marbl_kinds_mod, only : r8
  use iso_c_binding
  implicit none
  
contains 

  subroutine chpldrv_column_physics(marbl_instance, tracer_array, nz, nt, &
                                    ! Surface flux forcing variable indices
                                    sss_ind, sst_ind, ifrac_ind, dust_dep_ind, &
                                    fe_dep_ind, nox_flux_ind, nhy_flux_ind, &
                                    atmpress_ind, xco2_ind, xco2_alt_ind, &
                                    isalt, itemp, & 
                                    u10_sqr_ind, uwnd, vwnd, &
                                    pco2air, pco2air_alt, dust, iron, nox, nhy, &
                                    dt, &
                                    Hz, &
                                    z_w, z_r, &
                                    dustflux_ind, PAR_col_frac_ind, surf_shortwave_ind, &
                                    potemp_ind, salinity_ind, pressure_ind, fesedflux_ind, &
                                    srflx, Cp, rho0)
    !-----------------------------------------------------------------------
    !     SUBROUTINE: marbldrv_column_physics
          
    !     DESCRIPTION:
    !     Calculate surface fluxes, interior tracer tendencies,
    !     saved state variables, and diagnostics using MARBL,
    !     then update values in Chapel driver. 
    !      
    !     METHOD:
    !     1. Populate MARBL instance surface flux forcing values using forcing 
    !         variables from Chapel program
    !     2. Compute surface fluxes using MARBL
    !     4c. Update    tracer   array from Chapel program using newly computed values
    !     5. Populate MARBL instance interior tendency forcing values
    !        using ROMS ocean interior values
    !     7. Compute interior tendencies using MARBL
    !     8a. Apply newly computed increments to Chapel tracer array   
    !                 
    !     INPUT/OUTPUT:
    !     - tracer_array: the Chapel tracer array (t) to be updated by MARBL
    !     - nz: the depth of the column, and the length of the tracer arrays
    !     - nt: the number of tracers in the tracer array
    !     - sss_ind, sst_ind, ifrac_ind, dust_dep_ind, fe_dep_ind, nox_flux_ind, nhy_flux_ind, atmpress_ind, xco2_ind, xco2_alt_ind
    !     - isalt, itemp: 
    !     - u10_sqr_ind
    !     - uwnd, vwnd
    !     - pco2air, pco2air_alt, dust, iron, nox, nhy
    !     - dt
    !     - Hz
    !     - z_w, z_r
    !     - dustflux_ind, PAR_col_frac_ind, surf_shortwave_ind, potemp_ind, salinity_ind, pressure_ind, fesedflux_ind
    !     - srflx
    !     - Cp
    !     - rho0
    use iso_c_binding

    implicit none
    !     Arguments
    type(marbl_interface_class), intent(inout) :: marbl_instance
    integer(c_int), intent(in) :: nz, nt
    real(c_double), dimension(nz,nt), intent(inout) :: tracer_array
    integer(c_int), intent(in) :: sss_ind, sst_ind, ifrac_ind, dust_dep_ind, fe_dep_ind, nox_flux_ind, nhy_flux_ind, atmpress_ind, xco2_ind, xco2_alt_ind
    integer(c_int), intent(in) :: isalt, itemp
    integer(c_int), intent(in) :: u10_sqr_ind
    real(c_double), intent(in) :: uwnd, vwnd
    real(c_double), intent(in) :: pco2air, pco2air_alt, dust, iron, nox, nhy
    real(c_double), intent(in) :: dt
    real(c_double), dimension(nz), intent(in) :: Hz, z_w, z_r
    integer(c_int), intent(in) :: dustflux_ind, PAR_col_frac_ind, surf_shortwave_ind, potemp_ind, salinity_ind, pressure_ind, fesedflux_ind
    real(c_double), intent(in) :: srflx, Cp, rho0

    real(c_double), dimension(nz) :: dummy_array
    
    integer :: m, change_count, i,j
    real(c_double), dimension(nz,nt) :: tracer_array_at_start

    do i = 1,nz
      do j = 1,nt
        tracer_array_at_start(i,j) = tracer_array(i,j)
      end do
    end do
    dummy_array = 0.0
    ! 1. Populate MARBL instance surface flux forcing values
    
    print *, "populating surface flux forcing values..."

    ! sea surface salinity
    if (sss_ind > 0) then
      marbl_instance%surface_flux_forcings(sss_ind)%field_0d(1) = tracer_array(nz, isalt)
    end if

    print *, "populating sea surface temperature..."

    ! sea surface temperature
    if (sst_ind > 0) then
      marbl_instance%surface_flux_forcings(sst_ind)%field_0d(1) = tracer_array(nz, itemp)
    end if

    ! ice fraction (unused)
    if (ifrac_ind > 0) then
      marbl_instance%surface_flux_forcings(ifrac_ind)%field_0d(1) = 0
    end if
    
    ! squared 10m windspeed
    if (u10_sqr_ind > 0) then
      marbl_instance%surface_flux_forcings(u10_sqr_ind)%field_0d(1) = (uwnd**2) + (vwnd**2)
    end if

    print *, "populating atmospheric pressure..."
    ! atmospheric pressure (constant)
    if (atmpress_ind > 0) then
      marbl_instance%surface_flux_forcings(atmpress_ind)%field_0d(1) = 1.
    end if

    ! pco2
    if (xco2_ind > 0) then
      marbl_instance%surface_flux_forcings(xco2_ind)%field_0d(1) = pco2air
    end if

    ! pco2_alt
    if (xco2_alt_ind > 0) then
      marbl_instance%surface_flux_forcings(xco2_alt_ind)%field_0d(1) = pco2air_alt
    end if

    ! dust
    if (dust_dep_ind > 0) then
      marbl_instance%surface_flux_forcings(dust_dep_ind)%field_0d(1) = dust
    end if

    ! iron deposition
    if (fe_dep_ind > 0) then
      marbl_instance%surface_flux_forcings(fe_dep_ind)%field_0d(1) = iron * 0.01
    end if
    print *, "populating nox..."
    ! nox
    if (nox_flux_ind > 0) then
      marbl_instance%surface_flux_forcings(nox_flux_ind)%field_0d(1) = nox * 71394.200220751
    end if

    ! nhy
    if (nhy_flux_ind > 0) then
      marbl_instance%surface_flux_forcings(nhy_flux_ind)%field_0d(1) = nhy * 71394.200220751
    end if

    print *, " populating surface tracer values"
    ! surface tracer values
    do m=1,nt
      marbl_instance%tracers_at_surface(1,m) = tracer_array(nz,m)
    end do

    !     3. Compute surface fluxes using MARBL:
    !     ----------------------------------------------------------------------
              
    print *, 'calling surface_flux_compute...'
    call marbl_instance%surface_flux_compute()


    !     4c. Update tracer array using newly computed values:
    !     ----------------------------------------------------------------------           
    print *, "Updating tracer array with surface fluxes..."

    do m = 1,nt
      if (marbl_instance%surface_fluxes(1,m) .ne. 0) then
        tracer_array(nz, m) = tracer_array(nz,m) + marbl_instance%surface_fluxes(1,m) * dt / Hz(nz)
      end if
    end do

    change_count = 0
    do i = 1,nz
      do j = 1,nt
        if (tracer_array(i,j) /= tracer_array_at_start(i,j)) then
          change_count = change_count + 1
        end if
      end do
    end do
    print *, "surface flux update change count: ", change_count

    !     5.  Populate MARBL instance interior tendency forcing values
    !     -----------------------------------------------------------------------

    print *, "updating domain..."

    ! First, update domain ni MARBL instance to local geometry

    marbl_instance%domain%zw(:) = -z_w(nz-1:0:-1) ! bottom interface depth
    marbl_instance%domain%zt(:) = -z_r(nz:1:-1) ! centre depth
    marbl_instance%domain%delta_z = Hz(nz:1:-1) ! thickness
    marbl_instance%domain%kmt = nz ! number of active levels

    print *, "populating interior tendency forcing values..."
    ! dust flux
    if (dustflux_ind > 0) then
      marbl_instance%interior_tendency_forcings(dustflux_ind)%field_0d(1) = dust
    end if
  
    ! PAR subcolumns (unused as the dimension of this field is the number of ice categories and we have none)
    if (PAR_col_frac_ind > 0) then
      continue
    end if

    ! surface shortwave
    if (surf_shortwave_ind > 0) then
      marbl_instance%interior_tendency_forcings(surf_shortwave_ind)%field_1d(1,1) = srflx * Cp * rho0
    end if

    ! potential temperature
    if (potemp_ind > 0) then
      marbl_instance%interior_tendency_forcings(potemp_ind)%field_1d(1,:) = tracer_array(nz:1:-1, itemp)
    end if
      
    ! salinity
    if (salinity_ind > 0) then
      marbl_instance%interior_tendency_forcings(salinity_ind)%field_1d(1,:) = tracer_array(nz:1:-1, isalt)
    end if

    ! pressure (calculated from depth)
    if (pressure_ind > 0) then
      marbl_instance%interior_tendency_forcings(pressure_ind)%field_1d(1,:) = -z_r(nz:1:-1) * 0.1
    end if

    ! iron flux
    if (fesedflux_ind > 0) then
      marbl_instance%interior_tendency_forcings(fesedflux_ind)%field_1d(1,:)= 0.
    end if

    ! column tracers
    do m=1,nt
      marbl_instance%tracers(m,:) = tracer_array(nz:1:-1,m)
    end do

    !   7. Compute interior tendencies using MARBL
    !   -------------------------------------------

    print *, "Computing interior tendencies..."
    call marbl_instance%interior_tendency_compute()

    !   8a. Apply calculated increments to tracer array
    !   --------------------------------------------------

    print *, "Applying interior tendency calculations to tracer array..."

    do m = 1,nt
      tracer_array(:,m) = tracer_array(:,m) + marbl_instance%interior_tendencies(m,nz:1:-1)*dt
    end do

    change_count = 0
    do i = 1,nz
      do j = 1,nt
        if (tracer_array(i,j) /= tracer_array_at_start(i,j)) then
          change_count = change_count + 1
        end if
      end do
    end do
    print *, "interior tendency update change count: ", change_count
  end subroutine chpldrv_column_physics


end module driver13

program main
  use marbl_interface
  use marbl_kinds_mod, only : r8
  use iso_c_binding
  use driver13
  implicit none
  type(MARBL_interface_class) :: marbl_instance
  integer :: marbl_settings_in, open_status, read_status
  character(len=256) :: namelist_line
  character(len=256) :: marbl_namelist_fname='marbl_in'
  character(len=256) ::marbl_tracer_list_fname='marbl_tracer_output_list'
  integer :: nz
  real(r8), dimension(100)                    :: dummy_array
  integer(c_int) :: nt

  real(c_double), dimension(100,30)  :: tracer_array
  integer(c_int) :: sss_ind, sst_ind, ifrac_ind, dust_dep_ind, fe_dep_ind, nox_flux_ind, nhy_flux_ind, atmpress_ind, xco2_ind, xco2_alt_ind
  integer(c_int) :: isalt, itemp
  integer(c_int) :: u10_sqr_ind
  real(c_double) :: uwnd, vwnd
  real(c_double) :: pco2air, pco2air_alt, dust, iron, nox, nhy
  real(c_double) :: dt
  real(c_double), dimension(100) :: Hz
  real(c_double), dimension(100) :: z_w, z_r
  integer(c_int)  :: dustflux_ind, PAR_col_frac_ind, surf_shortwave_ind, potemp_ind, salinity_ind, pressure_ind, fesedflux_ind
  real(c_double) :: srflx, Cp, rho0
  real(c_double) :: r
  integer(c_int) :: i,j
  
  nz = 100
  nt = 30
  sss_ind = 1
  sst_ind = 2
  ifrac_ind = 3
  dust_dep_ind = 4
  fe_dep_ind = 5
  nox_flux_ind = 6
  nhy_flux_ind = 7
  atmpress_ind = 8
  xco2_ind = 9
  xco2_alt_ind = 10
  isalt = 1
  itemp = 2
  u10_sqr_ind = 11
  uwnd = 9.8
  vwnd = 9.8
  pco2air = 100
  pco2air_alt = 90
  dust = 1.8
  iron = 8.2
  nox = 3.3
  nhy = 4.4
  dt = 1.0

  Hz = 1.0
  do i = 1,100
    call random_number(r)
    z_w(i) = r
    call random_number(r)
    z_r(i) = r
    call random_number(r)
    Hz(i) = r
  end do
  
  do i = 1,nz
    do j = 1,nt
      call random_number(r)
      tracer_array(i,j) = r
    end do
  end do

  dustflux_ind = 1
  PAR_col_frac_ind = 2
  surf_shortwave_ind = 3
  potemp_ind = 4
  salinity_ind = 5
  pressure_ind = 6
  fesedflux_ind = 7

  srflx = 2.57
  Cp = 3.3
  rho0 = 1.0
  
  dummy_array(:)=1.0

  open(unit=marbl_settings_in, file=marbl_namelist_fname, iostat=open_status)
  read(marbl_settings_in,"(A)",iostat=read_status) namelist_line
  call marbl_instance%put_setting(namelist_line)

  call marbl_instance%init(gcm_num_levels=nz, gcm_num_PAR_subcols = 1, gcm_num_elements_surface_flux = 1, gcm_delta_z = dummy_array(:), gcm_zw = dummy_array(:),  gcm_zt = dummy_array(:),   lgcm_has_global_ops = .true., unit_system_opt='mks')

  do i = 1,size(marbl_instance%tracer_metadata) 
    print *, marbl_instance%tracer_metadata(i)%short_name
    print *, marbl_instance%tracer_metadata(i)%long_name
    print *, marbl_instance%tracer_metadata(i)%units
  end do
  print *, size(marbl_instance%tracer_metadata)
  call chpldrv_column_physics(marbl_instance, tracer_array, nz, nt, sss_ind, sst_ind, &
    ifrac_ind, dust_dep_ind, fe_dep_ind, nox_flux_ind, nhy_flux_ind,  &
    atmpress_ind, xco2_ind, xco2_alt_ind, isalt, itemp,  u10_sqr_ind, &
    uwnd, vwnd, pco2air, pco2air_alt, dust, iron, nox, nhy, dt, Hz,   &
    z_w, z_r, dustflux_ind, PAR_col_frac_ind, surf_shortwave_ind,     &
    potemp_ind, salinity_ind, pressure_ind, fesedflux_ind, srflx, Cp, &
    rho0)

  
end program main
