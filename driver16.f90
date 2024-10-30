module driver16
use iso_c_binding

type, bind(C) :: marbl_interop_type
  type(c_ptr) :: tracer_array
  integer(c_int) :: nz, nt
  integer(c_int) :: sss_ind, sst_ind, ifrac_ind, dust_dep_ind, fe_dep_ind, nox_flux_ind, nhy_flux_ind, atmpress_ind, xco2_ind, xco2_alt_ind
  integer(c_int) :: isalt, itemp
  integer(c_int) :: u10_sqr_ind
  real(c_double) :: uwnd, vwnd, pco2air, pco2air_alt, dust, iron, nox, nhy
  real(c_double) :: dt
  type(c_ptr) :: Hz, z_w, z_r
  integer(c_int) :: dustflux_ind, PAR_col_frac_ind, surf_shortwave_ind, potemp_ind, salinity_ind, pressure_ind, fesedflux_ind
  real(c_double) :: srflx, Cp, rho0
end type marbl_interop_type

contains

subroutine display(marbl_obj) 
  type(marbl_interop_type), intent(in) :: marbl_obj
  real(c_double), pointer :: h(:), w(:), r(:)
  print *, "nz, nt", marbl_obj%nz, marbl_obj%nt
  print *, "sss_ind, sst_ind, ifrac_ind, dust_dep_ind, fe_dep_ind, nox_flux_ind, nhy_flux_ind, atmpress_ind, xco2_ind, xco2_alt_ind", marbl_obj%sss_ind, marbl_obj%sst_ind, marbl_obj%ifrac_ind, marbl_obj%dust_dep_ind, marbl_obj%fe_dep_ind, marbl_obj%nox_flux_ind, marbl_obj%nhy_flux_ind, marbl_obj%atmpress_ind, marbl_obj%xco2_ind, marbl_obj%xco2_alt_ind
  print *, "isalt, itemp", marbl_obj%isalt, marbl_obj%itemp
  print *, "uwnd, vwnd, pco2air, pco2air_alt, dust, iron, nox, nhy", marbl_obj%uwnd, marbl_obj%vwnd, marbl_obj%pco2air, marbl_obj%pco2air_alt, marbl_obj%dust, marbl_obj%iron, marbl_obj%nox, marbl_obj%nhy
  print *, "dt", marbl_obj%dt
  call c_f_pointer(marbl_obj%Hz, h, [marbl_obj%nz])
  call c_f_pointer(marbl_obj%z_w, w, [marbl_obj%nz])
  call c_f_pointer(marbl_obj%z_r, r, [marbl_obj%nz])

  print *, "Hz(1), z_w(1), z_r(1)", h(1), w(1), r(1)
  print *, "dustflux_ind, PAR_col_frac_ind, surf_shortwave_ind, potemp_ind, salinity_ind, pressure_ind, fesedflux_ind", marbl_obj%dustflux_ind, marbl_obj%PAR_col_frac_ind, marbl_obj%surf_shortwave_ind, marbl_obj%potemp_ind, marbl_obj%salinity_ind, marbl_obj%pressure_ind, marbl_obj%fesedflux_ind
  print *, "srflx, Cp, rho0", marbl_obj%srflx, marbl_obj%Cp, marbl_obj%rho0
end subroutine display

subroutine pass_and_use(marbl_object) bind(C,name='pass_and_use')
  type(marbl_interop_type), intent(inout) :: marbl_object
  call display(marbl_object)
end subroutine pass_and_use

end  module driver16