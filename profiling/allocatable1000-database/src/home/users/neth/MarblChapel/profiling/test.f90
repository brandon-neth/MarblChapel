
subroutine using_allocatable( num_levels, num_PAR_subcols, &
  num_elements_surface_flux, delta_z, zw, zt, active_level_count, num_wrappers) 
  use marbl_interface
  use iso_c_binding

  integer(c_int), intent(in) :: num_levels, num_PAR_subcols, num_elements_surface_flux
  real(c_double), intent(in) :: delta_z(num_levels), zw(num_levels), zt(num_levels)
  integer(c_int), intent(in) :: active_level_count
  integer(c_int), intent(in) :: num_wrappers

  integer :: i
  type(marbl_interface_class), allocatable :: wrappers(:)
  allocate(wrappers(num_wrappers))

  print *, 'using allocated array', num_wrappers
  do i =1,num_wrappers
    call wrappers(i)%init(gcm_num_levels=num_levels, &
        gcm_num_PAR_subcols=num_PAR_subcols, &
        gcm_num_elements_surface_flux=num_elements_surface_flux, &
        gcm_delta_z=delta_z, gcm_zw=zw, gcm_zt=zt, lgcm_has_global_ops=.true.)
  end do
end subroutine using_allocatable

subroutine using_pointer( num_levels, num_PAR_subcols, &
  num_elements_surface_flux, delta_z, zw, zt, active_level_count,num_wrappers) 
  use marbl_interface
  use iso_c_binding

  integer(c_int), intent(in) :: num_levels, num_PAR_subcols, num_elements_surface_flux
  real(c_double), intent(in) :: delta_z(num_levels), zw(num_levels), zt(num_levels)
  integer(c_int), intent(in) :: active_level_count
  integer, intent(in) :: num_wrappers



    integer :: i
  type(marbl_interface_class), pointer :: marbl_ptr
  type(c_ptr) :: marbl_c_ptr

  print *, "using pointers", num_wrappers
  do i = 1,num_wrappers
    allocate(marbl_ptr)
    marbl_c_ptr = c_loc(marbl_ptr)
    call using_pointer_impl(num_levels, num_PAR_subcols, &
    num_elements_surface_flux, delta_z, zw, zt, active_level_count, marbl_c_ptr)
  end do

end subroutine using_pointer

subroutine using_pointer_impl( num_levels, num_PAR_subcols, &
  num_elements_surface_flux, delta_z, zw, zt, active_level_count,ptr) 
  use marbl_interface
  use iso_c_binding

  integer(c_int), intent(in) :: num_levels, num_PAR_subcols, num_elements_surface_flux
  real(c_double), intent(in) :: delta_z(num_levels), zw(num_levels), zt(num_levels)
  integer(c_int), intent(in) :: active_level_count
  type(c_ptr), intent(in) :: ptr


  integer :: i
  type(marbl_interface_class), pointer :: wrapper
  
  call c_f_pointer(ptr, wrapper)

  call wrapper%init(gcm_num_levels=num_levels, &
      gcm_num_PAR_subcols=num_PAR_subcols, &
      gcm_num_elements_surface_flux=num_elements_surface_flux, &
      gcm_delta_z=delta_z, gcm_zw=zw, gcm_zt=zt, lgcm_has_global_ops=.true.)
end subroutine using_pointer_impl

program main
  use marbl_interface
  use iso_c_binding

  integer(c_int) :: num_levels = 60
  integer(c_int) :: num_PAR_subcols = 1
  integer(c_int) :: num_elements_surface_flux = 5
  integer(c_int) :: active_level_count = 50

  real(c_double), dimension(60) :: delta_z = [ &
10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.196, 10.564, 11.059, &
11.67, 12.424, 13.309, 14.35, 15.571, 16.996, 18.662, 20.609, 22.888, 25.562, 28.70, 32.408, &
36.777, 41.940, 48.042, 55.247, 63.731, 73.669, 85.208, 98.436, 113.324, 129.671, 147.053, &
164.80, 182.091, 198.022, 211.85, 223.165, 231.864, 238.194, 242.572, 245.467, 247.310, 248.443, &
249.119, 249.51, 249.735, 249.859, 249.926, 249.962, 249.981]

  
real(c_double), dimension(60) :: zw = [ &
    10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0, 110.0, 120.0, 130.0, 140.0, 150.0, 160.0, 170.18, 180.761, &
    191.821, 203.499, 215.923, 229.233, 243.584, 259.155, 276.152, 294.814, 315.423, 338.312, 363.874, &
    392.580, 424.98, 461.766, 503.706, 551.749, 606.996, 670.728, 744.39, 829.606, 928.043, 1041.368, &
    1171.040, 1318.093, 1482.900, 1664.992, 1863.014, 2074.873, 2298.039, 2529.904, 2768.098, 3010.670, &
    3256.13, 3503.448, 3751.891, 4001.011, 4250.524, 4500.260, 4750.120, 5000.046, 5250.00, 5499.990]

real(c_double), dimension(60) :: zt = [ &
    5.0, 15.0, 25.0, 35.0, 45.0, 55.0, 65.0, 75.0, 85.0, 95.0, 105.0, 115.0, 125.0, 135.0, 145.0, 155.0, 165.098, 175.47, &
    186.2912, 197.660, 209.7113, 222.578, 236.408, 251.3701, 267.654, 285.483, 305.1192, 326.8679, &
    351.0934, 378.227, 408.784, 443.3777, 482.736, 527.728, 579.372, 638.8626, 707.563, 787.002, &
    878.825, 984.705, 1106.204, 1244.566, 1400.497, 1573.94, 1764.003, 1968.944, 2186.456, 2413.9715, &
    2649.001, 2889.384, 3133.4045, 3379.7935, 3627.6702, 3876.451, 4125.768, 4375.392, 4625.1902, &
    4875.08, 5125.0280, 5374.999]

  integer :: num_wrappers = 1000
  character(len=64) :: arg, arg2

    call get_command_argument(1, arg)
    call get_command_argument(2, arg2)
    
    read(arg2, *) num_wrappers

    ! Trim and compare
    select case (trim(arg))
        case ("pointer")
            call using_pointer(num_levels, num_PAR_subcols, num_elements_surface_flux, &
              delta_z, zw, zt, active_level_count, num_wrappers)
        case ("allocatable")
            call using_allocatable(num_levels, num_PAR_subcols, num_elements_surface_flux, &
              delta_z, zw, zt, active_level_count, num_wrappers)
        case default
            print *, "Invalid argument. Use 'pointer' or 'allocatable'."
            print *, '"', trim(arg), '"'
            stop 1
    end select
  



  
  !

end program main