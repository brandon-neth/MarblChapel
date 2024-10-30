program main
  use marbl_interface
  use marbl_kinds_mod, only : r8
  implicit none
  type(marbl_interface_class) :: marbl_driver
  real(r8) :: array(1)
  array(1) = 0.0
  call marbl_driver%init(1,1,1,array,array, array, '', .false.)

  PRINT *, 'called a function on a marbl_interface_class object'

end program main