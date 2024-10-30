module driver14
use iso_c_binding

type, bind(C) :: marbl_instance
  integer(c_int) :: nz, nt
  integer(c_int) :: isalt, itemp
  real(c_double) :: fe, nox, nhy
  type(c_ptr) :: arr
contains
  subroutine :: modify
end type marbl_instance

contains

subroutine modify(self) 
  class(marbl_instance), intent(in)
  self%isalt = 42
end subroutine

subroutine change_instance(marblDriver) bind(C,name="change_instance")
  type(marbl_instance), intent(in) :: marblDriver
  real(c_double), pointer :: f_arr(:)

  call c_f_pointer(marblDriver%arr, f_arr, [marblDriver%nz])

  f_arr(1) = 100.0
end subroutine change_instance

end module driver14