module driver19
use iso_c_binding

type marbl_instance
  integer :: nz, nt
  integer :: isalt, itemp
contains
  procedure :: init
  procedure :: write_values
end type marbl_instance

type, bind(C) :: marbl_interop_type
  integer(c_int) :: nz, nt
  integer(c_int) :: isalt, itemp
  type(c_ptr) :: tracer_array
  type(c_ptr) :: marbl_instance_obj
end type marbl_interop_type

contains

subroutine write_values(self)
  class(marbl_instance), intent(inout) :: self
  print *, "nz: ", self%nz
  print *, "nt: ", self%nt
  print *, "isalt: ", self%isalt
  print *, "itemp: ", self%itemp
end subroutine write_values


subroutine init(self, interop_type)
  class(marbl_instance), intent(inout) :: self
  type(marbl_interop_type), intent(in) :: interop_type
  real(c_double), pointer :: tracer_array(:,:)
  ! copy in the scalar values
  self%nz = interop_type%nz
  self%nt = interop_type%nt
  self%isalt = interop_type%isalt
  self%itemp = interop_type%itemp

end subroutine init


subroutine link_marbl_obj(interop_obj) bind(C,name='link_marbl_obj')
  type(marbl_interop_type), intent(inout) :: interop_obj

  type(marbl_instance), pointer :: marbl_obj_ptr

  allocate(marbl_obj_ptr)

  call marbl_obj_ptr%init(interop_obj)

  interop_obj%marbl_instance_obj = c_loc(marbl_obj_ptr)
end subroutine link_marbl_obj


subroutine use_marbl_obj(interop_obj) bind(C,name='use_marbl_obj')
  type(marbl_interop_type), intent(inout) :: interop_obj
  type(marbl_instance), pointer :: marbl_obj_ptr

  call c_f_pointer(interop_obj%marbl_instance_obj, marbl_obj_ptr)
  call marbl_obj_ptr%write_values()
  

end subroutine use_marbl_obj

end module driver19