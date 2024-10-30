module driver17
use iso_c_binding

type marbl_instance
  integer :: nz, nt
  real, allocatable :: tracer_array(:,:)
  integer :: isalt, itemp
contains
  procedure :: init
  procedure :: compute
end type marbl_instance

type, bind(C) :: marbl_interop_type
  integer(c_int) :: nz, nt
  integer(c_int) :: isalt, itemp
  type(c_ptr) :: tracer_array
end type marbl_interop_type

contains

subroutine init(self, interop_type)
  class(marbl_instance), intent(inout) :: self
  type(marbl_interop_type), intent(in) :: interop_type
  
  self%nz = interop_type%nz
  self%nt = interop_type%nt
  self%isalt = interop_type%isalt
  self%itemp = interop_type%itemp
  allocate(self%tracer_array(self%nz, self%nt))
  
  print *, "itemp:", self%itemp
  print *, "isalt:", self%isalt
  self%tracer_array = 0.0

  self%tracer_array(:,self%itemp) = 273.15

  self%tracer_array(:,self%isalt) = 35.0
  print *, "tracer array:", self%tracer_array
end subroutine init

subroutine compute(self)
  class(marbl_instance), intent(inout) :: self
  
  do i = 1, self%nz
    do j = 1, self%nt
      self%tracer_array(i,j) = self%tracer_array(i,j) + 1.0
    end do
  end do
end subroutine compute

subroutine compute_timestep(interop_obj) bind(C,name='compute_timestep')
  type(marbl_interop_type), intent(inout) :: interop_obj
  type(marbl_instance) :: marbl_obj
  real(c_double), pointer :: tracer_array(:,:)
  call marbl_obj%init(interop_obj)
  call marbl_obj%compute()
  
  call c_f_pointer(interop_obj%tracer_array, tracer_array, [interop_obj%nz, interop_obj%nt])
  do i = 1, interop_obj%nz
    do j = 1, interop_obj%nt
      print *, marbl_obj%tracer_array(i,j)
      tracer_array(i,j) = marbl_obj%tracer_array(i,j)
    end do
  end do
end subroutine compute_timestep

end module driver17