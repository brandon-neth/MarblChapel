module driver18
use iso_c_binding

type marbl_instance
  integer :: nz, nt
  integer :: isalt, itemp
  real(c_double), allocatable :: tracer_array(:,:)
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
  real(c_double), pointer :: tracer_array(:,:)
  ! copy in the scalar values
  self%nz = interop_type%nz
  self%nt = interop_type%nt
  self%isalt = interop_type%isalt
  self%itemp = interop_type%itemp

  ! allocate the dynamically sized array
  allocate(self%tracer_array(self%nz, self%nt))
  
  ! associate `tracer_array` with the pointer passed from Chapel
  call c_f_pointer(interop_type%tracer_array, tracer_array, [interop_type%nz, interop_type%nt])

  ! copy the values from Chapel into the marbl object
  do i = 1, self%nz
    do j = 1, self%nt
      self%tracer_array(i,j) = tracer_array(i,j)
    end do
  end do
end subroutine init

subroutine compute(self)
  class(marbl_instance), intent(inout) :: self
  
  do i = 1, self%nz
    do j = 1, self%nt
      if (j == self%itemp) then
        self%tracer_array(i,j) = self%tracer_array(i,j) + 273.15;
      end if
      self%tracer_array(i,j) = self%tracer_array(i,j) + 1.0
    end do
  end do
end subroutine compute

subroutine compute_timestep(interop_obj) bind(C,name='compute_timestep')
  type(marbl_interop_type), intent(inout) :: interop_obj
  type(marbl_instance) :: marbl_obj
  real(c_double), pointer :: tracer_array(:,:)

  ! initialize the marbl object with the data passed from Chapel
  call marbl_obj%init(interop_obj)

  ! run some compute
  call marbl_obj%compute()
  
  ! associate `tracer_array` with the pointer passed from Chapel
  call c_f_pointer(interop_obj%tracer_array, tracer_array, [interop_obj%nz, interop_obj%nt])

  ! update the Chapel array with the new calculated values
  do i = 1, interop_obj%nz
    do j = 1, interop_obj%nt
      tracer_array(i,j) = marbl_obj%tracer_array(i,j)
    end do
  end do
end subroutine compute_timestep

end module driver18