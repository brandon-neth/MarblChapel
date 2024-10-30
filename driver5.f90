module TestProcs
  use iso_c_binding
  implicit none

  contains

subroutine allocateSomething(size) bind(C,name='allocateSomething')
  use iso_c_binding, only: c_int, c_double
  implicit none
  integer(c_int), intent(in) :: size
  real, allocatable :: arr(:)

  allocate(arr(size))
  PRINT *, "Allocated!"
  
end subroutine allocateSomething

end module TestProcs