module TestProcs
  use iso_c_binding
  implicit none

  contains

! initializes the given 1D array of length size with val at each index
subroutine initArray(arr, size, val) bind(C,name='initArray')
  use iso_c_binding, only: c_int, c_double
  implicit none
  integer(c_int), intent(in) :: size
  real(c_double), intent(out) :: arr(size)
  real(c_double), intent(in) :: val
  integer :: i

  do i = 1, size
    arr(i) = val
  end do
end subroutine initArray


! FIXME: should rename this power and make the types more explicitly typed
!double precision function myfunc(formal1, formal2) bind(C,name='myfunc')
!  double precision formal1
!  integer formal2
!  double precision power
!  integer i
!
!  !real(8), bind(C, name='power') :: power = 1.0
!
!  power = 1.0d0
!  do i = 0, formal2 - 1
!    power = power * formal1
!  end do
!
!  myfunc = power
!end function myfunc

end module TestProcs