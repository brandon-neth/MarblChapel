module driver6
  use iso_c_binding
  implicit none

  contains

subroutine initArrays(a1, a2, size, val) bind(C,name='initArrays')
  use iso_c_binding
  implicit none
  integer(c_int), intent(in) :: size
  real(c_double), intent(inout) :: a1(size), a2(size)
  real(c_double), intent(in) :: val
  integer :: i

  do i = 1, size
    if (a1(i) == 1.0) then
      a1(i) = val
    endif
    a2(i) = val * 2.0
  end do

end subroutine initArrays


end module