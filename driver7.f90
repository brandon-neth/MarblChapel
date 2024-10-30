module driver7
  use iso_c_binding
  implicit none 
contains

  subroutine applyCurrent(length, velocities, accelerations, dt) bind(C,name='applyCurrent')
    use iso_c_binding
    implicit none
    integer(c_int), intent(in) :: length
    real(c_double), intent(inout) :: velocities(length), accelerations(length)
    real(c_double), intent(in) :: dt
    integer :: i

    do i = 1, length
      velocities(i) = velocities(i) + dt * accelerations(i)
    end do

    


  end subroutine applyCurrent
end module driver7