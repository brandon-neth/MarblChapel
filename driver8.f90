module driver8
  use iso_c_binding
  implicit none 

    type :: kinematics
      real(c_double), allocatable :: velocities(:)
      real(c_double), allocatable :: accelerations(:)
      real(c_double) :: dt
    contains
      procedure :: update_velocities
    end type kinematics

contains

  subroutine update_velocities(self)
    implicit none
    class(kinematics) , intent(inout) :: self
    integer :: i
    do i = 1, size(self%velocities)
      self%velocities(i) = self%velocities(i) + self%dt * self%accelerations(i)
    end do
  end subroutine update_velocities




  subroutine applyCurrent(length, velocities, accelerations, dt) bind(C,name='applyCurrent')
    use iso_c_binding
    implicit none
    integer(c_int), intent(in) :: length
    real(c_double), intent(inout) :: velocities(length), accelerations(length)
    real(c_double), intent(in) :: dt
    integer :: i
    type(kinematics) :: kin

    allocate(kin%velocities(length))
    allocate(kin%accelerations(length))

    do i = 1, length
      kin%velocities(i) = velocities(i)
      kin%accelerations(i) = accelerations(i)
    end do

    kin%dt = dt

    call kin%update_velocities()

    do i = 1, length
      velocities(i) = kin%velocities(i)
      accelerations(i) = kin%accelerations(i)
    end do

  end subroutine applyCurrent
end module driver8