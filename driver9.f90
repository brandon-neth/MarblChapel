module driver9
  use iso_c_binding
  implicit none 

    type :: kinematics
      real(c_double), allocatable :: p(:)
      real(c_double), allocatable :: v(:)
      real(c_double), allocatable :: a(:)
      real(c_double) :: dt
    contains
      procedure :: update_kinematics
    end type kinematics

contains

  subroutine update_kinematics(self)
    implicit none
    class(kinematics) , intent(inout) :: self
    integer :: i
    do i = 1, size(self%v)
      self%p(i) = self%p(i) + self%dt * self%v(i)
      self%v(i) = self%v(i) + self%dt * self%a(i)
    end do
  end subroutine update_kinematics

  subroutine updateKinematics(numColumns, columnLength, data) bind(C,name='updateKinematics')
    use iso_c_binding
    implicit none
    integer(c_int), intent(in) :: numColumns
    integer(c_int), intent(in) :: columnLength
    real(c_double), intent(inout) :: data(columnLength, numColumns)
    integer :: i, j
    type(kinematics) :: k

    allocate(k%p(columnLength))
    allocate(k%v(columnLength))
    allocate(k%a(columnLength))

    do i = 1,columnLength
      k%p(i) = data(i,1)
      k%v(i) = data(i,2)
      k%a(i) = data(i,3)
    end do

    k%dt = 1.0

    call k%update_kinematics()

    do i = 1,columnLength
       data(i,1) = k%p(i)
       data(i,2) = k%v(i)
       data(i,3) = k%a(i)
    end do

    deallocate(k%p)
    deallocate(k%v)
    deallocate(k%a)
  end subroutine updateKinematics
end module driver9