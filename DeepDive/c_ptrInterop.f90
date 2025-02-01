module c_ptrInterop
  use iso_c_binding

type, bind(C) :: wrapperType
  type(c_ptr) :: ptr
end type wrapperType

type:: myType
  integer :: length
  real, pointer :: temperature(:)
end type myType

contains

  subroutine initWrapper(wrapper, length) bind(C, name="initWrapper")
    type(wrapperType), intent(inout) :: wrapper
    integer, intent(in) :: length
    type(myType), pointer :: obj

    ! Allocate the Fortran object
    allocate(obj)
    ! Populate its fields
    obj%length = length
    allocate(obj%temperature(obj%length))
    obj%temperature = 0.0
    ! Fill the wrapper object with a pointer to the Fortran object
    wrapper%ptr = c_loc(obj)
  end subroutine initWrapper

  subroutine setTemperatureArray(wrapper, length, array) bind(C, name='setTemperatureArray')
    type(wrapperType), intent(inout) :: wrapper
    integer(c_int), intent(in) :: length
    real(c_double), intent(in), dimension(length) :: array
    type(myType), pointer :: obj

    ! Convert the C pointer to a Fortran pointer
    call c_f_pointer(wrapper%ptr, obj)

    ! Copy the array to the Fortran object
    obj%temperature = array
  end subroutine setTemperatureArray

  subroutine getTemperatureArray(wrapper, length, array) bind(C, name='getTemperatureArray')
    type(wrapperType), intent(in) :: wrapper
    integer, intent(in) :: length
    real(c_double), intent(out), dimension(length) :: array
    type(myType), pointer :: obj

    ! Convert the C pointer to a Fortran pointer
    call c_f_pointer(wrapper%ptr, obj)
    ! Copy the array from the Fortran object
    array = obj%temperature
  end subroutine getTemperatureArray

  subroutine convertFtoC(wrapperF, wrapperC) bind(C, name="convertFtoC")
    
    type(wrapperType), intent(in) :: wrapperF
    type(wrapperType), intent(inout) :: wrapperC
    
    type(myType), pointer :: myTypeF
    type(myType), pointer :: myTypeC
    integer :: i
    ! Convert the C pointers to Fortran pointers
    call c_f_pointer(wrapperF%ptr, myTypeF)
    call c_f_pointer(wrapperC%ptr, myTypeC)

    ! Make the conversion
    myTypeC%length = myTypeF%length
    do i = 1, myTypeC%length
      myTypeC%temperature(i) = (myTypeF%temperature(i) - 32.0) / 1.8
    end do
  end subroutine convertFtoC

  subroutine deinitWrapper(wrapper) bind(C, name="deinitWrapper")
    type(wrapperType), intent(inout) :: wrapper
    type(myType), pointer :: obj

    ! Convert the C pointer to a Fortran pointer
    call c_f_pointer(wrapper%ptr, obj)
    ! Deallocate the Fortran object
    deallocate(obj%temperature)
    deallocate(obj)
  end subroutine deinitWrapper
end module c_ptrInterop