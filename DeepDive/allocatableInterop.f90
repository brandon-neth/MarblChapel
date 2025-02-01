module allocatableInterop
  use iso_c_binding

type, bind(C) :: myType
  integer :: length
  real, allocatable :: temperature(:)
end type myType

contains
  subroutine convertFtoC(myTypeF, myTypeC) bind(C, name="convertFtoC")
    type(myType), intent(in) :: myTypeF
    type(myType), intent(out) :: myTypeC

    myTypeC%length = myTypeF%length
    allocate(myTypeC%temperature(myTypeC%length))
    myTypeC%temperature = (myTypeF%temperature - 32.0) / 1.8
  end subroutine convertFtoC

  subroutine deinit_myType(obj) bind(C, name="deinit_myType")
    type(myType), intent(inout) :: obj

    deallocate(obj%temperature)
  end subroutine deinit_myType

end module allocatableInterop