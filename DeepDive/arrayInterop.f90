module arrayInterop
  use iso_c_binding

type, bind(C) :: myType
  integer(c_int) :: year
  real(c_double) :: temperature(20)
end type myType

contains
  subroutine convertFtoC(myTypeF, myTypeC) bind(C, name="convertFtoC")
    type(myType), intent(in) :: myTypeF
    type(myType), intent(out) :: myTypeC

    myTypeC%year = myTypeF%year
    myTypeC%temperature = (myTypeF%temperature - 32.0) / 1.8
  end subroutine convertFtoC


end module arrayInterop