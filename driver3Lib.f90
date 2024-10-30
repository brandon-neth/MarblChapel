module TestProcs
  use iso_c_binding
  implicit none

  contains

  FUNCTION init_driver(temp,salinity,oxygen,length,uwind,vwind) RESULT(i) bind(C,name='init_driver')
    use iso_c_binding, only: c_int, c_double
    
    IMPLICIT NONE
    
    TYPE marbl_driver
      REAL, ALLOCATABLE :: temperature(:)
      REAL, ALLOCATABLE :: salinity(:)
      REAL, ALLOCATABLE :: oxygen(:)
      REAL :: uwind
      REAL :: vwind
    END TYPE marbl_driver

    INTEGER(c_int), intent(in) :: length
    REAL(c_double), intent(in) :: temp(length)
    REAL(c_double), intent(in) :: salinity(length)
    REAL(c_double), intent(in) :: oxygen(length)
    REAL(c_double), intent(in) :: uwind, vwind
    type(marbl_driver) :: new_driver
    INTEGER :: i
    PRINT *, "Checking sizes..."
    IF (SIZE(temp) /= SIZE(salinity) .OR. SIZE(temp) /= SIZE(oxygen)) THEN
      PRINT *, "Error: Array sizes must be the same when initializing driver.", SIZE(temp), SIZE(salinity), SIZE(oxygen)
      STOP
    END IF

    PRINT *, "Allocating..."
    ALLOCATE(new_driver%temperature(length))
    ALLOCATE(new_driver%salinity(length))
    ALLOCATE(new_driver%oxygen(length))

    PRINT *, "Initializing..."
    DO i = 1, length 
      new_driver%temperature(i) = temp(i)
      new_driver%salinity(i) = salinity(i)
      new_driver%oxygen(i) = oxygen(i)
    END DO

    new_driver%uwind = uwind
    new_driver%vwind = vwind

    PRINT *, "Driver Initialized. Outputting Values"
    PRINT *, "temperature: ", new_driver%temperature
    PRINT *, "oxygen: ", new_driver%oxygen
    PRINT *, "salinity: ", new_driver%salinity
    PRINT *, "uwind: ", new_driver%uwind
    PRINT *, "vwind: ", new_driver%vwind
  END FUNCTION init_driver

end module TestProcs