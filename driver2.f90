! This Driver will demonstrate initializing a data structure within
! a function using variables passed to it, then returning the 
! data structure.

MODULE init_mod
  IMPLICIT NONE

  TYPE marbl_driver
    REAL, ALLOCATABLE :: temperature(:)
    REAL, ALLOCATABLE :: salinity(:)
    REAL, ALLOCATABLE :: oxygen(:)
    REAL :: uwind
    REAL :: vwind
  END TYPE marbl_driver
CONTAINS


  FUNCTION init_driver(temp,salinity,oxygen,uwind,vwind) RESULT(new_driver)
    IMPLICIT NONE
    REAL, DIMENSION(:), intent(in) :: temp, salinity, oxygen
    REAL, intent(in) :: uwind,vwind
    type(marbl_driver) :: new_driver
    INTEGER :: i

    IF (SIZE(temp) /= SIZE(salinity) .OR. SIZE(temp) /= SIZE(oxygen)) THEN
      PRINT *, "Error: Array sizes must be the same when initializing driver."
      STOP
    END IF

    ALLOCATE(new_driver%temperature(SIZE(temp)))
    ALLOCATE(new_driver%salinity(SIZE(temp)))
    ALLOCATE(new_driver%oxygen(SIZE(temp)))

    DO i = 1, SIZE(temp) 
      new_driver%temperature(i) = temp(i)
      new_driver%salinity(i) = salinity(i)
      new_driver%oxygen(i) = oxygen(i)
    END DO

    new_driver%uwind = uwind
    new_driver%vwind = vwind

  END FUNCTION init_driver
END MODULE


PROGRAM main
  use init_mod

  IMPLICIT NONE

  REAL, ALLOCATABLE :: a1(:)
  REAL, ALLOCATABLE :: a2(:)
  REAL, ALLOCATABLE :: a3(:)
  type(marbl_driver) :: my_driver

  INTEGER :: n
  PRINT *, "Enter the size of the column:"
  READ *, n

  ALLOCATE(a1(n))
  ALLOCATE(a2(n))
  ALLOCATE(a3(n))

  a1 = 1.0
  a2 = 2.0
  a3 = 3.0

  my_driver = init_driver(a1, a2, a3, 4.0, 5.0)

  PRINT *, "Driver temp values: ", my_driver%temperature
  PRINT *, "Driver oxygen values: ", my_driver%oxygen
  PRINT *, "Driver salinity values: ", my_driver%salinity
  PRINT *, "Driver Wind values: ", my_driver%uwind, my_driver%vwind

  DEALLOCATE(a1)
  DEALLOCATE(a2)
  DEALLOCATE(a3)

  DEALLOCATE(my_driver%temperature)
  DEALLOCATE(my_driver%oxygen)
  DEALLOCATE(my_driver%salinity)
END PROGRAM main