
MODULE procedures
  IMPLICIT NONE
CONTAINS
  FUNCTION pass_two_arrays(arr1, arr2) RESULT(sum)
    IMPLICIT NONE
    INTEGER, DIMENSION(:) :: arr1, arr2
    INTEGER, ALLOCATABLE :: sum(:)
    INTEGER :: i,n
    IF (SIZE(arr1) /= SIZE(arr2)) THEN
      PRINT *, "ERROR: array size mismatch"
      STOP
    END IF
    n = SIZE(arr1,1)
    ALLOCATE(sum(n))
    DO i = 1, SIZE(arr1)
      sum(i) = arr1(i) + arr2(i)
    END DO

  END FUNCTION pass_two_arrays
END MODULE

PROGRAM main
  USE procedures
  
  IMPLICIT NONE
  
  ! Define a custom data structure (type)
  TYPE :: OceanColumn
      INTEGER, ALLOCATABLE :: temperature(:)
      INTEGER, ALLOCATABLE :: salinity(:)
      INTEGER, ALLOCATABLE :: oxygen(:)
  END TYPE OceanColumn

  INTEGER :: n

  ! Declare a variable of type OceanColumn
  TYPE(OceanColumn) :: column1

  PRINT *, "Enter the size of the column:"
  READ *, n

  ALLOCATE(column1%temperature(n))
  ALLOCATE(column1%salinity(n))

  column1%temperature = 10
  column1%salinity = 20
  column1%oxygen = pass_two_arrays(column1%temperature, column1%salinity)
  PRINT *, "temp: ", column1%temperature
  PRINT *, "salinity: ", column1%salinity
  PRINT *, "oxygen: ", column1%oxygen

  DEALLOCATE(column1%temperature)
END PROGRAM main
