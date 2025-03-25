program test_netcdf
  use netcdf
  implicit none

  integer :: ncid, status
  character(len=*), parameter :: filename = "test.nc"

  ! Create a new NetCDF file
  status = nf90_create(filename, NF90_CLOBBER, ncid)
  if (status /= NF90_NOERR) then
    print *, "Error creating NetCDF file: ", trim(nf90_strerror(status))
    stop
  end if

  ! Close the NetCDF file
  status = nf90_close(ncid)
  if (status /= NF90_NOERR) then
    print *, "Error closing NetCDF file: ", trim(nf90_strerror(status))
    stop
  end if

  print *, "NetCDF test successful, file created:", filename
end program test_netcdf