program test
  use iso_fortran_env
  use display
  use utils
  implicit none

  real(real64) :: a(3)
  integer :: i
  a = 1.0d0
  i = findloc(a, -1.0d0, dim=1)
  print *, i

end program
