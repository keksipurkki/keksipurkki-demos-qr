program test
  use iso_fortran_env
  use display
  use utils
  implicit none

  integer :: i
  real(real64), target :: a(3,3)
  real(real64), pointer :: diag(:)

  call disp_set(orient='row')

  a = reshape([( i, i = 1, size(a) )], shape(a))
  !call disp(a)
  !call disp()

  !call disp(diagonal(a, -1))
  !call disp(diagonal(a))
  !call disp(diagonal(a, 1))

  diag => diagonal(a)

  call disp(reshape([diag(2:),-diag(1:)], [2, 2], order=[2,1]))

  contains

end program
