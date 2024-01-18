program test
  use iso_fortran_env
  use display
  use benchmarks, only: rotation_matrix
  use utils
  implicit none

  real(real64), target :: a(3,3), diag(3,3)
  real(real64), pointer :: L(:)

  call disp_set(orient='row')

  a = rotation_matrix(3)
  call disp('A = ', a)

  diag = 0.0d0
  L => diagonal(diag)
  L = [-1, 2, 4]

  call disp(diag)

  call disp(matmul(diag, a))

  contains

end program
