program test
  use iso_fortran_env
  use display
  implicit none
  real :: a(5,5), block(2,2), v(5)
  integer :: i
  v = [(1.0d0*i, i = 1, 5)]
  !a = eye(5)
  !block = reshape([[5, 0],[0, 5]], [2,2])
  !a(4:,4:) = block

  call disp(reshape([v(2:), v], [4, 2]))
  call disp()
  call disp(reshape([v(2:), v], [2, 4], order=[2, 1]))

  contains

  pure function eye(n)
    integer, intent(in) :: n
    integer :: i
    real(real64) :: eye(n,n)
    eye = 0.0d0
    forall (i=1:n) eye(i,i) = 1.0d0
  end function

end program
