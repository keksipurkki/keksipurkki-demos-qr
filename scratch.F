program test
  use iso_fortran_env
  use display
  implicit none
  real :: a(5,5), block(2,2), v(5)
  real(real64) :: ulp, tol
  integer :: i
  v = [(1.0d0*i, i = 1, 5)]

  call disp(radix(0.0_real64))

  ulp = radix(1.0_real64)*epsilon(1.0_real64)
  tol = tiny(1.0_real64) * (200 / ulp)

  call disp(ulp)
  call disp(200/ulp)

  contains

  pure function eye(n)
    integer, intent(in) :: n
    integer :: i
    real(real64) :: eye(n,n)
    eye = 0.0d0
    forall (i=1:n) eye(i,i) = 1.0d0
  end function

end program
