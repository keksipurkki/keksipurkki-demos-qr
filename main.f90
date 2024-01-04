program main
  use display
  use eigenvalues
  use iso_fortran_env
  implicit none

  real(real64), allocatable :: X(:,:)
  real(real64), allocatable :: Q(:,:), L(:)

  X = data_matrix()
  call disp('X = ', X)

  call disp()
  call eig(X, Q, L)
  call disp()
  call disp('Λ =', L, digmax=15)
  call disp()

  contains

    function data_matrix()
      real(real64) :: data_matrix(25, 25)
      integer :: i
      data_matrix = -2.0d0

      do i = i, size(data_matrix, 1)
        data_matrix(i,i) = i
      enddo

    end function

end program
