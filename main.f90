program main
  use display
  use eigenvalues
  use iso_fortran_env
  implicit none

  real(real64), allocatable :: x(:,:)
  real(real64), allocatable :: q(:,:), lambda(:)

  x = data_matrix()
  call disp('X = ', x)

  call disp()
  call eig(x, q, lambda)

  !call disp()
  !call disp('Q = ', q)
  call disp()
  call disp('Λ = ', lambda, orient='row')
  call disp()

  contains

    function data_matrix()
      real(real64) :: data_matrix(11, 11)
      integer :: i
      data_matrix = -2.0d0

      do i = i, size(data_matrix, 1)
        data_matrix(i,i) = i
      enddo

    end function

end program
